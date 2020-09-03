import streams
import tables
import strutils
import parseutils
import options

const header_marker = "tiny"
const indent = '\t'
const space_string = '\t'
const escaped_names_property = "escaped-names"
const to_escape = "\\\n\r\0\t"
const escaped = "\\nr0t"

type
    TinyHeader* = object
        majorVersion: int32
        minorVersion: int32
        namespaces: seq[string]
        mapper: TableRef[string, int32]
        properties: OrderedTableRef[string, Option[string]]
    PartGetter* = object
        offset: int32
        parts: seq[string]
        escapedStrings: bool
    ITinyVisitor* = tuple[
        start: proc(metadata: TinyHeader),
        pushClass: proc(name: PartGetter),
        pushField: proc(name: PartGetter, descriptor: string),
        pushMethod: proc(name: PartGetter, descriptor: string),
        pushParameter: proc(name: PartGetter, localVariableIndex: int32),
        pushLocalVariable: proc(name: PartGetter, localVariableIndex: int32, localVariableStartOffset: int32, localVariableTableIndex: int32),
        pushComment: proc(comment: string),
        pop: proc(count: int32)
    ]

func getMajorVerion*(self: TinyHeader): int32 =
    return self.majorVersion

func getMinorVersion*(self: TinyHeader): int32 =
    return self.minorVersion

func getNamespaces*(self: TinyHeader): seq[string] =
    return self.namespaces

func getProperties*(self: TinyHeader): OrderedTableRef[string, Option[string]] =
    return self.properties

func index*(self: TinyHeader, namespace: string): int32 =
    return self.mapper.getOrDefault(namespace, -1)

func unescape(str: string): string =
    var pos: int = str.find("\\")
    if (pos < 0):
        return str
    result = ""
    var start: int32 = 0
    while true:
        result.add(str.substr(start, start + pos))
        pos += 1
        if pos >= str.len:
            debugEcho "incomplete escape sequence at the end"
        else:
            let typeint = escaped.find(str[pos])
            if typeint < 0:
                debugEcho "invalid escape character: \\" & str[pos]
            else:
                result.add(to_escape[typeint])
            start += 1
        pos = str.find("\\", start=start)
        if pos < 0:
            break

func unescapeOpt(raw: string, escapedStrings: bool): string =
    if escapedStrings:
        return unescape(raw)
    else:
        return raw

func get*(self: PartGetter, namespace: int32): string =
    var index = self.offset + namespace
    while self.parts[index] == "":
        index -= 1
    return unescapeOpt(self.parts[index], self.escapedStrings)

func getRaw*(self: PartGetter, namespace: int32): string =
    return unescapeOpt(self.parts[self.offset + namespace], self.escapedStrings)
    
func copyOfRange*[T](original_seq: seq[T], from_index: int, to_index: int): seq[T] =
    for i in from_index..(to_index-1):
        result.add(original_seq[i])

func getRawNames*(self: PartGetter): seq[string] =
    if self.escapedStrings != true:
        return copyOfRange(self.parts, self.offset, self.parts.len)
    result = newSeq[string](self.parts.len - self.offset)
    for i in 0..(result.len - 1):
        result[i] = unescape(self.parts[i + self.offset])

func getAllNames*(self: PartGetter): seq[string] =
    result = self.getRawNames()
    for i in 1..(result.len - 1):
        if result[i] == "":
            result[i] = unescape(self.parts[i + self.offset])

func makeHeader(major: int32, minor: int32, parts: seq[string], props: OrderedTableRef[string, Option[string]]): TinyHeader =
    var list = newSeq[string](0)
    var map = newTable[string, int32]()
    for i in 3..(parts.len - 1):
        list.add(parts[i])
        map.add(parts[i], int32(i) - 3)
    return TinyHeader(majorVersion: major,
                      minorVersion: minor,
                      namespaces: list,
                      mapper: map,
                      properties: props)

func countIndent(st: string): int32 =
    let len = st.len
    result = 0
    while result < len and st[result] == indent:
        result += 1

proc readMetadata*(reader: Stream): TinyHeader =
    let firstline = reader.readLine()
    if firstline == "":
        debugEcho "Empty reader!"
    let parts = firstline.split(space_string)
    if parts.len < 5 or parts[0] != header_marker:
        debugEcho "Unsupported format!"
    var bigMajorVersion: BiggestInt
    if parseBiggestInt(parts[1], bigMajorVersion) <= 0:
        debugEcho "Invalid major version!"
    let majorVersion = int32(bigMajorVersion)
    var bigMinorVersion: BiggestInt
    if parseBiggestInt(parts[2], bigMinorVersion) <= 0:
        debugEcho "Invalid minor version!"
    let minorVersion = int32(bigMinorVersion)
    var properties = newOrderedTable[string, Option[string]]()
    var line: TaintedString
    var mark = reader.getPosition()
    while readLine(reader, line):
        case countIndent(line):
        of 0:
            reader.setPosition(mark)
            return makeHeader(majorVersion, minorVersion, parts, properties)
        of 1:
            let elements = line.split(space_string)
            if elements.len == 2:
                properties.add(elements[1], none(string))
            else:
                properties.add(elements[1], some(elements[2]))
        else:
            debugEcho "Invalid indent in header! Encountered \"" & line & "\"!"
        mark = reader.getPosition()
    return makeHeader(majorVersion, minorVersion, parts, properties)

type
    TinyState = enum
        class, field, methodd, parameter, local_variable, comment

func getTinyState(indent: int32, identifier: string): TinyState =
    case identifier:
    of "c":
        if indent == 0:
            return class
        else:
            return comment
    of "m":
        return methodd
    of "f":
        return field
    of "p":
        return parameter
    of "V":
        return local_variable
    else:
        debugEcho "Invalid identifier \"" & identifier & "\"!"

func getActualParts(self: TinyState): int32 =
    case self:
    of class:
        return 1
    of field, methodd, parameter, comment:
        return 2
    of local_variable:
        return 4

func checkPartCount(self: TinyState, indent: int32, partCount: int32, namespaceCount: int32): bool =
    case self:
    of class, field, methodd, parameter, local_variable:
        return partCount - indent == namespaceCount + self.getActualParts()
    of comment:
        return partCount - indent == 2

func checkStack(self: TinyState, stack: openArray[TinyState], currentIndent: int32): bool =
    case self:
    of class:
        return currentIndent == 0
    of field, methodd:
        return currentIndent == 1 and stack[currentIndent - 1] == class
    of parameter, local_variable:
        return currentIndent == 2 and stack[currentIndent - 1] == methodd
    of comment:
        if currentIndent == 0:
            return false
        else:
            case stack[currentIndent - 1]:
            of class, methodd, field, parameter, local_variable:
                return true
            else:
                return false

func makeGetter(self: TinyState, parts: seq[string], indent: int32, escapedStrings: bool): PartGetter =
    return PartGetter(offset: indent + self.getActualParts(),
                      parts: parts,
                      escapedStrings: escapedStrings)

proc visit(self: TinyState, visitor: ITinyVisitor, parts: seq[string], indent: int32, escapedStrings: bool) =
    case self:
    of class:
        visitor.pushClass(self.makeGetter(parts, indent, escapedStrings))
    of field:
        visitor.pushField(self.makeGetter(parts, indent, escapedStrings), unescapeOpt(parts[indent + 1], escapedStrings))
    of methodd:
        visitor.pushMethod(self.makeGetter(parts, indent, escapedStrings), unescapeOpt(parts[indent + 1], escapedStrings))
    of parameter:
        var big: BiggestInt
        assert parseBiggestInt(parts[indent + 1], big) > 0
        visitor.pushParameter(self.makeGetter(parts, indent, escapedStrings), int32(big))
    of local_variable:
        var big1: BiggestInt
        assert parseBiggestInt(parts[indent + 1], big1) > 0
        var big2: BiggestInt
        assert parseBiggestInt(parts[indent + 1], big2) > 0
        var big3: BiggestInt
        assert parseBiggestInt(parts[indent + 1], big3) > 0
        visitor.pushLocalVariable(self.makeGetter(parts, indent, escapedStrings), int32(big1), int32(big2), int32(big3))
    of comment:
        visitor.pushComment(unescape(parts[indent + 1]))

proc visit*(reader: Stream, visitor: ITinyVisitor) =
    let meta = readMetadata(reader)
    let namespaceCount = int32(meta.getNamespaces().len)
    let escapedNames = meta.getProperties().hasKey(escaped_names_property)
    visitor.start(meta)
    var lastIndent: int32 = -1;
    var stack = [class, class, class, class] # max depth 4
    var line: TaintedString
    while readLine(reader, line):
        let currentIndent = countIndent(line)
        if currentIndent > lastIndent + 1:
            debugEcho "Broken indent! Maximum " & $(lastIndent + 1) & ", actual " & $currentIndent
        if currentIndent <= lastIndent:
            visitor.pop(lastIndent - currentIndent + 1)
        lastIndent = currentIndent
        let parts = line.split(space_string, -1)
        let currentState = getTinyState(currentIndent, parts[currentIndent])
        if not currentState.checkPartCount(currentIndent, int32(parts.len), namespaceCount):
            debugEcho "Wrong number of parts for definition of a " & $currentState & "!"
        if not currentState.checkStack(stack, currentIndent):
            debugEcho "Invalid stack " & $stack & " for a " & $currentState & " at position " & $currentIndent & "!"
        stack[currentIndent] = currentState
        currentState.visit(visitor, parts, currentIndent, escapedNames)
    if lastIndent > -1:
        visitor.pop(lastIndent + 1)

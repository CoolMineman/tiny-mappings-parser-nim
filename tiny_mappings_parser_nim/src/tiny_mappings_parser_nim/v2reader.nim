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
    var ret = 0
    while ret < len and st[ret] == indent:
        ret += 1

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

proc visit*(reader: Stream, visitor: ITinyVisitor) =
    discard



# test

var strm = newFileStream("mappings.tiny", fmRead)
echo readMetadata(strm)
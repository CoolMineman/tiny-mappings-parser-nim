# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import streams
import tables
import strutils
import tiny_mappings_parser_nim
import tiny_mappings_parser_nim/v2reader
test "TinyV2VisitTest":
  var strm = newFileStream("mappings.tiny", fmRead)
  type
    Visitor = object
  var level: int32
  
  proc indent(self: Visitor): string =
    return "\t".repeat(level)
  proc printNames(self: Visitor, name: PartGetter) =
    echo self.indent() & $name.getRawNames()
    echo self.indent() & $name.getAllNames()
  proc start(self: Visitor, metadata: TinyHeader) =
    echo metadata.getNamespaces()
    echo metadata.getProperties()
  proc pushClass(self: Visitor, name: PartGetter) =
    level += 1
    echo self.indent() & name.get(0)
    self.printNames(name)
  proc pushField(self: Visitor, name: PartGetter, descriptor: string) =
    level += 1
    echo self.indent() & name.get(0)
    self.printNames(name)
  proc pushMethod(self: Visitor, name: PartGetter, descriptor: string) =
    level += 1
    echo self.indent() & name.get(0)
    self.printNames(name)
  proc pushParameter(self: Visitor, name: PartGetter, localVariableIndex: int32) =
    level += 1
    stdout.write self.indent()
    self.printNames(name)
  proc pushLocalVariable(self: Visitor, name: PartGetter, localVariableIndex: int32, localVariableStartOffset: int32, localVariableTableIndex: int32) =
    level += 1
    stdout.write self.indent()
    self.printNames(name)
  proc pushComment(self: Visitor, comment: string) =
    level += 1
    echo self.indent() & comment
  proc pop(self: Visitor, count: int32) =
    level -= count
    echo "level -= " & $count
  proc toTinyVisitor(self: Visitor): ITinyVisitor =
    return (
      start: proc(metadata: TinyHeader) = self.start(metadata),
      pushClass: proc(name: PartGetter) = self.pushClass(name),
      pushField: proc(name: PartGetter, descriptor: string) = self.pushField(name, descriptor),
      pushMethod: proc(name: PartGetter, descriptor: string) = self.pushMethod(name, descriptor),
      pushParameter: proc(name: PartGetter, localVariableIndex: int32) = self.pushParameter(name, localVariableIndex),
      pushLocalVariable: proc(name: PartGetter, localVariableIndex: int32, localVariableStartOffset: int32, localVariableTableIndex: int32) = self.pushLocalVariable(name, localVariableIndex, localVariableStartOffset, localVariableTableIndex),
      pushComment: proc(comment: string) = self.pushComment(comment),
      pop: proc(count: int32) = self.pop(count)
    )

  var visitor = Visitor()
  visit(strm, visitor.toTinyVisitor())
  check add(5, 5) == 10

package compiler

import scala.collection.immutable.Range

sealed trait ASTNode
case class Location(line:Int, pos:Int) // Records posiion in source file

// Main program and declaration nodes
case class Program(name:String, block:ASTNode) extends ASTNode
case class Block(
  constantDefinitions:List[ASTNode],
  typeDefinitions:List[ASTNode],
  variableDeclarations:List[ASTNode],
  statementPart:ASTNode
) extends ASTNode
case class ConstantDefinition(name:String, value:ASTNode) extends ASTNode
case class TypeDefinition(name:String, typ:ASTNode) extends ASTNode
case class VariableDeclaration(names:List[String], typ:ASTNode) extends ASTNode

// Values
case class Identifier(name:String, loc:Location) extends ASTNode
case class Literal(value:Any, typ:AsaType, loc:Location) extends ASTNode
case class ArrayReference(aname:String,index:ASTNode,loc:Location) extends ASTNode
case class VariableReference(aname:String,loc:Location) extends ASTNode
case class SetLiteral(elements:List[ASTNode]) extends ASTNode

// Expression nodes
class Expression extends ASTNode
case class Binop(op:String, t1:ASTNode, t2:ASTNode) extends ASTNode
case class Unop(op:String, e:ASTNode) extends ASTNode

// Data type nodes
case class NamedType(name:String) extends ASTNode
case class RangeType(lowerBound:ASTNode, upperBound:ASTNode) extends ASTNode
case class ArrayType(range:ASTNode, elementType:ASTNode) extends ASTNode
case class SetType(range:ASTNode) extends ASTNode
case class EnumType(values:List[String]) extends ASTNode
case class FileType(baseType:ASTNode) extends ASTNode
case class PointerType(baseType:ASTNode) extends ASTNode
case class FieldDef(idlist:List[String], typ:ASTNode) extends ASTNode
case class FieldList(flist:List[FieldDef]) extends ASTNode
case class RecordType(fields:FieldList) extends ASTNode
case class IndexedVariable(name:String, indices:List[ASTNode]) extends ASTNode
case class FieldDesignator(record:String, field:String) extends ASTNode
case class ReferencedVariable(name:ASTNode) extends ASTNode
case class FunctionCall(name:ASTNode, acp:List[ASTNode]) extends ASTNode
case class FileBuffer(name:ASTNode) extends ASTNode

// Statement nodes
case class AssignmentStatement(lhs:ASTNode, rhs:ASTNode) extends ASTNode
case class CompoundStatement(list:List[ASTNode]) extends ASTNode
case class WhileStatement(cond:ASTNode, body:ASTNode) extends ASTNode
case class RepeatStatement(cond:ASTNode, body:List[ASTNode]) extends ASTNode
case class IfStatement(cond:ASTNode, thenstat:ASTNode, elsestat:Option[ASTNode]) extends ASTNode
case class ForStatement(indexvar:Identifier, startexp:ASTNode, endexp:ASTNode, ascending:Boolean, body:ASTNode) extends ASTNode
case class PrintfStatement(fmt:ASTNode, elist:List[ASTNode]) extends ASTNode
case class CaseStatement(expression:ASTNode, limbs:List[ASTNode]) extends ASTNode
case class CaseLimb(labels: List[Int], stat: ASTNode) extends ASTNode

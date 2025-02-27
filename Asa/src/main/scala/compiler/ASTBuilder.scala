package compiler

//import scala.collection.JavaConverters._
import scala.jdk.CollectionConverters._
import org.antlr.v4.runtime.tree.TerminalNode
import scala.util.matching._
import antlr._

class ASTBuilder extends AsaBaseVisitor[ASTNode] {

  val DEBUG = false
  val vocabulary = new AsaParser(null).getVocabulary()


  def escapeString(str:String) = {
    val nl = "\n"
    val tab = "\t"
    val escape = """\\([nt])""".r
    val escapedString = escape.replaceAllIn(str, m => { 
      m.group(1) match {
        case "n" => nl
        case "t" => tab
      }
    })
    escapedString
  }

  def loc(tnode:TerminalNode) = {
    Location(tnode.getSymbol().getLine(), tnode.getSymbol().getCharPositionInLine())
  }

  override def visitProgram(ctx:AsaParser.ProgramContext) = {
    val programName = ctx.IDENT().getText
    val block = visit(ctx.block())
    Program(programName, block)
  }

  override def visitBlock(ctx:AsaParser.BlockContext) = {
    val constDefs = if (ctx.constant_definition() == null) List() else ctx.constant_definition().asScala.toList.map(visit(_))
    val typeDefs  = if (ctx.type_definition() == null) List() else ctx.type_definition().asScala.toList.map(visit(_))
    val varDecl   = if (ctx.variable_declaration() == null) List() else ctx.variable_declaration().asScala.toList.map(visit(_))
    val compoundStatement = visit(ctx.compound_statement())
    Block(constDefs, typeDefs, varDecl, compoundStatement)
  }

  override def visitConstant_definition(ctx:AsaParser.Constant_definitionContext) = {
    val name = ctx.IDENT().getText
    val value = visit(ctx.literal())
    ConstantDefinition(name, value)
  }

  override def visitType_definition(ctx:AsaParser.Type_definitionContext) = {
    val name = ctx.IDENT().getText
    val typ = visit(ctx.atype())
    TypeDefinition(name, typ)
  }

  override def visitVariable_declaration(ctx:AsaParser.Variable_declarationContext) = {
    val identList = ctx.IDENT().asScala.toList.map(_.getText)
    val typ = visit(ctx.atype())
    VariableDeclaration(identList, typ)
  }

  /*
   * Literals
  */
  override def visitDiscreteLiteralAlt(ctx:AsaParser.DiscreteLiteralAltContext) = {
    visit(ctx.discreteLiteral())
  }

  override def visitStringLiteralAlt(ctx:AsaParser.StringLiteralAltContext) = {
    val s = ctx.STRINGLITERAL()
    val st = s.getText drop 1 dropRight 1
    Literal(escapeString(st), STRING_TYPE, loc(s))
  }

  override def visitSetLiteral(ctx:AsaParser.SetLiteralContext) = {
    val elements = ctx.discreteLiteral().asScala.toList.map(visit(_))
    SetLiteral(elements)
  }

  override def visitIntegerLiteralAlt(ctx:AsaParser.IntegerLiteralAltContext) = {
    visit(ctx.integerLiteral())
  }

  override def visitDecimalIntegerLiteralAlt(ctx:AsaParser.DecimalIntegerLiteralAltContext) = {
    val decint = ctx.DECIMALINTEGERLITERAL()
    val intValue = Integer.decode(decint.getText)
    Literal(intValue, INTEGER_TYPE, loc(decint))
  }

  override def visitHexadecimalIntegerLiteralAlt(ctx:AsaParser.HexadecimalIntegerLiteralAltContext) = {
    val hexint = ctx.HEXADECIMALINTEGERLITERAL()
    val intValue = Integer.decode(hexint.getText)
    Literal(intValue, INTEGER_TYPE, loc(hexint))
  }

  override def visitOctalIntegerLiteralAlt(ctx:AsaParser.OctalIntegerLiteralAltContext) = {
    /*

      Replace this comment with the code to visit OctalIntegerLiteralAlt.
    */
    val octint = ctx.OCTALINTEGERLITERAL()
    val intValue = Integer.decode(octint.getText)
    Literal(intValue, INTEGER_TYPE, loc(octint))
  }

  override def visitBooleanLiteralAlt(ctx:AsaParser.BooleanLiteralAltContext) = {
    val boolvalue = ctx.booleanLiteral()
    boolvalue.getText match {
      case "true" => Literal(true, BOOLEAN_TYPE, Location(0,0))
      case "false" => Literal(false, BOOLEAN_TYPE, Location(0,0))
    }
  }

  override def visitCharLiteralAlt(ctx:AsaParser.CharLiteralAltContext) = {
    val charvalue = ctx.CHARLITERAL()
    val ch = charvalue.getText drop 1 dropRight 1
    val escapedChar = escapeString(ch)(0)
    Literal(escapedChar, CHAR_TYPE, loc(charvalue))
  }

  /*
   * Types
   */
  
  override def visitAtype(ctx:AsaParser.AtypeContext) = {
    visitChildren(ctx)
  }

  override def visitNamedType(ctx:AsaParser.NamedTypeContext) = {
    val typeName = ctx.IDENT().getText
    NamedType(typeName)
  }

  override def visitRangeType(ctx:AsaParser.RangeTypeContext) = {
    val startIndexNode = visit(ctx.discreteLiteral(0))
    val endIndexNode   = visit(ctx.discreteLiteral(1))
    RangeType(startIndexNode, endIndexNode)
  }

  override def visitArrayType(ctx:AsaParser.ArrayTypeContext) = {
    val rangeType = visit(ctx.rangeType())
    val elementType = visit(ctx.atype())
    ArrayType(rangeType, elementType)
  }

  override def visitSetType(ctx:AsaParser.SetTypeContext) = {
    val rangeType = visit(ctx.rangeType)
    SetType(rangeType)
  }

  override def visitEnumType(ctx:AsaParser.EnumTypeContext) = {
    val identList = ctx.IDENT().asScala.toList.map(_.getText)
    EnumType(identList)
  }

  /*
      Statements
  */

  override def visitStatement(ctx:AsaParser.StatementContext) = {
    visitChildren(ctx)
  }

  override def visitAssignment_statement(ctx:AsaParser.Assignment_statementContext) = {
    /*
      Replace this comment with the code to visit an assignment_statement. This is an easy one - just visit the
      two non-terminals in the assignment_statement and then plug the resulting trees into the AssignmentStatement 
      node and return it.
    */
    val lhs = visit(ctx.lhsreference()).asInstanceOf[ASTNode] // Left-hand side
    val rhs = visit(ctx.logicalexpression()).asInstanceOf[ASTNode] // Right-hand side
    AssignmentStatement(lhs, rhs) // Create an ASTNode and return it
  }

  override def visitLhsreference(ctx:AsaParser.LhsreferenceContext) = {
    val ident = ctx.IDENT()
    val name = ident.getText
    val locx = loc(ident)
    if (ctx.simpleexpression() == null) {
      VariableReference(name, locx)
    } else {
      val index = visit(ctx.simpleexpression())
      ArrayReference(name, index, locx)
    }
  }

  override def visitRhsvalue(ctx:AsaParser.RhsvalueContext) = {
    val ident = ctx.IDENT()
    if (DEBUG) println(s"in visitRhsvalue: ident= $ident")
    val name = ident.getText
    if (DEBUG) println(s"in visitRhsvalue: name= $name")
    val locx = loc(ident)
    if (ctx.simpleexpression() == null) {
      VariableReference(name, locx)
    } else {
      val index = visit(ctx.simpleexpression())
      ArrayReference(name, index, locx)
    }
  }

  override def visitCompound_statement(ctx:AsaParser.Compound_statementContext) = {
    val statements = ctx.statement().asScala.toList.map(visit(_))
    CompoundStatement(statements)
  }

  override def visitWhile_statement(ctx:AsaParser.While_statementContext) = {
    val cond = visit(ctx.logicalexpression())
    val body = visit(ctx.statement())
    WhileStatement(cond, body)
  }

  override def visitRepeat_statement(ctx:AsaParser.Repeat_statementContext) = {
    val cond = visit(ctx.logicalexpression())
    val statements = ctx.statement().asScala.toList.map(visit(_))
    RepeatStatement(cond, statements)
  }

  override def visitIf_statement(ctx:AsaParser.If_statementContext) = {
    /* Replace this comment with the code to visit an if_statement. An if_statement always has a 
      logicalexpression for the condition and a statement for the "then" clause. Since the "else" 
      clause is optional, there will only be a second statement if the else is present. (Look at visitFor_statement 
      for inspiration on handling multiple instances of a non-terminal in a rule.) If there is no else clause,
      then return the Option None for the third value in IfStatement node. If there is an else clause, then 
      once you have visited the statement, wrap the resulting ast in Some().
    */
    val condition = visit(ctx.logicalexpression()).asInstanceOf[ASTNode] // Condition
    val thenStatement = visit(ctx.statement(0)).asInstanceOf[ASTNode] // Then Statement
    val elseStatement = if (ctx.statement().size() > 1) {
      Some(visit(ctx.statement(1)).asInstanceOf[ASTNode]) // Optional Else branch
    } else{
      None
    }
    IfStatement(condition, thenStatement, elseStatement) // Return an IfStatement AST node
  }

  override def visitFor_statement(ctx:AsaParser.For_statementContext) = {
    val indexvar = ctx.IDENT().getText
    val startexp = visit(ctx.simpleexpression(0))
    val endexp = visit(ctx.simpleexpression(1))
    val ascending = ctx.dir.getType == AsaParser.TO
    val body = visit(ctx.statement())
    ForStatement(Identifier(indexvar, Location(0,0)), startexp, endexp, ascending, body)
  }

  override def visitPrintf_statement(ctx:AsaParser.Printf_statementContext) = {
    val tmp = ctx.simpleexpression().asScala.toList.map(visit(_))
    val formatString = tmp.head
    val args = tmp.tail
    PrintfStatement(formatString, args)
    /*
      Replace this comment with the rest of the visitPrintf_statement. The given line will return a Scala list
      of ASTNodes, each one representing a simpleexpression. The first simpleexpression is the format string 
      for the printf, the rest of them are the expressions to be printed. Note that the PrintfStatement ASTNode 
      has separate parameters for these two.
    */
  }

  override def visitCase_statement(ctx:AsaParser.Case_statementContext) = {
    if (DEBUG) println(s"begin visitCase_statement")
    val caseExpression = visit(ctx.simpleexpression())
    if (DEBUG) println(s"in visitCase_statement: caseExpression= $caseExpression")
    val limbs = ctx.case_limb().asScala.toList.map(visit(_))
    CaseStatement(caseExpression, limbs)
  }

  override def visitCase_limb(ctx:AsaParser.Case_limbContext) = {
    val caseLabels = ctx.integerLiteral().asScala.toList.map(visit(_)).map((limb) => {
      limb match {
        case Literal(n, typ, loc) => n.asInstanceOf[Int]
        case _ => 0
      }
    })
    val statement = visit(ctx.statement())
    CaseLimb(caseLabels, statement)
  }

  override def visitLogicalexpression(ctx:AsaParser.LogicalexpressionContext) = {
    var re1 = visit(ctx.relationalexpression(0))
    if (ctx.relationalexpression.size() > 1) { 
      val re2 = visit(ctx.relationalexpression(1))
      val op = ctx.op.getType
      val re = Binop(vocabulary.getSymbolicName(op), re1, re2)
      re
    } else
      re1
  }

  override def visitRelationalexpression(ctx:AsaParser.RelationalexpressionContext) = {
    /* Replace this comment with code to visit a relationalexpression. Refer to the grammar and to
       visitLogicalexpression for inspiration.
    */
    var leftExpr = visit(ctx.simpleexpression(0)) // Visit the first simple expression
  
    if (ctx.op != null) { // If there's a relational operator
      val operator = vocabulary.getSymbolicName(ctx.op.getType) // Get operator name
      val rightExpr = visit(ctx.simpleexpression(1)) // Visit the second simple expression
      return Binop(operator, leftExpr, rightExpr) // Construct and return a binary operation
    }
    
    leftExpr // Return the single simple expression if no operator is present
  }

  override def visitSimpleexpression(ctx:AsaParser.SimpleexpressionContext) = {
    var t1 = visit(ctx.term(0))
    if (DEBUG) println(s"in visitSimpleexpression: t1= $t1")
    for (i <- 1 until ctx.term.size()) {
      val t2 = visit(ctx.term(i))
      val t = Binop(vocabulary.getSymbolicName(ctx.op.get(i-1).getType), t1, t2)
      t1 = t
    }
    t1
  }

  override def visitTerm(ctx:AsaParser.TermContext) = {
    var f1 = visit(ctx.factor(0))
    if (DEBUG) println(s"in visitTerm: f1= $f1")
    for (i <- 1 until ctx.factor.size()) {
      val f2 = visit(ctx.factor(i))
      val f = Binop(vocabulary.getSymbolicName(ctx.op.get(i-1).getType), f1, f2)
      f1 = f
    }
    f1
  }

  override def visitFactor(ctx:AsaParser.FactorContext) = { 
    if (ctx.fle != null) {
      if (DEBUG) println(s"in visitFactor: fle= ${ctx.fle}")
      visit(ctx.fle)
    }
    else if (ctx.fl != null) {
      if (DEBUG) println(s"in visitFactor: fl= ${ctx.fl}")
      visit(ctx.fl)
    }
    else if (ctx.fi != null) {
      if (DEBUG) println(s"in visitFactor: fi= ${ctx.fi}")
      visit(ctx.fi)
    }
    else {
      if (DEBUG) println(s"in visitFactor: fn= ${ctx.fn}")
      visit(ctx.fn)
    }
  }

  override def visitNegation(ctx:AsaParser.NegationContext) = {
    val value = visit(ctx.factor())
    Unop(vocabulary.getSymbolicName(AsaParser.NOT), value)
  }

}
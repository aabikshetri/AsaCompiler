package compiler

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import scala.jdk.CollectionConverters._
import java.io._
import antlr._

object AsaCompiler {
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      println("Error: No input file provided. Please provide a filename as a command-line argument.")
      sys.exit(1)
    }
    val input = CharStreams.fromFileName(args(0))
    val lexer = new AsaLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new AsaParser(tokens);

    println("Parsing...")
    val cst = parser.program()
    if (parser.getNumberOfSyntaxErrors() < 1) {
      val astbuilder = new ASTBuilder()
      println("Building AST...")
      val ast = astbuilder.visit(cst)

      val Program(programName, _) =
        ast : @unchecked
        // Extract the program name from the AST for use in the class and file name

      val astw = new PrintWriter(new File(programName + ".astx"))
      astw.println(PrettyPrinter.format(ast.toString))
      astw.close
    }
  }
}
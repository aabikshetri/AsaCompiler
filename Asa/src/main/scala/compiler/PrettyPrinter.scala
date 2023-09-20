package compiler

import scala.collection.mutable.StringBuilder

object PrettyPrinter {

  def format(s:String) = {
    val sb = new StringBuilder
    var indent = 0
    for (ch <- s) {
      if (ch == '(') {
        sb += '\n'
        for (i <- 1 to indent)
          sb += ' '
        sb += ch
        indent += 3
        sb += '\n'
        for (i <- 1 to indent)
          sb += ' '
      } else if (ch == ')') {
        indent -= 3
        sb += '\n'
        for (i <- 1 to indent)
          sb += ' '
        sb += ch
      } else
        sb += ch
    }
    sb.toString
  }

}
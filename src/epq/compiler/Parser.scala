package epq.compiler

import scala.util.parsing.combinator.RegexParsers

class TestParser extends RegexParsers {
  override type Elem = Char
  def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r
  def integer     = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def loop =
    "for"~identifier~"in"~integer~"to"~integer~statement ^^
      { case f~variable~i~lBound~t~uBound~statement => ForLoop(variable, lBound, uBound,statement) }
  def statements = statement*
  def block = "{"~>statements<~"}"  ^^ { l:List[Statement] => Block(l) }
  def statement : Parser[Statement] = loop | block
}

object TestLoopParser extends TestParser with Application {
  parseAll(loop, "for ax in 1 to 42 { for y in 0 to 1 {} }") match {
    case Success(lup,_) => println(lup)
    case x => println(x)
  }
}


abstract trait Statement
case class Block(statements : List[Statement]) extends Statement
case class ForLoop(variable: String, lowerBound:Int, upperBound: Int, statement:Statement) extends Statement
case class DefN(name:String, args:List[(String,String)], block:Block) extends Statement

class EPQParser extends RegexParsers {
  override type Elem = Char
  def identifier  = """[_\\-\\+*/\>\<\p{L}][_\\-\\+*/\>\<\p{L}\p{Nd}]*""".r
  def integer     = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def arg         = identifier ~ ((":"~>identifier)?) ^^ { case i~j => (i,j); case i => (i,"Any")}
  def args = arg*
  //def argswithcommas : Parser[Any] = arg <~ "," ~ argswithcommas | arg
  def defn =
    "defn"~identifier~"["~args~"]"~block ^^
      { case d~name~sqo~arguments~sqc~func => arguments match {case a:List[(String,String)] => DefN(name, a, func) }}
  def statements = statement*
  def block = "{"~>statements<~"}"  ^^ { l:List[Statement] => Block(l) }
  def statement : Parser[Statement] = defn | block
  def line : Parser[Object] = statement ~ line | statement
}

object TestEPQParser extends EPQParser with Application {
  parseAll(line,
    """defn fib [n:int] { }
      defn fab [a b:String] {
        defn feb [] {{}}
        }
    """.stripMargin) match {
    case Success(lup,_) => println(lup)
    case x => println(x)
  }
}
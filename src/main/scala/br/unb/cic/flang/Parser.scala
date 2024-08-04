package br.unb.cic.flang

import scala.util.parsing.combinator._
import Interpreter._

object FLangParser extends JavaTokenParsers {

  def expr: Parser[Expr] = int | bool | add | mul | id | app | eq | different | ifThenElse | or | and

  def int: Parser[CInt] = wholeNumber ^^ { s => CInt(s.toInt) }

  def bool: Parser[CBool] = ("true" | "false") ^^ { s => CBool(s.toBoolean) }

  def add: Parser[Add] = "Add(" ~> expr ~ "," ~ expr <~ ")" ^^ { case lhs ~ "," ~ rhs => Add(lhs, rhs) }

  def mul: Parser[Mul] = "Mul(" ~> expr ~ "," ~ expr <~ ")" ^^ { case lhs ~ "," ~ rhs => Mul(lhs, rhs) }

  def id: Parser[Id] = ident ^^ { s => Id(s) }

  def app: Parser[App] = "App(" ~> ident ~ "," ~ expr <~ ")" ^^ { case name ~ "," ~ arg => App(name, arg) }

  def eq: Parser[Eq] = "Eq(" ~> expr ~ "," ~ expr <~ ")" ^^ { case lhs ~ "," ~ rhs => Eq(lhs, rhs) }

  def different: Parser[Different] = "Different(" ~> expr ~ "," ~ expr <~ ")" ^^ { case lhs ~ "," ~ rhs => Different(lhs, rhs) }

  def ifThenElse: Parser[IfThenElse] = "IfThenElse(" ~> expr ~ "," ~ expr ~ "," ~ expr <~ ")" ^^ { case cond ~ "," ~ thenBranch ~ "," ~ elseBranch => IfThenElse(cond, thenBranch, elseBranch) }

  def or: Parser[Or] = "Or(" ~> expr ~ "," ~ expr <~ ")" ^^ { case lhs ~ "," ~ rhs => Or(lhs, rhs) }

  def and: Parser[And] = "And(" ~> expr ~ "," ~ expr <~ ")" ^^ { case lhs ~ "," ~ rhs => And(lhs, rhs) }

  def parseExpr(input: String): ParseResult[Expr] = parseAll(expr, input)
}

package br.unb.cic.flang

sealed trait Expr

case class CBool(v: Boolean) extends Expr
case class CInt(v: Integer) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: String) extends Expr
case class App(name: String, arg: Expr) extends Expr
case class Eq(lhs: Expr, rhs: Expr) extends Expr 
case class IfThenElse(cond: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr
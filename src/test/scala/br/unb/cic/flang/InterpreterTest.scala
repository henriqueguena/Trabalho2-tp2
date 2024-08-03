package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Interpreter._
import Declarations._
import StateMonad._

class InterpreterTest extends AnyFlatSpec with should.Matchers {

  val inc = FDeclaration("inc", "x", Add(Id("x"), CInt(1)))
  val declarations = List(inc)
  val initialState: S = List("x" -> 42)

  "eval Id(x)" should "return Right(42) when x is bound to 42" in {
    val id = Id("x")
    val (res, _) = runState(eval(id, declarations), initialState)
    res should be (Right(42))
  }

  "eval CInt(5)" should "return Right(5)" in {
    val c5 = CInt(5)
    val (res, _) = runState(eval(c5, declarations), initialState)
    res should be (Right(5))
  }

  "eval CBool(true)" should "return Left(true)" in {
    val cTrue = CBool(true)
    val (res, _) = runState(eval(cTrue, declarations), initialState)
    res should be (Left(true))
  }

  "eval Add(CInt(5), CInt(10))" should "return Right(15)" in {
    val add = Add(CInt(5), CInt(10))
    val (res, _) = runState(eval(add, declarations), initialState)
    res should be (Right(15))
  }

  "eval Mul(CInt(5), CInt(10))" should "return Right(50)" in {
    val mul = Mul(CInt(5), CInt(10))
    val (res, _) = runState(eval(mul, declarations), initialState)
    res should be (Right(50))
  }

  "eval App(inc, CInt(99))" should "return Right(100)" in {
    val app = App("inc", CInt(99))
    val (res, _) = runState(eval(app, declarations), initialState)
    res should be (Right(100))
  }

  "eval Eq(CInt(1), CInt(1))" should "return Left(true)" in {
    val eq = Eq(CInt(1), CInt(1))
    val (res, _) = runState(eval(eq, declarations), initialState)
    res should be (Left(true))
  }

  "eval Eq(CInt(1), CInt(0))" should "return Left(false)" in {
    val eq = Eq(CInt(1), CInt(0))
    val (res, _) = runState(eval(eq, declarations), initialState)
    res should be (Left(false))
  }

  "eval Different(CInt(1), CInt(1))" should "return Left(false)" in {
    val diff = Different(CInt(1), CInt(1))
    val (res, _) = runState(eval(diff, declarations), initialState)
    res should be (Left(false))
  }

  "eval Different(CInt(1), CInt(0))" should "return Left(true)" in {
    val diff = Different(CInt(1), CInt(0))
    val (res, _) = runState(eval(diff, declarations), initialState)
    res should be (Left(true))
  }

  "eval Different(CBool(true), CBool(false))" should "return Left(true)" in {
    val diff = Different(CBool(true), CBool(false))
    val (res, _) = runState(eval(diff, declarations), initialState)
    res should be (Left(true))
  }

  "eval Different(CBool(true), CBool(true))" should "return Left(false)" in {
    val diff = Different(CBool(true), CBool(true))
    val (res, _) = runState(eval(diff, declarations), initialState)
    res should be (Left(false))
  }

  "eval IfThenElse(CBool(true), CInt(100), CInt(200))" should "return Right(100)" in {
    val ifElse = IfThenElse(CBool(true), CInt(100), CInt(200))
    val (res, _) = runState(eval(ifElse, declarations), initialState)
    res should be (Right(100))
  }

  "eval IfThenElse(CBool(false), CInt(100), CInt(200))" should "return Right(200)" in {
    val ifElse = IfThenElse(CBool(false), CInt(100), CInt(200))
    val (res, _) = runState(eval(ifElse, declarations), initialState)
    res should be (Right(200))
  }

  "eval Or(CBool(true), CBool(false))" should "return Left(true)" in {
    val or = Or(CBool(true), CBool(false))
    val (res, _) = runState(eval(or, declarations), initialState)
    res should be (Left(true))
  }

  "eval Or(CBool(false), CBool(false))" should "return Left(false)" in {
    val or = Or(CBool(false), CBool(false))
    val (res, _) = runState(eval(or, declarations), initialState)
    res should be (Left(false))
  }

  "eval And(CBool(true), CBool(true))" should "return Left(true)" in {
    val and = And(CBool(true), CBool(true))
    val (res, _) = runState(eval(and, declarations), initialState)
    res should be (Left(true))
  }

  "eval And(CBool(true), CBool(false))" should "return Left(false)" in {
    val and = And(CBool(true), CBool(false))
    val (res, _) = runState(eval(and, declarations), initialState)
    res should be (Left(false))
  }
}

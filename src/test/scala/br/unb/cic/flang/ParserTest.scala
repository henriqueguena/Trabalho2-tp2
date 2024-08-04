package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

class FLangParserTest extends AnyFlatSpec with should.Matchers {

  "parseExpr(42)" should "return CInt(42)" in {
    val result = FLangParser.parseExpr("42")
    result.successful should be(true)
    result.get should be(CInt(42))
  }

  "parseExpr(true)" should "return CBool(true)" in {
    val result = FLangParser.parseExpr("true")
    result.successful should be(true)
    result.get should be(CBool(true))
  }

  "parseExpr(false)" should "return CBool(false)" in {
    val result = FLangParser.parseExpr("false")
    result.successful should be(true)
    result.get should be(CBool(false))
  }

  "parseExpr(Add(1, 2))" should "return Add(CInt(1), CInt(2))" in {
    val result = FLangParser.parseExpr("Add(1, 2)")
    result.successful should be(true)
    result.get should be(Add(CInt(1), CInt(2)))
  }

  "parseExpr(Mul(3, 4))" should "return Mul(CInt(3), CInt(4))" in {
    val result = FLangParser.parseExpr("Mul(3, 4)")
    result.successful should be(true)
    result.get should be(Mul(CInt(3), CInt(4)))
  }

  "parseExpr(Id(x))" should "return Id(x)" in {
    val result = FLangParser.parseExpr("x")
    result.successful should be(true)
    result.get should be(Id("x"))
  }

  "parseExpr(Add(Add(1, 2), 3))" should "return Add(Add(CInt(1), CInt(2)), CInt(3))" in {
    val result = FLangParser.parseExpr("Add(Add(1, 2), 3)")
    result.successful should be(true)
    result.get should be(Add(Add(CInt(1), CInt(2)), CInt(3)))
  }

  "parseExpr(Add(Id(x), Mul(2, Id(y))))" should "return Add(Id(x), Mul(CInt(2), Id(y)))" in {
    val result = FLangParser.parseExpr("Add(x, Mul(2, y))")
    result.successful should be(true)
    result.get should be(Add(Id("x"), Mul(CInt(2), Id("y"))))
  }
}

package br.unb.cic.flang

case class FDeclaration(name: String, arg: String, body: Expr)

object Declarations {
  def lookup(name: String, declarations: List[FDeclaration]): FDeclaration = declarations match {
    case List() => throw new NoSuchElementException(s"Function $name not found")
    case (f @ FDeclaration(n, _, _)) :: _ if n == name => f
    case _ :: fs => lookup(name, fs)
  }
}


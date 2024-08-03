package br.unb.cic.flang

import Declarations._
import StateMonad._

object Interpreter {

  def eval(expr: Expr, declarations: List[FDeclaration]): M[Integer] =
    expr match {
      case CInt(v) => pure(v)
      case CBool(b) => throw new IllegalArgumentException("Boolean expressions should not be evaluated to integers")
      case Add(lhs, rhs) =>
        bind(eval(lhs, declarations)) { l =>
          bind(eval(rhs, declarations)) { r =>
            pure(l + r)
          }
        }
      case Mul(lhs, rhs) =>
        bind(eval(lhs, declarations)) { l =>
          bind(eval(rhs, declarations)) { r =>
            pure(l * r)
          }
        }
      case Id(name) =>
        bind(get()) { state =>
          pure(lookupVar(name, state))
        }
      case App(name, arg) =>
        val fdecl = lookup(name, declarations)
        bind(eval(arg, declarations)) { value =>
          bind(get()) { s1 =>
            bind(put(declareVar(fdecl.arg, value, s1))) { s2 =>
              eval(fdecl.body, declarations)
            }
          }
        }
      case Eq(lhs, rhs) =>
        bind(eval(lhs, declarations)) { l =>
          bind(eval(rhs, declarations)) { r =>
            pure(if (l == r) 1 else 0)
          }
        }
      case IfThenElse(cond, thenBranch, elseBranch) =>
        bind(evalCondition(cond, declarations)) { condition =>
          if (condition) eval(thenBranch, declarations) else eval(elseBranch, declarations)
        }
    }


  def evalCondition(expr: Expr, declarations: List[FDeclaration]): M[Boolean] =
    expr match {
      case CInt(v) =>
        v match {
          case _ if v.equals(Integer.valueOf(0)) => pure(false)
          case _ if v.equals(Integer.valueOf(1)) => pure(true)
          case _ => throw new IllegalArgumentException("Integer values other than 0 or 1 cannot be evaluated as Boolean")
        }
      case CBool(v) => pure(v)
      case Eq(lhs, rhs) =>
        bind(eval(lhs, declarations)) { l =>
          bind(eval(rhs, declarations)) { r =>
            pure(l == r)
          }
        }
      case _ => throw new IllegalArgumentException("Invalid condition expression")
    }
}


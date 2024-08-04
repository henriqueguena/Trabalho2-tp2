package br.unb.cic.flang

import Declarations._
import StateMonad._

object Interpreter {
  def eval(expr: Expr, declarations: List[FDeclaration]): M[Either[Boolean, Integer]] = expr match {
    case CInt(v) => pure(Right(v))
    case CBool(b) => pure(Left(b))
    case Add(lhs, rhs) =>
      bind(eval(lhs, declarations)) { l =>
        bind(eval(rhs, declarations)) { r =>
          (l, r) match {
            case (Right(li), Right(ri)) => pure(Right(li + ri))
            case _ => throw new IllegalArgumentException("Add operation requires integer operands")
          }
        }
      }
    case Mul(lhs, rhs) =>
      bind(eval(lhs, declarations)) { l =>
        bind(eval(rhs, declarations)) { r =>
          (l, r) match {
            case (Right(li), Right(ri)) => pure(Right(li * ri))
            case _ => throw new IllegalArgumentException("Mul operation requires integer operands")
          }
        }
      }
    case Id(name) =>
      bind(get()) { state =>
        pure(Right(lookupVar(name, state)))
      }
    case App(name, arg) =>
      val fdecl = lookup(name, declarations)
      bind(eval(arg, declarations)) { value =>
        bind(get()) { s1 =>
          bind(put(declareVar(fdecl.arg, value.fold(_ => throw new IllegalArgumentException("Function argument must be integer"), identity), s1))) { _ =>
            eval(fdecl.body, declarations)
          }
        }
      }
    case Eq(lhs, rhs) =>
      bind(eval(lhs, declarations)) { l =>
        bind(eval(rhs, declarations)) { r =>
          (l, r) match {
            case (Right(li), Right(ri)) => pure(Left(li == ri))
            case (Left(lb), Left(rb)) => pure(Left(lb == rb))
            case _ => throw new IllegalArgumentException("Eq operation requires operands of the same type")
          }
        }
      }
    case Different(lhs, rhs) =>
      bind(eval(lhs, declarations)) { l =>
        bind(eval(rhs, declarations)) { r =>
          (l, r) match {
            case (Right(li), Right(ri)) => pure(Left(li != ri))
            case (Left(lb), Left(rb)) => pure(Left(lb != rb))
            case _ => throw new IllegalArgumentException("Different operation requires operands of the same type")
          }
        }
      }
    case IfThenElse(cond, thenBranch, elseBranch) =>
      bind(eval(cond, declarations)) {
        case Left(condition) =>
          if (condition) eval(thenBranch, declarations)
          else eval(elseBranch, declarations)
        case _ => throw new IllegalArgumentException("Condition must be a boolean")
      }
    case Or(lhs, rhs) =>
      bind(eval(lhs, declarations)) { l =>
        bind(eval(rhs, declarations)) { r =>
          (l, r) match {
            case (Left(lb), Left(rb)) => pure(Left(lb || rb))
            case _ => throw new IllegalArgumentException("Or operation requires boolean operands")
          }
        }
      }
    case And(lhs, rhs) =>
      bind(eval(lhs, declarations)) { l =>
        bind(eval(rhs, declarations)) { r =>
          (l, r) match {
            case (Left(lb), Left(rb)) => pure(Left(lb && rb))
            case _ => throw new IllegalArgumentException("And operation requires boolean operands")
          }
        }
      }
  }
}

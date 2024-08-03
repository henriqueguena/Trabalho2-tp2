package br.unb.cic.flang

import cats.data.{StateT, EitherT}
import cats.instances.either._
import cats.syntax.applicative._

object StateAndEH {

  // Define the type of the state
  type S = List[(String, Integer)]

  // Define the type of the error
  type Error = String

  // Define the monad stack
  type Stack[A] = StateT[Either[Error, *], S, A]

  // Helper to lift an Either into the stack
  def liftEither[A](either: Either[Error, A]): Stack[A] =
    StateT.liftF[Either[Error, *], S, A](either)

  // Helper to create pure values in the stack
  def pure[A](a: A): Stack[A] =
    StateT.pure[Either[Error, *], S, A](a)

  // Helper to run the stack
  def runStack[A](state: Stack[A], initialState: S): Either[Error, (S, A)] =
    state.run(initialState)

  // Function to declare a variable in the state
  def declareVar(name: String, value: Integer): Stack[Unit] =
    StateT.modify[Either[Error, *], S](state => (name, value) :: state)

  // Function to lookup a variable in the state
  def lookupVar(name: String): Stack[Integer] =
    StateT.inspectF[Either[Error, *], S, Integer] { state =>
      state.find(_._1 == name) match {
        case Some((_, value)) => Right(value)
        case None => Left(s"Variable '$name' not found")
      }
    }
}

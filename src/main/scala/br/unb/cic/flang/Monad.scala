package br.unb.cic.flang

import cats.data.State

object StateMonad {
  type S = List[(String, Integer)]
  type M[A] = State[S, A]

  def pure[A](a: A): M[A] = State.pure(a)

  def bind[A, B](m: M[A])(f: A => M[B]): M[B] = m.flatMap(f)

  def runState[A](state: M[A], initial: S): (A, S) = {
    val (finalState, result) = state.run(initial).value
    (result, finalState)
  }

  def put(s: S): M[Unit] = State.set(s)

  def get(): M[S] = State.get

  def declareVar(name: String, value: Integer, state: S): S =
    (name, value) :: state

  def lookupVar(name: String, state: S): Integer = state match {
    case List()                      => throw new NoSuchElementException(s"Variable $name not found")
    case (n, v) :: tail if n == name => v
    case _ :: tail                   => lookupVar(name, tail)
  }
}

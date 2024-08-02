package br.unb.cic.flang

import cats.data.State
import cats.syntax.all._ 

package object StateMonad {
  type S = List[(String, Integer)]

  type M[A] = State[S, A]

  def runState[A](state: M[A], initialState: S): (A, S) = {
    val (finalState, result) = state.run(initialState).value
    (result, finalState)
  }

  def put(s: S): M[Unit] = State.set(s)

  def get(): M[S] = State.get

  def declareVar(name: String, value: Integer, state: S): S =
    (name, value) :: state

  def lookupVar(name: String, state: S): Integer = state match {
    case List()                      => ???
    case (n, v) :: tail if n == name => v
    case _ :: tail                   => lookupVar(name, tail)
  }

  def pure[A](a: A): M[A] = State.pure(a)

  def bind[A, B](m: M[A])(f: A => M[B]): M[B] = m.flatMap(f)
}

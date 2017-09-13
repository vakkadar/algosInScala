package org.sanatana.algos

trait State[S,A] {

  def apply(s:S): (A, S)
  def map[B](f:A=>B):State[S, B]  = State{s =>
    val (a, newState) = this(s)
    (f(a), newState)
  }
  def flatMap[B](f:A=>State[S, B]): State[S, B] = State { s =>
    val (a, newState) = this(s)
    f(a)(newState)
  }
}
object State {
  def apply[S, A](r: S => (A, S)): State[S, A] = new State[S, A] {
    def apply(s:S) = r(s)
  }
}

object StackApp extends App {

  type Stack[A] = List[A]
  def push[A](a:A) : State[Stack[A], Unit] = State { stack => ((), a :: stack) }

  def pop[A]: State[Stack[A], Option[A]] = State {
    case a :: tail => (Some(a), tail)
    case Nil       => (None, Nil)
  }

  def popPairs[A]: State[Stack[A],(Option[A], Option[A])] =
    pop[A].flatMap(opt1 => pop[A].map(opt2 => (opt1, opt2) ))

}
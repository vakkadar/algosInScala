package org.sanatana.algos

object StateMonadExample {


  type Stack[A] = List[A]
  type State[S, A] = S => (A, S)

  def push[A](a:A) : State[Stack[A], Unit] = stack => ((), a :: stack)
  def pop[A]: State[Stack[A], Option[A]] =  {
    case head :: tail => (Some(head), tail)
    case Nil  => (None, Nil)
  }

  def popPairs[A]():State[Stack[A], (Option[A], Option[A])] = stack => {
    val (opt1, stack1) = pop(stack)
    val (opt2, stack2) = pop(stack1)
    ((opt1, opt2), stack2)
  }
  def map[S, A, B](sa:State[S, A])(f:A=>B): State[S, B] = state => {
    val (a, newState) = sa(state)
    (f(a), newState)
  }

  def flatMap[S, A, B](sa:State[S, A])(f:A=>State[S, B]): State[S, B] = state => {
    val (a, newState) = sa(state)
    f(a)(newState)
  }

  def popPairs[A]: State[Stack[A],(Option[A], Option[A])] =
    flatMap(pop[A]) { opt1 =>
      map(pop[A]) { opt2 =>
        (opt1, opt2)
      }
    }
}

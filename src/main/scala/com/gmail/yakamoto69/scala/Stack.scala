package sample

case class Stack[A](list: List[A]) {
  def head = list.head
  def tail = list.tail
}

object Stack {

  type StackOp[A,B] = Stack[A] => (B, Stack[A])

  def main(args: Array[String]) {
    val s = push(5, push(4, push(3, push(2, push( 1, empty[Int] )))))
    println( s )

    println( sumTwoElem(s) )

    println( sumFourElem(s) )

    println( combI(pop)(v1 => combI(pop)(v2 => ret(v1 + v2)))(s) )

    println( comb[Int,Int,Unit](pop)(push2)(s) )
    println( comb2(push2(10))(pop)(s) )

    println( empty2(s) )

    println( comb2(push2(10))(comb2[Int,Unit,Unit](empty2)(push2(1)))(s) )
  }

  def empty[A] = Stack(List[A]())

  def empty2[A](s: Stack[A]) = ((), empty[A])

  def pop[A](s: Stack[A]) = ( s.head, Stack(s.tail) )

  def push[A](v: A, s: Stack[A]) = Stack( v :: s.list )

  def push2[A](v: A): Stack[A] => (Unit, Stack[A]) = {
    s => {
      ((), Stack( v :: s.list ))
    }
  }

  def ret[A](v: A): StackOp[A,A] = {
    s => (v, s)
  }

  def sumTwoElem(s: Stack[Int]): (Int, Stack[Int]) = {
    val (v1, s1) = pop(s)
    val (v2, s2) = pop(s1)
    (v1 + v2, s2)
  }

  def sumFourElem(s: Stack[Int]): (Int, Stack[Int]) = {
    val (v1, s1) = pop(s)
    val (v2, s2) = pop(s1)
    val (v3, s3) = pop(s2)
    val (v4, s4) = pop(s3)
    (v1 + v2 + v3 + v4, s4)
  }

  def combI = comb[Int,Int,Int] _

  def comb[A,B,C](m: StackOp[A,B])(n: B => StackOp[A,C]): StackOp[A,C] = {
    s0 => {
      val (v1, s1) = m(s0)
      n(v1)(s1)
    }
  }

  def comb2[A,B,C](m: StackOp[A,B])(n: StackOp[A,C]): StackOp[A,C] = {
    comb(m)(b => n)
  }
}

case class Counter[A](value: A, step: A) {

}

object Counter {

  type CounterOp[A, B] = Counter[A] => (B, Counter[A])

  def comb[A,B,C](m: CounterOp[A,B])(n: B => CounterOp[A,C]): CounterOp[A,C] = {
    s0 => {
      val (v1, s1) = m(s0)
      n(v1)(s1)
    }
  }

  def comb2[A,B,C](m: CounterOp[A,B])(n: CounterOp[A,C]): CounterOp[A,C] = {
    comb(m)(b => n)
  }

  def next(c: Counter[Int]): (Int, Counter[Int]) = {
    val v1 = c.value + c.step
    (v1, Counter(v1, c.step))
  }

  def ret[A](v: A): CounterOp[A,A] = {
    s => (v, s)
  }

  def main(args: Array[String]) {
    println( comb2(next)(comb2(next)(next))( Counter(0, 1) ) )

    println( comb(next)(x => comb(next)(y => comb(next)(z => ret(x+y+z))))( Counter(0, 1) ) )
  }
}

object Nanika {
  type State[S,A] = S => (A, S)

  def comb[S,A,B](m: State[S,A])(n: A => State[S,B]): State[S,B] = {
    s0 => {
      val (a, s1) = m(s0)
      n(a)(s1)
    }
  }

  def ret[A,B](v: A) = {
    s:B => (v, s)
  }
}

class StateMonad[S,A](f: S => (A, S)) {

  def apply(s: S) = f(s)

  def runState = apply _

  def flatMap[B](g: A => StateMonad[S, B]): StateMonad[S, B] = {
    new StateMonad(
      s0 => {
        val (a, s1) = f(s0)
        g(a)(s1)
      }
    )
  }

  def map[B](g: A => B): StateMonad[S, B] = {
    new StateMonad(
      s0 => {
        val (a, s1) = f(s0)
        StateMonad.unit[S,B](g(a))(s1)
      }
    )
  }
}

object StateMonad {

  case class Var(value: Int)

  def main(args: Array[String]) {
    val a = for {
      _ <- add(1)
      v <- get

    } yield {
      v.value
    }

    println( a.runState( new Var(0) ) )

    // println( add(1).flatMap( v => add(2) ).flatMap( v => add(3) ).flatMap(x => current).runState(new Var(0)) )
  }

  def get[S] = new StateMonad[S, S](s => (s, s))

  def put[S](s: S) = new StateMonad[S, Unit](_ => ((), s))

  def current: StateMonad[Var, Int] = {
    // StateMonad[Var, Var] => StateMonad[Var, Int]
    get[Var].map(_.value)
  }

  def add2(n: Int): StateMonad[Var, Int] =
    for ( g <- get[Var];
          x = g.value + n;
          g_ = new Var(x);
          _ <- put[Var](g_)
    ) yield x

  def add(n: Int): StateMonad[Var, Int] = {
    new StateMonad[Var, Int]({
      v =>
        val newValue = v.value + n
        (n, new Var(newValue))
      }
    )
  }

  def unit[S, A](a: A) = new StateMonad[S,A](s => (a, s))
}

package com.gmail.yakamoto69

package object scala {
  def ??? = throw new UnsupportedOperationException

  def generator(start: Int): () => Int = {
    var i = start
    () => {
      val num = i
      i += 1
      num
    }
  }

  def rotate[A](xs: Seq[A], x: A): A = {
    val i = xs.indexOf(x)

    assert(i != -1)

    val n = i + 1
    xs(if (n > xs.length - 1) 0 else n)
  }

  def randomSelect[A](xs: Seq[A]): A = {
    assert(!xs.isEmpty)

    xs(util.Random.nextInt(xs.size))
  }

  implicit def toOptW[A](o: Option[A]) = new OptionW[A](o)

  class OptionW[A](o: Option[A]) {
    def some[X](some: A => X) = new Cata[A, X](o, some)
  }

  class Cata[A, X](o: Option[A], some: A => X) {
    def none(none: => X): X = o match {
      case Some(a) => some(a)
      case None => none
    }
  }
}

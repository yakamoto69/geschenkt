package com.gmail.yakamoto69.scala

import com.gmail.yakamoto69
import yakamoto69.scala._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PackageSpec extends FunSuite {

  test("generator") {
    val i = generator(start = 5)

    assert(5 == i())
    assert(6 == i())
    assert(7 == i())
  }

  test("rotate") {
    val xs = Seq(1, 2, 3)

    assert(2 == rotate(xs, 1))
    assert(3 == rotate(xs, 2))
    assert(1 == rotate(xs, 3))
  }

  test("some, none") {
    assert(1 == Some(1).some(x=>x).none(0))
    assert(0 == Option.empty[Int].some(x=>x).none(0))
  }

  test("buffer") {
    val i = generator(1)
    val buff = Buffer(i())
    assert(1 == buff.peek())
    assert(1 == buff.peek()) // peekでは変わらない
    assert(1 == buff.pop()) // popしたから次呼んだときに値が変わってる
    assert(2 == buff.peek())
  }
}

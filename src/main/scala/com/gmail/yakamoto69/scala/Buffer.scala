package com.gmail.yakamoto69.scala

import com.gmail.yakamoto69
import yakamoto69.scala._

trait Buffer[+A] {

  def peek(): A

  def pop(): A

  def sync: Buffer[A]
}


object Buffer {
  def apply[A](create: => A): Buffer[A] = new BufferImpl(create)


  class BufferImpl[A](create: => A) extends Buffer[A] {

    self =>

    private var buff: Option[A] = None

    private def setBuff(): A = {
      create hook { a =>
        buff = Some(a)
      }
    }

    def peek(): A = {
      buff getOrElse setBuff()
    }

    def pop(): A = {
      peek() hook { _ =>
        buff = None
      }
    }

    def sync = new Buffer[A] {
      def peek() = self.synchronized { self.peek() }
      def pop() = self.synchronized { self.pop() }
      def sync = this
    }
  }
}

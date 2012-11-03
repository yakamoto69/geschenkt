package com.gmail.yakamoto69.scala

import actors.threadpool.locks.{Condition, ReentrantLock}

class EasyLock {
  val lock = new ReentrantLock

  def mkCondition(f: => Boolean): EasyCondition = {
    new EasyCondition(lock.newCondition(), f)
  }

  def apply[A](f: => A): A = {
    lock.lock()
    try {
      f
    } finally {
      lock.unlock()
    }
  }
}

class EasyCondition(c: Condition, condition: => Boolean) {

  def isTrue = condition

  def waitUntilFulfilled[A](f: => A): A = {
    while (!isTrue) {
      c.await()
    }
    f
  }

  def signalAllIfFulfilled() {
    if (isTrue) c.signalAll()
  }

  def signalIfFulfilled() {
    if (isTrue) c.signal()
  }
}
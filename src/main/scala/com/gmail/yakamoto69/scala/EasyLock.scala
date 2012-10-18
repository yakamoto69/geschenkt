package com.gmail.yakamoto69.scala

import actors.threadpool.locks.{Condition, ReentrantLock}

class EasyLock {
  val lock = new ReentrantLock

  def mkCondition(f: => Boolean): EasyCondition = {
    new EasyCondition(lock.newCondition(), f)
  }

  def apply(f: => Unit) {
    lock.lock()
    try {
      f
    } finally {
      lock.unlock()
    }
  }
}

class EasyCondition(c: Condition, condition: => Boolean) {

  def waitUntilFulfilled(f: => Unit) {
    while (!condition) {
      c.await()
    }
    f
  }

  def signalAllIfFulfilled() {
    if (condition) c.signalAll()
  }

  def signalIfFulfilled() {
    if (condition) c.signal()
  }
}
package com.gmail.yakamoto69.scala

import com.gmail.yakamoto69
import yakamoto69.scala._
import annotation.tailrec

class SM[S, I](transitionF: S => I => S) {
  def recv(state: S, input: I): S = transitionF(state)(input)

  def hook(hookF: PartialFunction[(S, I), Unit]): SM[S, I] = {
    new SM[S, I]({ s => i =>
      val next = transitionF(s)(i)
      if (hookF.isDefinedAt(s, i)) hookF(s, i)
      next
    })
  }

}

/**
 * うまくできなかったのでLockを使わないバージョンに変える。
 * 実行スレッド、Broker, Offer, PoolしかでてこなければLockはいらないはず
 */
class AutonomousSM[S, I](sm: SM[S, I], terminal: S)(autonomousF:  S => I) {
  def start(start: S) {
    @tailrec
    def startR(s: S) {
      if (s != terminal) startR(sm.recv(s, autonomousF(s)))
    }

    startR(start)
  }
}


object AutonomousSM {

  /**
   * conditions: name -> 条件関数
   */
  class Conditions(conditions: Map[String, () => Boolean]) {

    private val lockAndConditions: String => (EasyLock, EasyCondition) = conditions map {
      case (name, conditionF) => {
        val lock = new EasyLock
        name -> (lock, lock.mkCondition(conditionF()))
      }
    }


    def mkOption[A](name: String)(onTrue: => A) = new ConOption(name, onTrue)

    def waitUntil[A](name: String)(onTrue: => A): A = {
      withLock(name) { condition =>
        condition.waitUntilFulfilled(onTrue)
      }
    }

    /**
     * 条件にかかわる変数の値を変える時はこれを呼んで、fの中で値を変えないといけない
     */
    def change(name: String)(f: => Unit) {
      withLock(name) { condition =>
        f
        condition.signalIfFulfilled()
      }
    }

    private def withLock[A](name: String)(f: EasyCondition => A): A =  {
      val (lock, condition) = lockAndConditions(name)
      lock {
        f(condition)
      }
    }


    class ConOption[A](name: String, onTrue: => A) {

      def getOrElse(onFalse: => A): A = {
        withLock(name) { condition =>
          if (condition.isTrue) onTrue else onFalse
        }
      }

      def orElse(alternative: ConOption[A]): ConOption[A] = {
        withLock(name) { condition =>
          if (condition.isTrue) this else alternative
        }
      }
    }
  }

}

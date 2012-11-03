package com.gmail.yakamoto69.scala

import com.gmail.yakamoto69
import yakamoto69.scala._

import com.twitter.concurrent.{NamedPoolThreadFactory, Broker}
import com.twitter.util.{ExecutorServiceFuturePool, FuturePool}
import java.util.concurrent.{Executors, TimeUnit}
import annotation.tailrec

object RepeatTask {

  /**
   * task を　repeatMilliSecsミリ秒間できるだけたくさん実行する
   * task はもちろんスレッドセーフじゃないといけないぞ！
   */
  def run(repeatMilliSecs: Long, task: => Unit): Int = {

    val b = new Broker[Unit]
    // FuturePool.defaultPoolと一緒。
    // タスクが全部終わるのを待つ必要があったけど、shutdownなしでできないので、仕方なく別のPoolを作ることにした
    val pool = new ExecutorServiceFuturePool(
      Executors.newFixedThreadPool(
        scala.collection.parallel.availableProcessors,
        new NamedPoolThreadFactory("RepeatTask")
      )
    )
    val o = b.recv

    // 自分でこれを決めなくてはいけないのは嫌だなあ。何か手はないものか
    val maxRunningSize = scala.collection.parallel.availableProcessors

    val t = Timer.start(repeatMilliSecs)

    @tailrec
    def cycle(runningCnt: Int = 0, acc: Int = 0): Int = {
      if (t.isOver) return acc

      if (runningCnt < maxRunningSize) {
        pool(task).onSuccess { b ! _ }
        cycle(runningCnt + 1, acc + 1)
      } else {
        o.syncWait()
        cycle(runningCnt - 1, acc)
      }
    }

    val cnt = cycle()

    pool.executor.shutdown()

    assert(pool.executor.awaitTermination(1, TimeUnit.SECONDS))

    cnt
  }
}
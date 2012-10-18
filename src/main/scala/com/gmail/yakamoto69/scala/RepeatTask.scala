package com.gmail.yakamoto69.scala

import com.gmail.yakamoto69
import yakamoto69.scala._

import actors.threadpool.{TimeUnit, Future, Executors}
import collection.mutable.ListBuffer

object RepeatTask {

  /**
   * task を　repeatMilliSecsミリ秒間できるだけたくさん実行する
   * task はもちろんスレッドセーフじゃないといけないぞ！
   */
  def run(task: Runnable, repeatMilliSecs: Long): Int = {
    val executor = Executors.newCachedThreadPool
    val remover = Executors.newCachedThreadPool

    val queue = ListBuffer[Future]()
    def removeDone() {
      (queue filter (_.isDone)) foreach { f =>
        queue.remove(queue indexOf f)
      }
    }

    val lock = new EasyLock()

    // queueサイズが最大同時スレッド数になるので注意
    // 自分でこれを決めなくてはいけないのは嫌だなあ。何か手はないものか
    val maxQueueSize = scala.collection.parallel.availableProcessors

    val queueAvailable = lock.mkCondition(queue.size < maxQueueSize)

    var cnt = 0

    val t = Timer.start(repeatMilliSecs)

    while(!t.isOver) {
      lock {
        queueAvailable.waitUntilFulfilled()

        cnt += 1
        val r = executor.submit(task)
        queue += r

        remover.submit(runnable {
          r.get() // タスク終わるまで待つ
          lock {
            removeDone()
            queueAvailable.signalIfFulfilled()
          }
        })
      }
    }

    executor.shutdown()
    remover.shutdown()

    assert(executor.awaitTermination(1, TimeUnit.SECONDS))

    cnt
  }

  def runnable(f: => Unit) = new Runnable {
    def run() {
      f
    }
  }
}
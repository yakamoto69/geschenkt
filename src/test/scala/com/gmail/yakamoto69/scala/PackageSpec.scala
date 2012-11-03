package com.gmail.yakamoto69.scala

import com.gmail.yakamoto69
import yakamoto69.scala._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import yakamoto69.scala.AutonomousSM.Conditions
import scala.actors.Actor._
import com.twitter.conversions.thread

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

  test("StateMachine") {
    val s = new SM[Int, String]({
      case 1 => _ match {
        case "next" => 2
      }
      case 2 => _ match {
        case "prev" => 1
      }
    })

    assert(2 == s.recv(1, "next"))
    assert(1 == s.recv(2, "prev"))
  }

  test("Autonomous State Machine") {

    // 純粋なステートマシン
    val sm = new SM[Symbol, String]({
      case 'working => {
        case "work" => 'working
        case "wait" => 'waiting
        case "end" => 'end
      }

      case 'waiting => {
        case "work" => 'working
//        case "end" => 'end   'waiting -> 'endの遷移があると、'waitingで待つ条件が難しくなるのでやめ
      }
    })


    var total = 0
    var workingCnt = 0

    // 遷移の条件に使われる
    // autonomousFやhookFが引数に取るようにしたほうがいいかも
    val conditions = new Conditions(Map(
      "workable" -> (() => workingCnt < 3),
      "end" -> (() => total >= 10)
    ))

    def work() {
      actor {
        conditions.change("end") {
          total += 1
        }

        println("begin work")
        Thread.sleep(100)
        println("finish work")

        conditions.change("workable") {
          workingCnt -= 1
        }
      }
    }

    // ('working, "work") にフックしてwork()を実行するようにしたステートマシン
    val workingSm = sm.hook {
      case ('working, "work") => work()
    }

    // 終端ステートになるまで自力でInputを求めて遷移を続ける
    val asm = new AutonomousSM(workingSm, 'end)({

      case 'working => {
        val end = conditions.mkOption("end") {
          "end"
        }
        val work = conditions.mkOption("workable") {
          workingCnt += 1
          "work"
        }

        end orElse work getOrElse "wait"
      }

      case 'waiting => {
        conditions.waitUntil("workable") {
          "work"
        }
      }
    })

    asm.start('working)

    println(total)
    println(workingCnt)
  }

  test("RepeatTask") {
    val cnt = RepeatTask.run(500, (1 to 3000).foldRight(0)(_ + _))
    println("cnt:"+cnt)
  }
}

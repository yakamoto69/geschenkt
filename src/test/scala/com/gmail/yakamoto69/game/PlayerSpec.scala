package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import org.scalatest.{BeforeAndAfter, FunSpec}

class PlayerSpec extends FunSpec with BeforeAndAfter {

  var player: Player = _

  before {
    player = new Player("test")
  }

  describe("A Player") {

    it("should score a straight of cards as a minimum number of those") {
      assert(0 == player.score)

      val cards = (
        Seq(5) ++
        Seq(10, 11, 12) ++ // straight
        Seq(15, 16, 17) // straight
      ) map toCard

      cards foreach (player.pick(_))

      expectResult(5 + 10 + 15) {
        player.score
      }
    }

    it("should subtract The number of chips from the score") {
      player.pick(toCard(20))

      assert(20 == player.score)

      player.numOfChips += 10

      expectResult(10){
        player.score
      }
    }

  }


  def toCard(num: Int): Card = Card(num)
}

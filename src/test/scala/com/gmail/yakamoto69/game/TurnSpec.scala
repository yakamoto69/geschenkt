package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import org.scalatest.{BeforeAndAfter, FunSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TurnSpec extends FunSpec with BeforeAndAfter {

  var game: Game = _
  var player1, player2, player3: Player = _
  var round: Round = _

  before {
    def p(i: Int) = {
      val p = new Player("player"+i)
      p.numOfChips = 10 // なくならない程度にたくさん
      p
    }

    player1 = p(1); player2 = p(2); player3 = p(3)
    game = new Game(Seq(player1, player2, player3))
    game.backedCards = new FixedBackedCards(((10 until 20) map toCard).toList)  // 適当に、なくならない程度にたくさん
    round = game.round
  }

  describe("A Turn") {

    it("should rotate around all the players") {
      // rotate player1 -> player2 -> player3 -> player1

      round.start()

      assert(player1 == game.turnPlayer)

      round.doTurn(Pass())

      assert(player2 == game.turnPlayer)

      round.doTurn(Pass())

      assert(player3 == game.turnPlayer)

      round.doTurn(Pass())

      assert(player1 == game.turnPlayer) // player1に戻る
    }

    it("When a player ends a round, the player plays the first turn of the next round") {
      round.start()

      assert(player1 == game.turnPlayer)

      round.doTurn(Pick()) // ラウンドを終わらせる

      assert(player1 == game.turnPlayer) // 変わらない
    }

    it("When a game begins, the start player plays the first turn") {
      pending
    }
  }


  def toCard(num: Int) = new Card(num)
}

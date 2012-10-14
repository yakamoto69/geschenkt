package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import org.scalatest.{BeforeAndAfter, FunSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ChoiceSpec extends FunSpec with BeforeAndAfter {

  var game: Game = _
  var round: Round = _
  var player: Player = _

  before {
    player = new Player("test")
    player.numOfChips = 5

    game = new Game(Seq(player))
    game.backedCards = new FixedBackedCards(Card(10) :: Nil)
    round = game.round
    round.start()
  }

  describe("A Choice of a Player") {

    it("When a player passes, he/she should pay a chip") {
      assert(5 == player.numOfChips)

      round.doTurn(Pass())

      expectResult(4) {
        player.numOfChips
      }
    }

    it("When a player pay a chip, the chip should be piled on the board") {
      assert(0 == game.numOfChipsOnBoard)

      round.doTurn(Pass())

      expectResult(1) {
        game.numOfChipsOnBoard
      }
    }

    it("When a player picks a faced card, he/she should take also all the chips piled on the board") {
      game.numOfChipsOnBoard = 2

      val faced = round.facedCard.get // 表向いたカードがあるはず

      assert(!(player.cards contains faced))

      round.doTurn(Pick())

      expectResult(5 + 2) {
        player.numOfChips
      }

      assert(player.cards contains faced)
    }

    it("If a player has no chip, he/she can't pass") {
      player.numOfChips = 0

      intercept[AssertionError] {
        round.doTurn(Pass())
      }
    }
  }
}

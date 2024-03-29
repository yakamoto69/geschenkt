package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import org.scalatest.{BeforeAndAfter, FunSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RoundSpec extends FunSpec with BeforeAndAfter {

  var game: Game = _
  var round: Round = _

  before {
    val p = new Player("test")
    p.numOfChips = 10 // 0 にならない程度にたくさん
    game = new Game(Seq(p))
    game.backedCards = new FixedBackedCards(((10 until 20) map toCard).toList)  // 適当に、なくならない程度にたくさん
    round = game.round
  }

  /**
   * このテストではPlayerが出てこないようにしたい
   */
  describe("A Round") {

    it("When a round starts, a backed card faces up") {
      val cardsNum = game.backedCards.size

      assert(!round.facedCard.isDefined)  // 最初は表を向いているカードはない

      round.start()

      assert(round.facedCard.isDefined)

      // 山札が一枚減る
      expectResult(cardsNum - 1) {
        game.backedCards.size
      }
    }

    it("should continue untill any player picks a faced card") {
      round.start()

      assert(!round.isEnd)

      round.doTurn(Pass())

      assert(!round.isEnd)

      round.doTurn(Pick())

      assert(round.isEnd)
    }

    it("When a round ends, the next round begins") {
      round.start()

      round.doTurn(Pick())  // ラウンドを終了させる

      assert(game.round.isRunning)

      expectResult(round.num + 1) {
        game.round.num
      }
    }

    it("When a round finishes and there are no bakced cards, the game is over") {
      game.backedCards = new FixedBackedCards(List(Card(10)))  // 最後の一枚

      round.start()

      assert(!game.isOver)

      round.doTurn(Pick())  // ラウンドを終了させる

      assert(game.isOver)
    }
  }


  def toCard(num: Int) = new Card(num)
}

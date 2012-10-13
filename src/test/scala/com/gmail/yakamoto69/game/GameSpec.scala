package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import org.scalatest.{FunSuite, BeforeAndAfter}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class GameSpec extends FunSuite with BeforeAndAfter {

  var game: Game[Player] = _
  var player1, player2, player3: Player = _

  before {
    def p(i: Int) = new Player("player"+i)
    player1 = p(1); player2 = p(2); player3 = p(3)
    game = new Game(Seq(player1, player2, player3))
  }


  test("A player who scores fewest among the players is a winner") {
    player1.pick(Card(10)) // fewest
    player2.pick(Card(20))
    player3.pick(Card(30))

    expectResult(player1) {
      game.winner
    }
  }

}

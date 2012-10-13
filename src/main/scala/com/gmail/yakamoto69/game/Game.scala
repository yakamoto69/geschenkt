package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import util.Random

object Game {

  def main(args: Array[String]) {

    def initP[P <: Player](p: P): P = {
      p.numOfChips = 11
      p
    }

    def ai(i: Int) = initP(new Player("AI"+i) with Ai)
    def player = initP(new Player("Player") with ConsoleUi)

    def initCards = {
      val all = util.Random.shuffle((3 to 35).toList)
      all.drop(9) map Card
    }

    val players = Seq(player, ai(1), ai(2), ai(3))
    val game = new Game(Random.shuffle(players))
    game.backedCards = initCards

    game.round.start()
    while (!game.isOver) {
      val choice = game.turnPlayer.choose(game)
      println(game.turnPlayer.name+" "+choice)
      game.round.doTurn(choice)
    }

    println("winner: "+game.winner.name)
    players foreach { p =>
      println(p.info)
    }
  }
}

/**
 * genericsにするのは正しいのか？
 * 使う側でラップすればいいんじゃないかしら？
 */
class Game[P <: Player](val players: Seq[P]) {
  require(!players.isEmpty)

  val startPlayer = players(0)

  var turnPlayer = startPlayer

  var round: Round[P] = new Round(1, this)

  var backedCards: List[Card] = Nil

  var numOfChipsOnBoard: Int = _

  var isOver: Boolean = _

  def winner = {
    players minBy (_.score) // playerは空じゃない
  }

  def onRoundEnd() {
    if (backedCards.isEmpty)
      isOver = true
    else
      startNextRound()
  }

  def startNextRound() {
    round = new Round(round.num + 1, this)
    round.start()
  }

  def nextPlayer = {
    rotate(players, turnPlayer)
  }
}


/**
 * Gameのinnerの方がいい？
 */
class Round[P <: Player](val num: Int, game: Game[P]) {
  require(num > 0)

  private trait State
  private case class NotStart() extends State
  private case class Running() extends State
  private case class End() extends State

  private var state: State = NotStart()

  def isEnd = state == End()
  def isRunning = state == Running()

  var facedCard: Option[Card] = None

  def doTurn(choice: Choice) {
    choice match {
      case Pass() => {
        def payChip() {
          assert(game.turnPlayer.numOfChips > 0)

          game.turnPlayer.numOfChips -= 1
          game.numOfChipsOnBoard += 1
        }

        payChip()
        game.turnPlayer = game.nextPlayer
      }

      case Pick() => {
        def pickChips() {
          game.turnPlayer.numOfChips += game.numOfChipsOnBoard
          game.numOfChipsOnBoard = 0
        }

        def pickCard() {
          facedCard
            .some(game.turnPlayer.pick)
            .none(throw new AssertionError)
        }

        pickChips()
        pickCard()
        state = End()
        game.onRoundEnd()
      }
    }
  }

  def start() {
    val (head :: tail) = game.backedCards
    facedCard = Some(head)
    game.backedCards = tail

    state = Running()
  }

  def info = {
    val faced = facedCard.some(_.num.toString).none("none")
    "round:"+num+" faced:"+faced+" chips:"+game.numOfChipsOnBoard
  }
}


case class Card(num: Int)


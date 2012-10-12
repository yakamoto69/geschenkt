package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._

class Game(val players: Seq[Player]) {
  var round: Round = new Round(1, this)


  var backedCards: List[Card] = Nil

  var isOver: Boolean = _

  def winner: Player = {
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
}


class Round(val num: Int, game: Game) {

  private trait State
  private case class Default() extends State
  private case class Running() extends State
  private case class End() extends State

  private var state: State = Default()

  def doTurn(choice: Choice) {
    choice match {
      case Pay() =>
      case Pick() => {
        state = End()
        game.onRoundEnd()
      }
    }
  }

  def isEnd = state == End()
  def isRunning = state == Running()

  var facedCard: Option[Card] = None

  def start() {
    val (head :: tail) = game.backedCards
    facedCard = Some(head)
    game.backedCards = tail

    state = Running()
  }
}


case class Card(num: Int)


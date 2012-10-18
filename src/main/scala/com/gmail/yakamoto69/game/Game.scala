package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import util.Random

object Game {

  def main(args: Array[String]) {

    val console = new ConsoleUi

    def initP[P <: Player](p: P): P = {
      p.numOfChips = 11
      p
    }

    def ai(i: Int) = initP(new Player("AI"+i) with Ai)
    def player: Player with Human = initP(new Player("Player") with Human {
      val ui = console
    })

    def initCards = {
      val all = util.Random.shuffle((3 to 35).toList)
      all.drop(9) map Card
    }

    val players = Seq(player, ai(1), ai(2), ai(3))
    val game = new InteractiveGame(console, Random.shuffle(players))
    game.backedCards = new FixedBackedCards(initCards)
    console.game = game

    game.start()
  }
}

class Game(val players: Seq[Player]) {

  require(!players.isEmpty)

  val startPlayer = players(0)

  var turnPlayer = startPlayer

  var round: Round = new Round(1, this)

  var backedCards: BackedCards = new FixedBackedCards(Nil)

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

  def onRoundStart() {}

  def startNextRound() {
    round = new Round(round.num + 1, this)
    round.start()
  }

  def nextPlayer = {
    rotate(players, turnPlayer)
  }

  def allOptions: Seq[Choice] = {
    val pass = if (turnPlayer.numOfChips > 0) Some(Pass()) else None
    val pick = Some(Pick())

    Seq(pass, pick).flatten filterNot (_=>isOver)
  }
}

class InteractiveGame(ui: Ui, players: Seq[Player with Playable]) extends Game(players) {

  private def toPlayable(p: Player): Player with Playable = {
    (players find (p ==)).get
  }

  override def onRoundEnd() {
    ui.onRoundEnd()
    super.onRoundEnd()
  }

  override def onRoundStart() {
    ui.onRoundStart()
    super.onRoundStart()
  }

  def start() {
    round.start()

    while (!isOver) {
      val choice = toPlayable(turnPlayer).choose(this)
      ui.onChosen(choice)
      round.doTurn(choice)
    }

    ui.onEnd()
  }
}


/**
 * Gameのinnerの方がいい？
 */
class Round(val num: Int, game: Game) {
  require(num > 0)

  import Round._

  var state: State = NotStart()

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
    val (faced, backed) = game.backedCards.faceUp
    facedCard = Some(faced)
    game.backedCards = backed

    state = Running()
    game.onRoundStart()
  }

  def info = {
    val faced = facedCard.some("'"+_.num+"'").none("none")
    "round "+num+" faced:"+faced+" chips:"+game.numOfChipsOnBoard
  }
}

object Round {
  trait State
  case class NotStart() extends State
  case class Running() extends State
  case class End() extends State
}


trait BackedCards {
  def faceUp: (Card, BackedCards) // (faced, remain)

  def isEmpty: Boolean

  def size: Int

  def possibleCards: Seq[Card] // まだ目に見えてないカード。山札とは違うよ
}


class FixedBackedCards(cards: List[Card], removed: Seq[Card] = Nil) extends BackedCards {
  def faceUp: (Card, BackedCards) = {
    val head :: tail = cards
    (head, new FixedBackedCards(tail))
  }

  def isEmpty = cards.isEmpty

  def size = cards.size

  def possibleCards = cards ++ removed  // 山札 + 取り除いたカード
}


case class Card(num: Int)


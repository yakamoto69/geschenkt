package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import collection.mutable.ArrayBuffer
import annotation.tailrec

object PlayOutStrategy {

  case class Result(winner: Player)

  /**
   * game.turnPlayerの勝率が高くなる選択肢を求める
   */
  def bestChoice(game: Game): Choice = {
    def winRate(choice: Choice): Double = {
      val total = 10000 // todo

      @tailrec
      def playN(n: Int, wins: Int = 0): Int = {
        if (n == 0)
          wins
        else {
          val advanced = mkAdvanced(game, choice)
          val r = randomPlayOut(advanced)
          playN(n - 1, if (advanced.players.indexOf(r.winner) == game.players.indexOf(game.turnPlayer)) wins + 1 else wins)
        }
      }

      val wins = playN(total)
      wins.toDouble / total.toDouble
    }

    val options = game.allOptions
    if (options.size == 1) return options(0) // 選択肢が1個しかないから計算する必要なし

    val rates = options map (winRate(_))
//    println((options zip rates))
    (options zip rates).maxBy(_._2)._1
  }


  private def randomPlayOut(game: Game): Result = {
    //    val choices = ArrayBuffer[Choice]()
    while(!game.isOver) {
      val choice = randomSelect(game.allOptions)
      //      choices += choice
      game.round.doTurn(choice)
    }
    Result(game.winner)
  }

  /**
   * gameを一手進めた状態のGameを作る
   * gameは、手が進むたびに状態を変えるモデルなのでコピーしないといけない
   */
  private def mkAdvanced(game: Game, choice: Choice): Game = {
    val copy: Game = copyGame(game)
    copy.round.doTurn(choice)
    copy
  }


  private def copyGame(game: Game): Game = {
    val copy = new Game(game.players map copyPlayer)
    copy.numOfChipsOnBoard = game.numOfChipsOnBoard
    copy.isOver = game.isOver
    copy.backedCards = new RandomBackedCards(game.backedCards.cards)
    copy.turnPlayer = copy.players(game.players.indexOf(game.turnPlayer)) // copyしたplayerの中から対応するplayerを選ぶ
    copy.round = copyRound(game.round, copy)
    copy
  }

  private def copyRound(round: Round, copiedGame: Game): Round = {
    val copy = new Round(round.num, copiedGame)
    copy.facedCard = round.facedCard
    copy.state = round.state
    copy
  }

  private def copyPlayer(p: Player): Player = {
    val copy = new Player(p.name)
    copy.numOfChips = p.numOfChips
    copy.cards ++= p.cards
    copy
  }
}

// todo 省いたカードの分を考慮してない
class RandomBackedCards(val cards: Seq[Card]) extends BackedCards {
  def faceUp: (Card, BackedCards) = {
    val faced = randomSelect(cards)
    val remain = cards filterNot (faced ==)
    (faced, new RandomBackedCards(remain))
  }
}




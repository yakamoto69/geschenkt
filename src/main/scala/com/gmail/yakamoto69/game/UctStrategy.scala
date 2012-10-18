package com.gmail.yakamoto69.game

import ai._
import com.gmail.yakamoto69
import yakamoto69.scala._

object UctStrategy {

  def toWorld(game: Game, player: Int, selectedChoices: List[Choice] = Nil): McWorld = new McWorld {

    var copy = Buffer(mkCopy).sync

    private def mkCopy: Game = {
      val c = PlayOut.copyGame(game)
      selectedChoices foreach (c.round.doTurn(_))
      c
    }

    def allChoices = copy.peek().allOptions map toMcChoice

    def proceed(choice: McChoice): McWorld = {
      choice match {
        // この使い方っていいの？ダウンキャストとかわんないんだけど
        case WrapChoice(c) => {
          toWorld(game, player, selectedChoices :+ c)
        }
      }
    }

    def playOut: Boolean = {
      val r = PlayOut.randomPlayOut(copy.pop()).winner == player
      r
    }
  }

  def bestChoice(game: Game): Choice = {
    val playerIx = game.players.indexOf(game.turnPlayer)
    val world = toWorld(game, playerIx)
    new MonteCarlo().compute(world) match {
      case WrapChoice(c) => c // 実質ダウンキャストじゃないか？
    }
  }

  private def toMcChoice(choice: Choice) = WrapChoice(choice)

  case class WrapChoice(choice: Choice) extends McChoice
}

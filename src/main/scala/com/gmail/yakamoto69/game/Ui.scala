package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import annotation.tailrec

trait Ui {
  def promptToChoose(player: Player): Choice

  def onRoundStart()

  def onRoundEnd()

  def onChosen(choice: Choice)

  def onEnd()
}

class ConsoleUi extends Ui {

  var game: Game = _

  private val DoPick = """\s*(?i:pick)\s*""".r
  private val DoPass = """\s*(?i:pass)\s*""".r
  private val ShowStatus = """\s*(?i:status)\s*""".r

  def promptToChoose(player: Player): Choice = {

    def showStatus() {
      game.players foreach { p => println(p.info) }
      println(game.round.info)
    }

    @tailrec
    def chooseByConsole(): Choice = {
      print(">")

      readLine() match {
        case DoPick() => Pick()
        case DoPass() => Pass()

        case ShowStatus() => {
          showStatus()
          chooseByConsole()
        }

        case _ => {
          println("Wrong Command")
          chooseByConsole()
        }
      }
    }


    showStatus()
    chooseByConsole()
  }

  def onChosen(choice: Choice) {
    val c = choice match {
      case Pass() => "pass"
      case Pick() => "pick "+facedCard+" and "+game.numOfChipsOnBoard+" chips"
    }
    println(game.turnPlayer.name+" "+c)
  }

  def onEnd() {
    println("winner: "+game.winner.name)
    game.players foreach { p =>
      println(p.info)
    }
  }

  def facedCard = {
    "'"+game.round.facedCard.get.num+"'"
  }

  def roundNum = {
    game.round.num
  }

  def onRoundStart() {
    println("round "+roundNum+" start: "+facedCard+" faced")
  }

  def onRoundEnd() {
    println("round "+roundNum+" end")
  }
}

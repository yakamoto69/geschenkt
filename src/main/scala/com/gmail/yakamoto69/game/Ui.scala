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

class ConsoleUi[P <: Player] extends Ui {

  var game: Game[P] = _

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
      case Pick() => "pick '"+game.round.facedCard.get.num+"' and "+game.numOfChipsOnBoard+" chips" // todo get使ってる
    }
    println(game.turnPlayer.name+" "+c)
  }

  def onEnd() {
    println("winner: "+game.winner.name)
    game.players foreach { p =>
      println(p.info)
    }
  }

  def onRoundStart() {
    val num = game.round.num
    val newCard = game.round.facedCard.get.num // todo get使ってる
    println("round "+num+" start: '"+newCard+"' faced")
  }

  def onRoundEnd() {
    val num = game.round.num
    println("round "+num+" end")
  }
}

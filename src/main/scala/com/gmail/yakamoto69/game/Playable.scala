package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import annotation.tailrec

trait Playable {
  def choose(game: Game[_]): Choice
}

trait Ai extends Playable {
  self: Player =>

  def choose(game: Game[_]): Choice = {
    if (numOfChips > 0)
      Pass()
    else
      Pick()
  }
}


trait ConsoleUi extends Playable {
  self: Player =>

  private val DoPick = """\s*(?i:pick)\s*""".r
  private val DoPass = """\s*(?i:pass)\s*""".r
  private val ShowStatus = """\s*(?i:status)\s*""".r

  def choose(game: Game[_]): Choice = {

    def pl = println(_:String)

    def showStatus() {
      pl(self.info)
      pl(game.round.info)
    }

    @tailrec
    def chooseByConsole(): Choice = {
      Console.print(">")

      Console.readLine() match {
        case DoPick() => Pick()
        case DoPass() => Pass()

        case ShowStatus() => {
          showStatus()
          chooseByConsole()
        }

        case _ => {
          pl("Wrong Command")
          chooseByConsole()
        }
      }
    }

    showStatus()
    chooseByConsole()
  }
}
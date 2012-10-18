package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import annotation.tailrec

trait Playable {
  def choose(game: Game): Choice
}

trait Ai extends Playable {
  self: Player =>

  def choose(game: Game): Choice = {
    assert(self == game.turnPlayer)

    UctStrategy.bestChoice(game)
  }
}


trait Human extends Playable {
  self: Player =>

  def ui: Ui

  def choose(game: Game): Choice = {
    assert(self == game.turnPlayer)

    ui.promptToChoose(self)
  }
}
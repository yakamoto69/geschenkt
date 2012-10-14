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


trait Human extends Playable {
  self: Player =>

  def ui: Ui

  def choose(game: Game[_]): Choice = {
    ui.promptToChoose(self)
  }
}
package com.gmail.yakamoto69.scala

import com.gmail.yakamoto69
import yakamoto69.scala._

object Timer {
  def start(milliSecs: Long) = new Timer(milliSecs)
}

class Timer(milliSecs: Long) {
  val start = System.nanoTime()
  def isOver = System.nanoTime() - start > 1000000 * milliSecs
}
package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import collection.mutable.ArrayBuffer

class Player(val name: String) {

  var numOfChips: Int = _
  val cards = ArrayBuffer[Card]()

  def pick(card: Card) {
    cards += card
  }

  def score: Int = scoreOfCards - numOfChips

  def cInfo = cards.map(_.num).sorted.mkString(" ")

  def info = {
    name+": score="+score+" chips="+numOfChips+" cards="+cInfo
  }

  private def scoreOfCards: Int = {

    case class Strait(cards: List[Card]) {
      require(!cards.isEmpty)

      def minNum: Int = cards.head.num
      def add(card: Card): Strait = Strait(card :: cards)
    }

    val sorted = cards sortBy (_.num)

    // どうやら折りたたみでList作るときはfoldRightするものらしい
    val straits = sorted.foldRight(List.empty[Strait])((a, acc) => acc match {
      case x :: xs
        //　数字が隣りあってるならStraitになる
        if (x.minNum - 1 == a.num) => x.add(a) :: xs

      case _ => Strait(a :: Nil) :: acc
    })
    (straits map (_.minNum)).sum
  }
}

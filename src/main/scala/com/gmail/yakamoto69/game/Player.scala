package com.gmail.yakamoto69.game

import com.gmail.yakamoto69
import yakamoto69.scala._
import annotation.tailrec
import collection.mutable.ArrayBuffer

class Player(val name: String) {

  var numOfChips: Int = _
  val cards = ArrayBuffer[Card]()

  def pick(card: Card) {
    cards += card
  }

  def score: Int = scoreOfCards - numOfChips

  def info = {
    val cInfo = cards.map(_.num).sorted.mkString(" ")
    name+": score="+score+" chips="+numOfChips+" cards="+cInfo
  }

  private def scoreOfCards: Int = {

    /**
     * cardsは逆順に並んでいる e.g. (7 6 5)
     */
    case class Strait(cards: List[Card]) {
      require(!cards.isEmpty)

      def minNum: Int = cards.last.num
      def add(card: Card): Strait = Strait(card :: cards)
      def maxNum = cards.head.num
    }

    // こういうときはreverseするといいのか？
    val sorted = cards sortBy (_.num)

    /**
     * straitsは逆順に並んでいる e.g. (11 10) (7 6 5)
     */
    @tailrec
    def mkStraits(i: Int, straits: List[Strait] = Nil): Seq[Strait] = {
      if (sorted.length == i) return straits

      val card = sorted(i)
      straits match {
        //　数字が隣りあってるならStraitになる
        case head :: tail if (head.maxNum + 1 == card.num) =>
          mkStraits(i + 1, head.add(card) :: tail)

        case _ => mkStraits(i + 1, Strait(card :: Nil) :: straits)
      }
    }

    val straits = mkStraits(0)
    (straits map (_.minNum)).sum
  }
}

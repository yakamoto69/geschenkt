package com.gmail.yakamoto69.game.ui

import com.gmail.yakamoto69
import yakamoto69.game._
import yakamoto69.scala._
import android.view.View
import android.util.AttributeSet
import android.content.Context
import util.Random
import android.graphics.{Paint, Canvas}
import android.os.Handler
import com.twitter.conversions.thread.makeRunnable

class Field(context: Context, attrs: AttributeSet, defStyle: Int) extends View(context, attrs, defStyle) {
  self =>

  /**
   * これ必須だから消すな
   */
  def this(context: Context, attrs: AttributeSet) = this(context, attrs, 0)

  val console = new Ui {
    def promptToChoose(player: Player): Choice = {
      refresh()
      // todo ここでタッチできるようにする
      randomSelect(game.allOptions)
    }

    // val onRoundStart = refresh _   はなんで駄目なんだろう？
    def onRoundStart() { refresh() }

    def onRoundEnd() { refresh() }

    def onChosen(choice: Choice) { refresh() }

    def onEnd() { refresh() }
  }

  private def refresh() {
    handler.post(makeRunnable{
      invalidate()
    })
  }

  val paint = new Paint
  val handler = new Handler

  val players = {
    def initP[P <: Player](p: P): P = {
      p.numOfChips = 11
      p
    }

    def ai(i: Int) = initP(new Player("AI"+i) with Ai)
    def player: Player with Human = initP(new Player("Player") with Human {
      val ui = console
    })

    Seq(ai(4), ai(1), ai(2), ai(3))
//    Seq(player, ai(1), ai(2), ai(3)) todo ほんとはこっち
  }

  val game: InteractiveGame = {
    def initCards = {
      val all = util.Random.shuffle((3 to 35).toList)
      (all.map(Card) splitAt 9).swap
    }

    val game = new InteractiveGame(console, Random.shuffle(players)) // todo humanを先頭にしとかないと面倒だったのでそうした
//    val game = new InteractiveGame(console, Random.shuffle(players))
    val (backed, removed) = initCards
    game.backedCards = new FixedBackedCards(backed, removed)
    game
  }

  // onDrawで更新する領域。PlayerAreaとCenterAreaがある
  val allAreas: Seq[Area] = (game.players map mkPlayerArea) :+ mkCenterArea

  // 10 × 10 のグリッドで表現したときの、グリッドの縦横の大きさ
  var gridScale: Float = _


  private def mkPlayerArea(p: Player): Area = {
    val allPos = Seq(Bottom(), Left(), Top(), Right())
    val ixOfPlayer = game.players indexOf p
    /*
      先頭Humanってのに頼ってる。 ほんとは何回Humanから先かを求めて、Pos側で同じ回数進めて求めないといけない
      例えば AI1 -> Human -> AI3 -> AI2 -> AI1 ...　で、AI1はHumanから3つ先なのでRightになる
      これだけコメント書くぐらいなら実装すれよって思う
     */
    val pos = allPos(ixOfPlayer)
    new Area {
      def draw(board: Board): Drawing = {
        board.startDrawing(pos)
          .println(p.name)
          .println("score:"+p.score+" chips:"+p.numOfChips)
          .println(p.cInfo)
      }
    }
  }

  private def mkCenterArea: Area = new Area {
    def draw(board: Board): Drawing = {
      board.startDrawing(Center())
        .println("backed:"+game.backedCards.size)
        .println("faced:"+game.round.facedCard)
        .println("chips:"+game.numOfChipsOnBoard)
    }
  }

  override def onDraw(canvas: Canvas) {
    super.onDraw(canvas)

    val board = new Board {
      def startDrawing(pos: Pos) = new Drawing {
        val line = 0

        def draw(canvas: Canvas) {}

        def drawLine(canvas: Canvas, line: Int, text: String) {
          val (x, y) = computeCoordinate(pos, line)
          canvas.drawText(text, x, y, paint)
        }
      }
    }


    val drawings = allAreas map (_.draw(board))
    drawings foreach draw(canvas)
  }

  private def draw(canvas: Canvas)(drawing: Drawing) {
    drawing.draw(canvas)
  }

  override def onSizeChanged(w: Int, h: Int, oldw: Int, oldh: Int) {
    gridScale = math.min(w, h).toFloat / 10
  }


  private def computeCoordinate(pos: Pos, line: Int): (Float, Float) = {
    // 回転なし
    // とりあえず 10 × 10 のグリッドで考える
    val (gridX, gridY) = pos match {
      case Top() => (4, 0)
      case Right() => (7, 4)
      case Bottom() => (4, 7)
      case Left() => (0, 4)
      case Center() => (4, 4)
    }

    val toReal = (_:Int) * gridScale


    // +できるPositionか何かを定義するべき
    (toReal(gridX), toReal(gridY + line + 1))
  }
}


/*
Top -> Right -> Bottom -> Left -> Top ...
このように時計回りにローテートする
 */
sealed trait Pos
case class Top() extends Pos
case class Right() extends Pos
case class Bottom() extends Pos
case class Left() extends Pos
case class Center() extends Pos

trait Board {
  def startDrawing(pos: Pos): Drawing
}

trait Drawing {
  self =>

  def println(text: String): Drawing = new Drawing {
    def draw(canvas: Canvas) {
      self.draw(canvas)
      drawLine(canvas, self.line, text)
    }

    def drawLine(canvas: Canvas, line: Int, text: String) {
      // todo 無駄にラップしてるなあ
      self.drawLine(canvas, line, text)
    }

    val line = self.line + 1
  }

  def line: Int // lineだけじゃなくて場所を表すもの

  // todo drawLineできる何かを渡すようにするべき
  def draw(canvas: Canvas)

  def drawLine(canvas: Canvas, line: Int, text: String)
}

trait Area {
  def draw(board: Board): Drawing
}

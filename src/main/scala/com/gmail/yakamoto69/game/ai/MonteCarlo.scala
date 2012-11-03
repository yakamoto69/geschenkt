package com.gmail.yakamoto69.game.ai

import com.gmail.yakamoto69
import yakamoto69.scala._
import annotation.tailrec

class MonteCarlo {

  var tree: McTree = _

  def compute(world: McWorld): McChoice = {
    tree = new McTree(world)
    RepeatTask.run(1000, cycle())

    printResult()

    // この時点でchildrenがないわけないんだけどgetしちゃっていいのかな
    val best = tree.root.children.get maxBy (_.ucb)
    best.choice
  }

  private def printResult() {
    val children = tree.root.children.get
    val choices = children map (_.choice)
    val ucbs = children map (_.ucb)
    println("total:" + tree.root.total)
    println(choices.toList zip ucbs)
  }

  @tailrec
  private def select(node: McNode = tree.root): McNode = {
    // tailrecにするためgetOrElseを使えない
    /* こんな感じにすればネストしなくていいしわかりやすいんだけど、tailrecできなくなってしまう
    val hasChild = node.children filter (_.size > 0)
    def selectChild(children: Seq[McNode]): McNode = {
      val notSimulated = children find (!_.isSimulated)
      notSimulated getOrElse select(c maxBy (_.ucb))
    }
    hasChild.map(selectChild) getOrElse node
    */
    node.children match {
      case Some(children) if (children.size > 0) => {
        children find (!_.isSimulated) match {
          case Some(notSimulated) => notSimulated // playOutしていないノードが優先で選ばれる
          case None => select(children maxBy (_.ucb))
        }
      }
      case _ => node
    }
  }

  private def expand(node: McNode): McNode = {
    assert(node.children == None)

    val children = node.world.allChoices map { choice =>
      new ChildNode(tree, Some(node), node.world.proceed(choice), choice)
    }
    node.children = Some(children)
    if (children.size > 0) children(0) else node
  }

  private def simulate(node: McNode): McResult = {
    val win = node.world.playOut
    McResult(win)
  }

  @tailrec
  private def backPropagate(node: McNode, result: McResult) {
    node.addResult(result)
    node.parent match {
      case Some(p) => backPropagate(p, result)
      case None =>
    }
  }

  private def cycle() {
    // selectはツリーを探索するので、途中で状態が変わるとおかしいことになるのでsynchronizedする
    val selected = tree.synchronized {
      select()
    }

    // expand以外はsynchronizedから外せそうだけど深く考えてない
    val forSimulate = tree.synchronized {
      (selected.isSimulated, selected.children) match {
        case (false, _) => selected // まだplayOutしてないノードは、expandよりも前にまずplayOutする

        case (true, Some(c)) => {
          // expand済みなのにこのノードが選ばれるってことは終端ノードなはず
          assert(c.size == 0)
          // 既にexpandしているときは終端ノードをplayOutする
          selected
        }

        case (true, None) => expand(selected)
      }
    }

    // 2回連続でsimulateすることになっても別に問題ない。
    val result = simulate(forSimulate) // ここは絶対同期から外れてないといけない

    // ノードの値を更新するのでもちろんsynchronizedする
    tree.synchronized {
      backPropagate(forSimulate, result)
    }
  }

}

case class McResult(isWin: Boolean)

trait McNode {

  def tree: McTree
  def parent: Option[McNode]
  def world: McWorld

  def isSimulated: Boolean = total > 0

  var children: Option[Seq[ChildNode]] = None

  var win: Int = _
  var total: Int = _

  def addResult(r: McResult) {
    total += 1
    if (r.isWin) win += 1
  }

  // めんどいからdoubleでいいや
  def ucb: Double = {
    val xi = win.toDouble / total.toDouble
    val n = tree.root.total.toDouble
    val ni = total.toDouble
    val c = 1.0

    xi + c * math.sqrt(math.log(n) / ni)
  }
}

class RootNode(val tree: McTree, val world: McWorld) extends McNode {
  val parent = None
}

class ChildNode(val tree: McTree, val parent: Some[McNode], val world: McWorld,
                val choice: McChoice) extends McNode


class McTree(world: McWorld) {
  val root = new RootNode(this, world)
}

/**
 * もちろんimmutableだぞ！
 */
trait McWorld {
  def allChoices: Seq[McChoice]

  def proceed(choice: McChoice): McWorld

  def playOut: Boolean  // win/lose
}

trait McChoice
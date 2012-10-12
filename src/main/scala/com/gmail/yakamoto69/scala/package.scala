package com.gmail.yakamoto69

package object scala {
  def ??? = throw new UnsupportedOperationException

  def generator(start: Int): () => Int = {
    var i = start
    () => {
      val num = i
      i += 1
      num
    }
  }
}

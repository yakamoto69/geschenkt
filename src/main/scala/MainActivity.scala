package com.gmail.yakamoto69.game

import _root_.android.app.Activity
import _root_.android.os.Bundle
import scala.actors.Actor._

class MainActivity extends Activity with TypedActivity {
  override def onCreate(bundle: Bundle) {
    super.onCreate(bundle)
    setContentView(R.layout.main)

    actor {
      findView(TR.field).game.start()
    }
  }
}

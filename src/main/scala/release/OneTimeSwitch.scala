package release

import java.util.concurrent.atomic.AtomicBoolean

class OneTimeSwitch {
  private val v = new AtomicBoolean(false)

  def trigger(): Unit = {
    v.set(true)
  }

  def isTriggered(): Boolean = v.get()
}

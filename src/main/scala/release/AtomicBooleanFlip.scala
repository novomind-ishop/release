package release

import java.util.concurrent.atomic.AtomicBoolean

class AtomicBooleanFlip {
  private val v = new AtomicBoolean(false)
  def set(): Unit = {
    v.set(true)
  }
  def get():Boolean = v.get()
}

package release

import java.awt.{Point, Rectangle}
import javax.swing.JFrame

object Splash {

  def show(): Unit = {

    val f = new JFrame()
    f.setSize(400, 150)
    f.setTitle("release")
    f.setVisible(true)
    f.setResizable(false)
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    case class DisplayPos(name: String, down: Double, left: Double)
    object DisplayPos {
      val tr = DisplayPos("topRight", 0.1, 0.9)
      val tl = DisplayPos("topLeft", 0.1, 0.1)
      val br = DisplayPos("bottomRight", 0.9, 0.9)
      val bl = DisplayPos("bottomLeft", 0.9, 0.1)
    }

    def location(size: Rectangle, displayPos: DisplayPos): Point = {
      val moveDown = displayPos.down
      val moveLeft = displayPos.left
      Some(java.awt.Toolkit.getDefaultToolkit.getScreenSize)
        .map(in ⇒ (in.height, in.width))
        .map(in ⇒ in.copy(_1 = in._1 - size.height))
        .map(in ⇒ in.copy(_2 = in._2 - size.width))
        .map(in ⇒ in.copy(_1 = (in._1 * moveDown).toInt, (in._2 * moveLeft).toInt))
        .map(in ⇒ {
          val x: Int = in._2
          val y: Int = in._1
          new Point(x, y)
        }).get
    }

    f.setLocation(location(f.getBounds, DisplayPos.br))
    println("done")
  }

}

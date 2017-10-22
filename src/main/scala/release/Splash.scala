package release

import java.awt.{Image, Point, Rectangle, TrayIcon}
import javax.swing.{JFrame, UIManager}

object Splash {

  def show(): Unit = {
    tray()
    System.setProperty("apple.laf.useScreenMenuBar", "true")
    System.setProperty("com.apple.mrj.application.apple.menu.about.name", "WikiTeX")
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    val f = new JFrame()
    f.setSize(400, 150)
    f.setTitle("release")
    f.setVisible(true)
    f.setResizable(false)
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

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

  def tray(): Unit = {
    import javax.swing.ImageIcon

    def loadImage(f: String): Image = {
      val url = getClass.getClassLoader.getResource(f)
      new ImageIcon(url).getImage
    }

    val systemTray = java.awt.SystemTray.getSystemTray
    val image = loadImage("foo.png")

    val icon = new TrayIcon(image)
    systemTray.add(icon)
  }

  case class DisplayPos(name: String, down: Double, left: Double)

  object DisplayPos {
    val tr = DisplayPos("topRight", 0.1, 0.9)
    val tl = DisplayPos("topLeft", 0.1, 0.1)
    val br = DisplayPos("bottomRight", 0.9, 0.9)
    val bl = DisplayPos("bottomLeft", 0.9, 0.1)
  }

}

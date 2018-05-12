import java.awt.Toolkit
import java.io.File

import javax.swing.{JFrame, JPanel, JScrollPane, JTextArea}

import scala.swing._

object Main extends App{
  val pathToResources = System.getProperty("user.dir") + "/src/main/resources/"
  val mainWindow = new JFrame()
  mainWindow.setLayout(null)
  mainWindow.setTitle("Model STP")
  mainWindow.setBounds(250, 100, 850, 500)
  mainWindow.setIconImage(Toolkit.getDefaultToolkit.getImage(pathToResources + "scalaLogo.jpeg"))
  mainWindow.add(buildTextArea(isEditable = true))


  def buildTextArea(x: Int = 200, y: Int = 100, width: Int = 300, height: Int = 200,
                    wrapLines: Boolean = true, isEditable: Boolean = false): JScrollPane = {
    val textArea = new JTextArea()
    textArea.setLineWrap(wrapLines)
    textArea.setEditable(isEditable)
    val scPane = new JScrollPane(textArea)
    scPane.setBounds(x, y, width, height)
    scPane
  }

  mainWindow.setVisible(true)
}



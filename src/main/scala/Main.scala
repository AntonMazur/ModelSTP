import java.awt.{Image, Toolkit}

import akka.actor.{Props, ActorRef, ActorSystem}
import Properties._

import scala.collection.mutable.Set
import javax.swing.{JTextArea, _}

import scala.concurrent.Future
import scala.util.{Random}
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App{
//  runSTP(mockData())

  def runSTP(adjMatr: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val system = ActorSystem("System")
    val nodes = Set[ActorRef]()
    val nodeIDs = generateSetOfInts(adjMatr.length)
    for (i <- 0 until adjMatr.length) {
      var nbhrs = Seq[Int]()
      for (j <- 0 until adjMatr(0).length if i != j && adjMatr(i)(j))
        nbhrs = nbhrs :+ nodeIDs(j)
      Future { nodes.add(system.actorOf(Props(Node(nodeIDs(i), nbhrs)))) }
      Thread.sleep(2000)
    }

    Node.run(array2map(nodeIDs))
  }

  def array2map(arr: Array[Int]): Map[Int, Int] = {
    (for (i <- 0 until arr.length) yield (arr(i) -> i)).toMap
  }

  def buildUI(): JFrame = {
    def buildTextArea(x: Int = 200, y: Int = 100, width: Int = 350, height: Int = 350,
                      wrapLines: Boolean = true, isEditable: Boolean = false): (JScrollPane, JTextArea) = {
      val textArea = new JTextArea()
      textArea.setLineWrap(wrapLines)
      textArea.setEditable(isEditable)
      textArea.setFont(textArea.getFont().deriveFont(16f))
      val scPane = new JScrollPane(textArea)
      scPane.setBounds(x, y, width, height)
      (scPane, textArea)
    }

    def loadImage(path: String): Image = Toolkit.getDefaultToolkit.getImage(pathToResources + path)

    def buildButton(x: Int = 150, y: Int = 100, width: Int = 150, height: Int = 100,
                    text: String = null, pathToIcon: String = null): JButton = {
      val button = if (pathToIcon == null) new JButton(text) else new JButton(new ImageIcon(loadImage(pathToIcon)))
      button.setBounds(x, y, width, height)
      button
    }

    val mainWindow = new JFrame()
    mainWindow.setLayout(null)
    mainWindow.setTitle("Model STP")
    mainWindow.setBounds(250, 100, 1050, 700)
    mainWindow.setIconImage(loadImage("scalaLogo.jpeg"))
    val (inputSP, inputTA) = buildTextArea(x = 30, y = 50, isEditable = true)
    mainWindow.add(inputSP)
    val (outSP, logTA) = buildTextArea(x = 400, y = 30, width = 600, height = 600)
    mainWindow.add(outSP)
    val startButton = buildButton(y = 500, pathToIcon = "play-button.jpg")
    Node.setLogger(logTA)
    startButton.addActionListener(event => {
      val result = Future {
        runSTP(readAdjMatrix(inputTA.getText split ("\n") map (_.split(" "))))
      }
      result foreach (result => {
          val binRes: Array[Array[Int]] = result map (row => row map (bool => if (bool) 1 else 0))
          Node.log(binRes.map(row => row.mkString(" ")).mkString("\n"))
        });
    });
    mainWindow.add(startButton)
    mainWindow
  }

  def readAdjMatrix(lines: Array[Array[String]]): Array[Array[Boolean]] = {
    val n = lines.length
    val adjMatr: Array[Array[Boolean]] = Array.ofDim(n, n)
    for (i <- 0 until n; j <- 0 until n if (j != i)) {
      adjMatr(i)(j) = if (lines(i)(j) == "1") true else false
    }
    adjMatr
  }

  def mockData(): Array[Array[Boolean]] ={
    Array[Array[Boolean]](
      Array(false, true, true, true, true),
      Array(true, false, true, true, true),
      Array(true, true, false, true, true),
      Array(true, true, true, false, true),
      Array(true, true, true, false, true))
  }

  def generateSetOfInts(size: Int): Array[Int] = {
    val intSet = Set[Int]()
    while (intSet.size != size)
      intSet.add(Random.nextInt().abs % 1000)
    intSet.toArray

  }

  buildUI().setVisible(true)
}



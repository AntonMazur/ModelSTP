import java.util.Calendar

import akka.actor.Actor

import collection.mutable.Queue
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class Node(val id: Int, val nbhrsIndexes: Seq[Int]) extends Actor{
  private var nbhrs: Queue[Node] = Queue()
  private var sendingId: Int = id;
  private var rootNode: Int = id;

  def addNbhr(nbhr: Node) = nbhrs.enqueue(nbhr)

  def sendPackageToNeighbours: Unit= {
      nbhrs.foreach(node => {
        log(s"Send package(retranslation id = ${sendingId}) to node_${node.id}")
        node.self ! buildMsg()
      })
  }

  override def receive: Receive = {
    case Message(senderId, retranslId) =>
      if (retranslId > sendingId) {
        log(s"Received message from $senderId(retranslation id = $retranslId), changed retranslation id on $retranslId")
        sendingId = retranslId
        rootNode = senderId
      }
      else {
        log(s"Received message from $senderId(retranslation id = $retranslId)")
      }
    case _ => log(s"Some bastard sent me bullshit")
  }

  def removeRedundantNodes(): Unit =  nbhrs.dequeueAll(_.id != rootNode)

  def log(msg: String) = Node.log(id, msg)

  def buildMsg():Message = { Message(id, sendingId) }

  override def toString: String = s"Node #$id"

  Node.addNode(this)
}
object Node{
  private var nodes: Queue[Node] = Queue()

  private var logger: {def append(value: String) : Unit} = new AnyRef{def append(v: String) = println(v)}

  def setLogger(logger: {def append(value: String) : Unit}) = Node.logger = logger

  def log(id: Int, msg: String): Unit = synchronized {logger.append(s"[${Calendar.getInstance().getTime}]  [Node_$id]: " + msg + "\n\n")}

  def log(msg: String): Unit = logger.append(msg + "\n\n")

  def addNode(node: Node) = {nodes.enqueue(node)}

  def resolveNeighboursIndexes(): Unit ={
    val copyNodes = nodes.clone()
    nodes foreach (node => node.nbhrsIndexes foreach (nIdx => node addNbhr getIf(copyNodes, n => n.id == nIdx)))
    println("Indexes resolving finished")
  }

  def getIf(nodes: Queue[Node], predicate: Node => Boolean) = {
    var result: Node = null
    nodes.foreach(n => if (predicate(n)) result = n)
    result
  }

  def run(idNumPairs: Map[Int, Int]): Array[Array[Boolean]] = {
    resolveNeighboursIndexes()
    for (i <- 1 until nodes.size)
      Node.runPackageSending()
    logRootNode()
    turnOffRedundantNodes()

    val optNetwork: Array[Array[Boolean]] = Array.ofDim(nodes.length, nodes.length)
    for (node <- nodes; nbhr <- node.nbhrs){
      optNetwork(idNumPairs.getOrElse(node.id, -1))(idNumPairs.getOrElse(nbhr.id, -1)) = true
      optNetwork(idNumPairs.getOrElse(nbhr.id, -1))(idNumPairs.getOrElse(node.id, -1)) = true
    }
    dispose
    optNetwork
  }

  def runPackageSending(): Unit = {
    nodes.foreach(node => Future { node.sendPackageToNeighbours })
    Thread.sleep(5000)
  }

  def dispose(): Unit = {
    nodes.clear()
  }

  def logRootNode() = nodes.foreach(node => if (node.sendingId == node.id) log(node.id, "this node acknowledged as root"))

  def turnOffRedundantNodes() = nodes.foreach(_.removeRedundantNodes())

  def apply(id: Int, nbhrsIndexes: Seq[Int]) = new Node(id, nbhrsIndexes)
}
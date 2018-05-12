import akka.actor.Actor
import collection.mutable.Queue


class Node(id: Int, nbhrsIndexes: Seq[Int]) extends Actor{
  private var nbhrs: Queue[Node] = Queue()

  private var stop: Boolean = false

  def addNbhr(nbhr: Node) = nbhrs.enqueue(nbhr)

  override def toString: String = s"Node #$id"

  override def receive: Receive = ???

  def start: Boolean = {
    Node.nodes
  }

  Node.addNode(this)




  object Node{
    private var nodes: Queue[Node] = Queue()

    def addNode(node: Node) = {nodes.enqueue(node)}

    def shutDownNodes() = nodes.foreach(_.stop = true)

    def resolveNeighboursIndexes(): Unit ={
      nodes foreach (node => nbhrs foreach (nbhr => if (node.nbhrsIndexes.contains(nbhr.id)) node addNbhr nbhr))
    }


  }
}

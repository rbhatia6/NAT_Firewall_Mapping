import scala.collection.mutable.HashMap
import util.control.Breaks._

object NATFirewall {
  var natMap = new HashMap[String, (String, Integer)]()
  var port = 1025

  def IPMatch(x: String) : String = x match {
    case "10" => "local"
    case "53" => "public"
    case _ => "external"
  }

  def getPacket(sourceIP: String, sourcePort: String, destIP: String, destPort: String): Unit = {
    val sourceIPPort = sourceIP + ":" + sourcePort
    val destIPPort = destIP + ":" + destPort

    val sourceIPtype = IPMatch(sourceIP.split('.')(0))
    val destIPtype = IPMatch(destIP.split('.')(0))

    if (sourceIPtype == "local" && destIPtype == "local") 
        println("%s to %s %s".format(sourceIPPort, destIPPort, "local"))
    else if (sourceIPtype == "local" && destIPtype == "external") {
      var found = false
      breakable {
        for ((k,v) <- natMap) {
          if ((k == sourceIPPort) && (v._1 == destIPPort)) {
            found = true
            break
          }
        }
      }
      if (!found) {
        natMap += (sourceIPPort -> (destIPPort, port))
        println("Mapped %s to %s assigned %d".format(sourceIPPort, destIPPort, port))
        port += 1
      }
    }
    else if ((sourceIPtype == "external") && (destIPtype == "external")) {
      var found = false
      var redirectPort = ""
      breakable {
        for ((k,v) <- natMap) {
          if (v._1 == sourceIPPort && k.split(":")(1) == destPort) {
            found = true
            redirectPort = k
            break
          }
        }
      }
      if (!found) {
        println("Accept %s %s redirect %s".format(sourceIPPort, destIPPort, redirectPort))
      } 
      else
        println("Reject %s %s".format(sourceIPPort, destIPPort))
    }
  }
 
  def main(args: Array[String]): Unit = {
    //Enter your code here. Read input from STDIN. Print output to STDOUT
    val sourceIP = args(0)
    val sourcePort = args(1)
    val destIP = args(2)
    val destPort = args(3)

    getPacket(sourceIP, sourcePort, destIP, destPort) 
  } 
}

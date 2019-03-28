import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Queue

object GS {

  def main(args: Array[String]): Unit = {
    val n: Integer = scala.io.StdIn.readLine.toInt
    println("n = " + n)

    var lines = new ListBuffer[Vector[Int]]()

    for (i <- (0 until 2*n)) {
      var line = scala.io.StdIn.readLine
      lines += line.split(" ").toVector.map(_.toInt)
    }

    //Kvinna -> [Preferenser]
    val women: Map[Int, Vector[Int]] = lines.take(n).map(line => (line.head -> invert(line.tail))).toMap
    val men: Map[Int, Vector[Int]] = lines.drop(n).map(line => (line.head -> line.tail)).toMap

    println("Content of Men: " + men)
    println("Content of Women: " + women)

    printSolution(solve(women, men))
  }

  def printSolution (pairs: Map[Int, Int]): Unit = {
    pairs.foreach(p => println(p))
  }

  def invert (pref: Vector[Int]): Vector[Int] = {
    pref.zip(pref.length to 1 by -1).sortBy(_._1).map(_._2)
  }

  def solve(women: Map[Int, Vector[Int]], 
            men: Map[Int, Vector[Int]]): Map[Int, Int] = {

    var men2: Map[Int, Vector[Int]] = men
    var unmarriedMen = Queue[Int]()
    men2.foreach(m => unmarriedMen.enqueue(m._1))

    println("unmarriedMen: " + unmarriedMen)
    println("Men2: " + men2)

    var pairs = new HashMap[Int, Int]()

    while (!unmarriedMen.isEmpty) {
      val proposingMan: Int = unmarriedMen.dequeue._1
      val woman: Int = men2(proposingMan).head
      val currentMan = pairs(woman)

      if (!(pairs contains woman)) {
        pairs += (woman -> proposingMan)
      } else if (women(woman)(proposingMan) > currentMan) {
        pairs += (woman -> proposingMan)
        unmarriedMen enqueue currentMan
      } else {
        unmarriedMen enqueue proposingMan
      }
      println("End of while")
      men2 += proposingMan -> men2(proposingMan).tail
    }
    pairs.toMap
      /*
        remove proposing man from unmarriedMen
        if(w has no partner) 
          they become a pair
        else if (w prefers m over current partner) 
          remove her current pair
          (m, w) becomes new pair
          add old partner to unmarried list
        else 
          add proposing man back to unmarriedMen
        
        modify man so that he has proposed to this woman
        call solve(women, men, unmarried men)

      */
  }
}
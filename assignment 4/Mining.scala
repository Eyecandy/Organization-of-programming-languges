import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source


object Mining extends  App{

  def isAllDigits(x: String) = x forall Character.isDigit
  def isDelay(line:String,map: mutable.HashMap[String,(Int,Int)]) = {
    val lstLine = line.split(",")
    val flightCode = lstLine(8)
    val delay = lstLine(14)
    if (isAllDigits(delay) && !delay.equals("0")) {
      if (map.contains(flightCode)) {
        val tup: (Int, Int) = map(flightCode)
        map.put(flightCode,(tup._1 +1, tup._2 + 1))
      }
      else {
        map.put(flightCode,(1,1))
      }
    }
    else {
      if (map.contains(flightCode)) {
        val tup = map(flightCode)
        map.put(flightCode,(tup._1 , tup._2 + 1))
      }
      else {
        map.put(flightCode,(0,1))
      }
    }

    }

  def groupWork(lines:Seq[String]) = {
    val map = new mutable.HashMap[String,(Int,Int)]
    for (elem <- lines) {isDelay(elem,map)}
    //println(map)
    map
  }

  def onTimeRank(fileName: String): Vector[(String, Double)] = {
    val lines = Source.fromFile(fileName).getLines().drop(1).
      grouped(1 << 14).
      map(groupedLines => Future {
        groupWork(groupedLines)
      })

    val totalFut = Future.sequence(lines).map(c => c)

    val tot = Await.result(totalFut,Duration.Inf)
    val combinedMap = new mutable.HashMap[String, (Int, Int)]

    while (tot.hasNext) {
      val map: mutable.Map[String, (Int, Int)] = tot.next()
      map.foreach(key => {
        if(combinedMap.contains(key._1)) {
          val tup: (Int, Int)= combinedMap(key._1)
          combinedMap.put(key._1, (tup._1 + key._2._1 , tup._2 + key._2._2) )
        }
        else {
            combinedMap.put(key._1,(key._2._1, key._2._2) )
        }

      })
    }

    var sum = 0
    val perCombinedMap = new mutable.HashMap[String, Double]
    combinedMap.foreach(x => {
      sum += x._2._2
      perCombinedMap.put(x._1, 100*(1 - (x._2._1 / x._2._2.toDouble)))
    }

    )

    perCombinedMap.toVector.sortBy(x => -x._2)
  }

 //println(onTimeRank("/Users/joakimnilfjord/Desktop/2008.csv").length)

}

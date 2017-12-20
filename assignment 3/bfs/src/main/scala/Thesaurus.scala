
object Thesaurus extends App {
  import scala.io.Source

  val defaultEncoding = "ISO8859-1"

  def load(filename: String):Map[String,Set[String]] =  {
    val lines =  Source.fromFile(filename,defaultEncoding).getLines.drop(1).toList
    val thesaurusMap: Map[String,Set[String]] = loopStem(lines,Map())
    thesaurusMap
  }

  def loopStem(lines:List[String], mapRet:Map[String,Set[String]]):Map[String,Set[String]] = lines match {
    case Nil => mapRet
    case  line::t => {
      if (line.isEmpty) mapRet
      else {
        val arr = line.split("\\|")
        val (s: String, ng: String) = (arr.head, arr.tail.head)
        val stem: String = s.trim()
        val numberOfGroups = ng.trim().toInt
        val stemWords: Set[String] = t.take(numberOfGroups).flatMap(x => x.split("\\|").tail).toSet
        val trimmedStemWords = stemWords.map(x => x.trim)
        val newLines = t.drop(numberOfGroups)
        loopStem(newLines, mapRet + (stem -> trimmedStemWords))
      }
    }
  }

  def linkage(thesaurusFile: String): String => String => Option[List[String]] = {
    val internDB:Map[String,Set[String]]= load(thesaurusFile)

    def nbrs(wordA:String): Set[String]= {
      if (internDB.contains(wordA)) internDB(wordA)
      else Set()
    }
    def shortPathFromA(wordA:String): String => Option[List[String]] = {
      val (spt,hops):(Map[String,String], Map[String, Int]) = GraphBFS.bfs(nbrs,wordA)

      def findPathFromBtoA(wordB:String):Option[List[String]] = {

        def helper(wordB:String,lst:Option[List[String]]): Option[List[String]] = {
          if (spt.contains(wordB) && wordB.equals(wordA)) Some(wordA::lst.get)
          else if (spt.contains(wordB))  {
            val nextWord:String= spt(wordB)
            helper(nextWord, Some(wordB::lst.get))
          }
          else None
        }
        helper(wordB,Some(List()))
      }
      findPathFromBtoA
    }
    shortPathFromA
  }
}

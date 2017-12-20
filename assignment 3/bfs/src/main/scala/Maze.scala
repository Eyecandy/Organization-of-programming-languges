object Maze extends App{
  def solveMaze(maze: List[String]): Option[String] = {
    val strMaze:String = maze.foldLeft("") ((acc,elt) => acc + elt)
    val row:Int= maze.head.length
    val startingPoint:Int = strMaze.indexOf("s")//findPoint(strMaze,0,'s');
    val endPoint:Int = strMaze.indexOf("e")

    def nbrs(i:Int): Set[Int] = {
      Set(i+1,i-1,i+row,i-row).filter(h => strMaze.charAt(h) != 'x')
    }
    val (shortPathMap, hopsAway) = GraphBFS.bfs(nbrs,startingPoint)

    def findWay( currPos:Int,direction:String): Option[String] = {
      val (left,right,down,up) = (currPos + 1,currPos -1, currPos -row,currPos + row)
      val ncp = shortPathMap.get(currPos).get
      if (currPos == startingPoint) Some(direction)
      else if (ncp == right)  findWay( ncp, "r" + direction )
      else if (ncp == left) findWay( ncp,"l" + direction )
      else if (ncp == down ) findWay( ncp, "d" + direction )
      else findWay(ncp,"u" + direction )
    }
    if (shortPathMap.contains(endPoint)) (findWay(endPoint,"")) else None
  }

}

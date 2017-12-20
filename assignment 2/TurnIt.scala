object TurnIt extends App {
	
	def transpose(A: List[List[Int]]): List[List[Int]] = {
		def makeRows(A: List[List[Int]], NA:List[List[Int]], row: List[Int], AT: List[List[Int]]): List[List[Int]] = {
			if (A.isEmpty && NA.head.isEmpty) row.reverse::AT
			else{
				if (A.isEmpty ) makeRows(NA.reverse, List(),List(),row.reverse:: AT) 
				else makeRows(A.tail, A.head.tail::NA, A.head.head::row,AT) 
			}
		}
		makeRows(A,List(),List(),List()).reverse
		
	}
}

object Zombies extends App {
	def countBad(hs: List[Int]): Int = {
		mergeSort(hs,0)._2
	}
	def splitAt(hs:List[Int],k:Int): (List[Int],List[Int]) = {
		if (k == 0) (List(),hs)
		else { 	
			val (a,b) = splitAt(hs.tail,k-1)
			(hs.head::a,b)
		}
	}
	def mergeSort(hs: List[Int],count:Int): (List[Int] ,Int) = {
		val n =	hs.length
		val l = n / 2
		if (n <= 1) (hs,count)
		else {
			val (left, right) = splitAt(hs,l)
			val (ml,cl) = mergeSort(left,count)
			val (mr,cr) = mergeSort(right,count)
			merge(ml,mr, cl + cr)
		}
	}

	def merge(left: List[Int], right: List[Int],count:Int): (List[Int],Int) =  {
		if (left.isEmpty) (right,count)
		else if (right.isEmpty) (left,count)
		else {
			if (left.head < right.head) {
				val (ml,cl) = merge(left.tail,right,count + right.length)
				(left.head::ml, cl)
			}
			else { 
				val (mr,cr) = merge(left, right.tail,count) 
				(right.head::mr,cr)
			}
		}
	}
}

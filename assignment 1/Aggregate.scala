object Aggregate extends App {
	def myMin(p: Double, q: Double, r: Double): Double  = {
		if (p <= q && p <= r) p
		else if (q <= p && q <= r) q
		else r
	}
	def myMean(p: Double, q: Double, r: Double): Double = {
		(p+q+r)/3.0
	}
	def myMed(p: Double, q: Double, r: Double): Double = {
		if ( (p >= q && p <= r) || (p >= r && p <= q) ) p
		else if ( (q >= p && q <= r) || (q >= r && q <= p) )  q 
		else r  
	}
}



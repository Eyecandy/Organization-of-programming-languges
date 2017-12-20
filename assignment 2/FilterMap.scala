object FilterMap extends App {
	def map[A, B](f: A => B, xs: List[A]): List[B] = {
		def helper(xs:List[A]): List[B] = xs match {
			case List() => List()
			case h::t => f(h)::helper(t)
		}
		helper(xs)
	}
	def filter[A](p: A => Boolean, xs: List[A]): List[A] = {
		def helper(xs:List[A]): List[A] = xs match {
				case List() => List()
				case h::t if p(h) => h :: helper(t)
				case h::t => helper(t)
		}
		helper(xs)
	}
}

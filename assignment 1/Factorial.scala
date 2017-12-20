object Factorial extends App{
	def factorial(n: Int): Long = {
		if (n <= 1) 1
		else n * factorial(n-1)
	}	
}
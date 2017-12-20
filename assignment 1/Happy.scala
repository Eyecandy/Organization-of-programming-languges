object Happy extends App {
	def sumOfDigitsSquared(n: Int): Int = {
		if (n/10==0) n*n
		else (n%10)*(n%10) + sumOfDigitsSquared(n/10)
	}
	def isHappy(n: Int): Boolean = {
		if (n == 1) true
		else if (n == 4 ) false
		else isHappy(sumOfDigitsSquared(n))
	}
	def helper(k:Int, n: Int): Int = {
		if (k==0) n
		else if (isHappy(n) == true && k != 1) helper(k-1,n+1)
		else if (isHappy(n) == true) helper(k-1,n)
		else helper(k,n+1)
	}

	def kThHappy(k: Int): Int = {
		helper(k,1)
	}
}
object Roman extends App {
	
	def toRoman(n: Int): String = {
		"M"*(n/1000) + makeRoman((n/100)%10,"C","D","M") + makeRoman((n/10)%10,"X","L","C") +  makeRoman(n%10,"I","V","X")
	}
	def makeRoman(n:Int,low: String, middle: String, high: String): String = {
		if (n == 9) low+high
		else if (n <= 8 && n >= 4) low*(4/n)+ middle + low*(n/6)*(n%5)
		else low*n
	}
}
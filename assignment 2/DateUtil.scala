object DateUtil extends App {
  type Date = (Int, Int, Int)

  def isOlder(x: Date, y: Date): Boolean =  {
    if (x._3 != y._3) x._3 < y._3
    else if (x._2 != y._2) x._2 < y._2
    else x._1 < y._2
  }
  def numberInMonth(xs: List[Date], month: Int): Int = xs match {
    case List() => 0
    case (_,`month`,_) :: t => 1 + numberInMonth(t,month)
    case h::t => numberInMonth(t,month)
  }
  def numberInMonths(xs: List[Date], months: List[Int]): Int = months match  {
    case List() => 0
    case h::t => numberInMonth(xs,h) + numberInMonths(xs,t)
  }

  def datesInMonth(xs: List[Date], month: Int): List[Date] = {
    if (xs.isEmpty) List()
    else if (xs.head._2 == month) xs.head::datesInMonth(xs.tail,month)
    else datesInMonth(xs.tail,month)
  }
  def dateInMonthsHelper(months: List[Int],xMonth:Int): Boolean = months match {
    case List() => false
    case `xMonth`::t => true
    case h::t => dateInMonthsHelper(t,xMonth)
  }
  def datesInMonths(xs: List[Date], months: List[Int]): List[Date] = {
    if (xs.isEmpty) List()
    else if (dateInMonthsHelper(months,xs.head._2)) {xs.head::datesInMonths(xs.tail,months)}
    else datesInMonths(xs.tail,months)
  }

  def dateToString(d: Date): String = {
    val months: List[String] = List("January","February","March","April","May","June","July","August","September","October","November","December")
    
    def getStringFromMonthList(iter:Int, months: List[String]): String =  {
      if (iter == d._2) months.head
      else getStringFromMonthList(iter + +1, months.tail)
    }

    getStringFromMonthList(1,months) + "-" + d._1 + "-" +d._3

  }

  def whatMonth(n: Int, yr: Int): Int =  {
    val normalYear = List(31,28,31,30,31,30,31,31,30,31,30,31)
    val leapYear = List(31,29,31,30,31,30,31,31,30,31,30,31)

    def findMonth(n:Int,daysOfMonth: List[Int]): Int = {
      val dayOfOfCurMonth = daysOfMonth.head
      if (n <= dayOfOfCurMonth) return 1
      else 1 + findMonth(n-dayOfOfCurMonth, daysOfMonth.tail)
    }

    if ( ( yr %4 == 0  || yr%400 == 0) && yr%100 != 0 )  findMonth(n,leapYear)
    else findMonth(n,normalYear)
  }


  def oldest(dates: List[Date]): Option[Date] =  {
    def helper(dates: List[Date],date: Date): Date = dates match {
      case List() => date
      case h::t => if ( isOlder(h,date) ) helper(t,h) else helper(t,date)
    }

    dates match {
      case List() => None
      case h::t => Some(helper(t,h))}
    }

    def isReasonableDate(d: Date): Boolean = {
      val normalYear = List(31,28,31,30,31,30,31,31,30,31,30,31)
      val leapYear = List(31,29,31,30,31,30,31,31,30,31,30,31)

      def findNdaysOfMonth(daysOfMonth: List[Int], iter:Int): Int = {
        if (iter== d._2) daysOfMonth.head
        else findNdaysOfMonth(daysOfMonth.tail,iter+1)

      }
      def findOut(d:Date,daysOfMonth: List[Int]): Boolean = {
        if (d._1 <= 0 || d._2 <= 0 || d._3 <= 0) return false
        if (d._2 > 12 ) return false
        if (d._1 > findNdaysOfMonth(daysOfMonth,1)) return false
        true
      }
      val yr = d._3
      if ( ( yr %4 == 0  && yr%100 != 0) || yr%400 == 0  ) findOut(d,leapYear)
      else findOut(d,normalYear)
    }
  }

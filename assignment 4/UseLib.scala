object UseLib  extends App{

  def onlyBeginsWithLower(xs: Vector[String]): Vector[String]  ={
    xs.filter(x =>  x.nonEmpty && x.head.isLower)
  }

  def longestString(xs: Vector[String]): Option[String] ={
    if (xs.isEmpty) {
      None
    }
    else {
      Some((xs.maxBy(x => x.length)))
    }
  }
  def longestLowercase(xs: Vector[String]): Option[String] = {
    longestString(onlyBeginsWithLower(xs))
  }
  /*
  val v2 = Vector("","")
  val v = Vector("hi","HI","iH","Hi","lal","","okildadN")
  println("only begins with lower")
  println(onlyBeginsWithLower(v))
  println(onlyBeginsWithLower(v2))
  println("LOngest string")
  println(longestString(v2))
  println(longestString(v))
  println("longest lowercase")
  println(longestLowercase(v2))
  println(longestLowercase(v))
  */




}

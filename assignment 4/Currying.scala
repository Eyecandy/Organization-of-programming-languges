object Currying extends App{
  case object NoAnswer extends Exception
  def firstAnswer[A,B]( func: A => Option[B] ):  List[A] => B = {
    (l: List[A]) =>  {
      val filtered: List[A]= l.filter(elt => func(elt).isDefined)
       filtered.length match {
         case 0 => throw NoAnswer
         case _ => func(filtered.head).get
       }
     }
  }

  def allAnswers[A,B]( func:A => Option[List[B]] ): List[A] => Option[List[B]] = {
    (l:List[A]) => {
      val filtered:List[A] = l.filter(x => func(x).isEmpty)
      filtered.isEmpty match {
        case true => Some(l.flatMap(a => func(a)).flatten)
        case false => None
      }
    }
  }

  /*
  def g(someInt:Int): Option[List[String]] = {
    if (someInt > 2 && someInt < 100) {Some(List(someInt.toString))}
    else None
  }
  def f(someInt:Int ):Option[String] = {
    if (someInt == 5) {Some("exists")}
    else {None}
  }
  println(firstAnswer(f)(List(1,2,3,4,5)))
  println(allAnswers(g)(List(1,3,4,5,6,7,8)))
  */

}

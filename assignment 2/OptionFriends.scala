object OptionFriends extends App {
  
  def lookup(xs: List[(String, String)], key: String): Option[String] = xs match {
      case Nil => None
      case (`key`,v) :: t => Some(v)
      case h::t => lookup(t,key)
  }

  def resolve(userIdFromLoginName: String => Option[String],
              majorFromUserId: String => Option[String],
              divisionFromMajor: String => Option[String],
              averageScoreFromDivision: String => Option[Double],
              loginName: String): Double = {
    
    val score: Option[Double] = userIdFromLoginName(loginName)
      .flatMap(majorFromUserId)
      .flatMap(divisionFromMajor)
      .flatMap(averageScoreFromDivision)
    
    score.getOrElse(0.0)
  }
}


  

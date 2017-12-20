
object Spaghetti extends  App{

  def spaghetti: Stream[String] = {
    def speg(string: String): Stream[String] = string #:: speg(nextSeq(string.tail, string.head, 1))
      def nextSeq(input:String,preChar:Char,count:Int):String= {
        input.isEmpty match {
          case true => {count.toString() + preChar}
          case false => {
            if (input.head == preChar) {
              nextSeq(input.tail,preChar,count+1)
            }
            else {
              count.toString + preChar + nextSeq(input.tail,input.head,1)
            }
          }
        }
      }
    speg("1")
  }


  def ham: Stream[String] = {

    def curr(input:List[String]):Stream[String] = {
      input.toStream#:::curr(nextSeq(input))
    }

    def nextSeq(lst:List[String]):List[String] = {
      val nwlst  = lst.map(x => '0'+x)
      val revlst: List[String] = lst.reverse.map(x => '1'+x)
      val ret = nwlst ::: revlst
      ret
    }
    curr(List("0","1"))
  }
  /*
  println(spaghetti.take(4).toList)
  println(ham.take(14).toList)
  */


}

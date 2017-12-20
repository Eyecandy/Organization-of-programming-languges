object ReadAloud extends App {
  def readAloud(lst: List[Int]): List[Int] = {
    def helper(lst: List[Int],oldHead:Int, count:Int, ans: List[Int]): List[Int] = {  
      if (lst.isEmpty) {
        val a = count::ans
        val b = oldHead::a
        b
      }
      else {
        if (oldHead == lst.head) { helper(lst.tail, oldHead, count+1,ans) }
        else {
          val a = count::ans
          val b = oldHead::a
          helper(lst.tail,lst.head,1,b)
        }
      }
    }
    //Empty list or Helper
    if (lst.isEmpty) { return List() }
    else { helper(lst.tail,lst.head,1,List()).reverse }
  }
 
  def unreadAloud(rlst: List[Int]): List[Int] = {

    def helper(lst:List[Int],curNo: Int,count: Int, newList: List[Int]): List[Int] = {
   
      if (count == 0) {
        val t = lst.tail
        val t2 = t.tail
        if (t2.isEmpty)  { newList }
        else  { helper(t2,t2.tail.head,t2.head, newList) }
      }

      else  {
        helper(lst,curNo,count -1 ,curNo::newList)}
      }
      //Empty list or helper
      if (rlst.isEmpty)  { List() }
      else { helper(rlst, rlst.tail.head, rlst.head, List()).reverse }
    }
      
  }

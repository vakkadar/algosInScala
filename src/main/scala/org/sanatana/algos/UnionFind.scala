package org.sanatana
package algos


trait UF{
  val n:Int
  def isConnected(p:Int, q:Int):Boolean
  def union(p:Int, q:Int):Unit
}
case class QFUF(n:Int) extends  UF{
  private [this] val seq = 0 until 10
  private [this] val  id = Array(seq:_*)
  def union(p:Int, q:Int):Unit = {
    val pid = id(p)
    val qid = id(q)
    for (i <- 0 until id.size){
      if(id(i) == pid)  // if any position has the value of the position in p, change it to the value in position of q
        id(i) = qid
    }
  }

  def isConnected(p:Int, q:Int):Boolean = id(p) == id(q)

}

case class QUF(n:Int) extends  UF{

  import RootUtil._

  private [this] val seq = 0 until 10
  private [this] val  id = Array(seq:_*)
  implicit val id2 = id

  def union(p:Int, q:Int):Unit = {
    val i = root(p)
    val j = root(q)
    id(i) = j // assign the q root to  the location where p as current root
  }
  def isConnected(p:Int, q:Int):Boolean = root(p) == root(q)

}
object RootUtil {
  def mutableRoot(i:Int)(implicit id:Array[Int]):Int = {
    var j = i ;
    while (j != id(j)){
      j = id(j)
    }
    j
  }

  def root(i:Int)(implicit id:Array[Int]):Int = {
    def loop(j:Int):Int =
      if(j == id(j)) // if the index value is the same as the value in that index it is the root
        j
      else
        loop(id(j)) // take value from the index and pass it as index
    loop(i)
  }
}
object RootTest extends App {
  implicit val arr = Array(2, 1, 1)
  val mrootOf2 = RootUtil.mutableRoot(2)
  Console println mrootOf2
  mrootOf2 == 1

  val rootOf2 = RootUtil.root(2)
  Console println rootOf2
  rootOf2 == 1
}


object UnionFindApp extends App {
 val n = 10
  val pairList = List((4,3), (3, 8), (6,5), (9,4), (2,1), (5,0), (7,2), (6,1))

  val uf = QFUF(n)
  pairList.foreach{elem =>
    val p = elem._1
    val q = elem._2
    if(!uf.isConnected(p,q)){
      uf.union(p,q)
      Console println(p + " " + q)
    }
  }
  Console println(uf.isConnected(1,2))
  Console println(uf.isConnected(1,3))
  val uf2 = QUF(n)
  pairList.foreach{elem =>
    val p = elem._1
    val q = elem._2
    if(!uf2.isConnected(p,q)){
      uf2.union(p,q)
      Console println(p + " " + q)
    }
  }
  Console println(uf2.isConnected(1,2))
  Console println(uf2.isConnected(1,3))
}
package org.sanatana
package algos


trait UF{
  val n:Int
  def isConnected(p:Int, q:Int):Boolean
  def union(p:Int, q:Int):Unit
  def components :Int
  protected def validate(p: Int): Unit = {
    if (p < 0 || p >= n)
      throw new IllegalArgumentException("index " + p + " is not between 0 and " + (n - 1))
  }
}
case class QFUF(n:Int) extends  UF{
  private [this] val seq = 0 until n
  private [this] var count = n
  private [this] val  id = Array(seq:_*)
  def components :Int = count
  def union(p:Int, q:Int):Unit = {
    val pid = id(p)
    val qid = id(q)
    for (i <- 0 until id.size){
      // if any position has the value of the position in p, change it to the value in position of q
      if(id(i) == pid) {
        id(i) = qid
        count -= 1
      }

    }
  }

  def isConnected(p:Int, q:Int):Boolean = id(p) == id(q)

}

case class QUF(n:Int) extends  UF{

  import RootUtil._
  private [this] var count = n
  private [this] val seq = 0 until n
  private [this] val  id = Array(seq:_*)
  implicit val id2 = id
  def components :Int = count
  def union(p:Int, q:Int):Unit = {
    val i = root(p)
    val j = root(q)
    id(i) = j // assign the q root to  the location where p as current root
    count -= 1
  }
  def isConnected(p:Int, q:Int):Boolean = root(p) == root(q)

}

case class WeightedQUF(n:Int) extends  UF{

  import RootUtil._
  private [this] var count = n

  private [this] val seq = 0 until n
  private [this] val rankSeq = Seq.fill(seq.size)(0:Byte)

  private [this] val  id = Array(seq:_*)
  private [this] val  rank :Array[Byte]= Array(rankSeq:_*)

  implicit val id2 = id
  def components :Int = count
  def union(p:Int, q:Int):Unit = {
    val i = root(p)
    val j = root(q)
    if(i == j)
      ()
    else{
      if(rank(i) < rank(j))
        id(i) = j
      else
      if (rank(i) > rank(j))
        id(j) = i
      else{
        id(i) = j // assign the q root to  the location where p as current root
         rank(i) = (rank(i) +1 ).asInstanceOf[Byte]
      }
      count -= 1
    }

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

  def mutableRootWithPathCompression(i:Int)(implicit id:Array[Int]):Int = {
    var j = i ;
    while (j != id(j)){
      id(j) = id(id(j)) // path compression
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

  def rootWithPathCompression(i:Int)(implicit id:Array[Int]):Int = {
    def loop(j:Int):Int =
      if(j == id(j)) // if the index value is the same as the value in that index it is the root
        j
      else{
        id(j) = id(id(j)) // path compression
        loop(id(j)) // take value from the index and pass it as index
      }

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
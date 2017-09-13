package org.sanatana.algos

case class ResizableArrayStack(initCapacity:Int = 10) {

  private [this] var internal = new Array[String](initCapacity)

  private [this] var headPosition = -1

  def isEmpty = headPosition == -1

  def push(item:String):Unit = {
    headPosition += 1
    if( isFull)
      increaseCapacity()
    internal(headPosition) = item
  }

  def pop():String = {
    val item = internal(headPosition)
    headPosition -= 1
    if(isQuarterFull)
      decreaseCapacity()
    item
  }

  private def isFull:Boolean = headPosition == internal.length
  private def isQuarterFull:Boolean = headPosition == (internal.length/4)

  private def increaseCapacity() :Unit = {
    val temp = new Array[String](internal.length * 2)
    Array.copy(internal,0, temp,0, internal.length )
    internal = temp
  }


  private def decreaseCapacity() :Unit = {
    val temp = new Array[String](internal.length / 2)
    Array.copy(internal,0, temp,0, internal.length / 4 )
    internal = temp
  }
}

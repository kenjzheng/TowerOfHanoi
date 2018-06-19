import scala.collection.mutable.ListBuffer
/**
  * Created by ken.j.zheng on 6/5/2018.
  */
object TowerOfHanoi extends App {
  def splitDiskByWaterMark(list:ListBuffer[(Int,Int)], waterMark:Int):(ListBuffer[(Int,Int)],ListBuffer[(Int,Int)])={
    val list1 = list.take(waterMark)
    val list2 = list.drop(waterMark)
    (list1,list2)
  }

  def getIndexOfBaseDisk(list:ListBuffer[(Int,Int)]):Int = {
    var init = list.last._2
    for (i <- list.length-1 to 0 by -1){
      if(init-list(i)._2>1)
        return i+1
      else
        init = list(i)._2
    }
    -1
  }

  def moveDisk(tower1:ListBuffer[(Int,Int)],
               tower2:ListBuffer[(Int,Int)],
               tower3:ListBuffer[(Int,Int)],
               direction:(String,String)
              ): (ListBuffer[(Int,Int)],ListBuffer[(Int,Int)],ListBuffer[(Int,Int)]) = {

    var x = tower1
    var y = tower2
    var z = tower3

    println(direction._1 + "->" + direction._2,x.map(m => {m._1}),y.map(m => {m._1}),z.map(m => {m._1}))

    val output = direction match {
      case (from,to) => {
        val output = (from,to) match {
          case ("TOWER1","TOWER3") => {
            if(x.length==2 || (x.length>2 && x(x.length-2)._2 - x(x.length-3)._2>1)){
              y.append(x.last)
              x = x.dropRight(1)
              z.append(x.last)
              x = x.dropRight(1)
              z.append(y.last)
              y = y.dropRight(1)

              (x,y,z)
            }
            else {
              val waterMark = getIndexOfBaseDisk(x)
              if(waterMark != -1){
                val (l1,l2) = splitDiskByWaterMark(x,waterMark)
                val (x1, y1, z1) = moveDisk(l2.tail, y, z, ("TOWER1", "TOWER2"))
                z1.append(l2.head)
                moveDisk(l1 ++ x1, y1, z1,("TOWER2", "TOWER3") )
              }
              else {
                val (x1, y1, z1) = moveDisk(x.tail, y, z, ("TOWER1", "TOWER2"))
                z1.append(x.head)
                moveDisk(x1, y1, z1, ("TOWER2", "TOWER3"))
              }
            }
          }
          case ("TOWER1", "TOWER2") => {
            if(x.length==2 || (x.length>2 && x(x.length-2)._2 - x(x.length-3)._2>1)){
              z.append(x.last)
              x = x.dropRight(1)
              y.append(x.last)
              x = x.dropRight(1)
              y.append(z.last)
              z = z.dropRight(1)

              (x,y,z)
            }
            else {
              val waterMark = getIndexOfBaseDisk(x)
              if(waterMark != -1){
                val (l1,l2) = splitDiskByWaterMark(x,waterMark)
                val (x1, y1, z1) = moveDisk(l2.tail, y, z, ("TOWER1", "TOWER3"))
                y1.append(l2.head)
                moveDisk(l1 ++ x1,y1,z1,("TOWER3", "TOWER2"))
              }
              else {
                val (x1, y1, z1) = moveDisk(x.tail, y, z, ("TOWER1", "TOWER3"))
                y1.append(x.head)
                moveDisk(x1, y1, z1, ("TOWER3", "TOWER2"))
              }
            }
          }
          case ("TOWER2", "TOWER1") => {
            if(y.length==2 || (y.length>2 && y(y.length-2)._2 - y(y.length-3)._2>1)){
              z.append(y.last)
              y = y.dropRight(1)
              x.append(y.last)
              y = y.dropRight(1)
              x.append(z.last)
              z = z.dropRight(1)
              (x,y,z)
            }
            else {
              val waterMark = getIndexOfBaseDisk(y)
              if(waterMark != -1){
                val (l1,l2) = splitDiskByWaterMark(y,waterMark)
                val (x1, y1, z1) = moveDisk(x, l2.tail, z, ("TOWER2", "TOWER3"))
                x1.append(l2.head)
                moveDisk(x1, l1 ++ y1, z1, ("TOWER3", "TOWER1"))
              }
              else {
                val (x1, y1, z1) = moveDisk(x, y.tail, z, ("TOWER2", "TOWER3"))

                x1.append(y.head)
                moveDisk(x1, y1, z1, ("TOWER3", "TOWER1"))
              }
            }
          }
          case ("TOWER2","TOWER3") => {
            if(y.length==2 || (y.length>2 && y(y.length-2)._2 - y(y.length-3)._2>1)){
              x.append(y.last)
              y = y.dropRight(1)
              z.append(y.last)
              y = y.dropRight(1)
              z.append(x.last)
              x = x.dropRight(1)

              (x,y,z)
            }
            else {
              val waterMark = getIndexOfBaseDisk(y)
              if(waterMark != -1){
                val (l1,l2) = splitDiskByWaterMark(y,waterMark)
                val (x1, y1, z1) = moveDisk(x, l2.tail, z, ("TOWER2", "TOWER1"))
                z1.append(l2.head)
                moveDisk(x1, l1 ++ y1, z1, ("TOWER1", "TOWER3"))
              }
              else {
                val (x1, y1, z1) = moveDisk(x, y.tail, z, ("TOWER2", "TOWER1"))
                z1.append(y.head)
                moveDisk(x1, y1, z1, ("TOWER1", "TOWER3"))
              }
            }
          }
          case ("TOWER3", "TOWER2") => {
            if(z.length==2 || (z.length>2 && z(z.length-2)._2 - z(z.length-3)._2>1)){
              x.append(z.last)
              z = z.dropRight(1)
              y.append(z.last)
              z = z.dropRight(1)
              y.append(x.last)
              x = x.dropRight(1)

              (x,y,z)
            }
            else {
              val waterMark = getIndexOfBaseDisk(z)
              if(waterMark != -1){
                val (l1,l2) = splitDiskByWaterMark(z,waterMark)
                val (x1, y1, z1) = moveDisk(x, y, l2.tail, ("TOWER3", "TOWER1"))
                y1.append(l2.head)
                moveDisk(x1, y1, l1 ++ z1, ("TOWER1", "TOWER2"))
              }
              else {
                val (x1, y1, z1) = moveDisk(x, y, z.tail, ("TOWER3", "TOWER1"))
                y1.append(z.head)
                moveDisk(x1, y1, z1, ("TOWER1", "TOWER2"))
              }
            }
          }
          case ("TOWER3", "TOWER1") => {
            if (z.length==2 || (z.length>2 && z(z.length-2)._2 - z(z.length-3)._2>1)){
              y.append(z.last)
              z = z.dropRight(1)
              x.append(z.last)
              z = z.dropRight(1)
              x.append(y.last)
              y = y.dropRight(1)

              (x,y,z)
            }
            else {
              val waterMark = getIndexOfBaseDisk(z)
              if(waterMark != -1){
                val (l1,l2) = splitDiskByWaterMark(z,waterMark)
                val (x1, y1, z1) = moveDisk(x, y, l2.tail, ("TOWER3", "TOWER2"))
                x1.append(l2.head)
                moveDisk(x1, y1, l1 ++ z1, ("TOWER2", "TOWER1"))
              }
              else {
                val (x1, y1, z1) = moveDisk(x, y, z.tail, ("TOWER3", "TOWER2"))
                x1.append(z.head)
                moveDisk(x1, y1, z1, ("TOWER2", "TOWER1"))
              }

            }
          }
        }
        output
      }
    }
    output
  }


  val testMethod = ListBuffer((9,1), (6,4), (3,7), (2,8), (1,9))
  val a1 = getIndexOfBaseDisk(testMethod)
  println(a1)

  val testSample = ListBuffer(13,12,11,10,9,8,7,6,5,4,3,2,1).zipWithIndex
  val test = testSample.map (m => {
    (m._1,m._2+1)
  })
  println(test)

  val t131 = ListBuffer[(Int,Int)]()
  val t132 = ListBuffer[(Int,Int)]()
  println(moveDisk(test,t131,t132,("TOWER1","TOWER3")))



}

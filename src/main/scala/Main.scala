//
//
//
//object Hello extends App{
//
//  val input = List("oleg", "oleg@email.com", "7bdaf0a1be3", "a8af118b1a2", "28d74b7a3fe")
////  val input = List("oleg oleg@email.com", "7bdaf0a1be3", "a8af118b1a2", "28d74b7a3fe")
//
//  val regex = "([a-zA-Z]+)".r
//  val regex_2 = "(\\w+@\\w+\\.\\w+)".r
//  val regex_3 = "([a-zA-Z]+)\\s(\\w+@\\w+\\.\\w+)".r
//
//
//
//  val result = input match {
//    case List( name@regex(_), email@regex_2(_), rest@_*) => s"$name ${email.split("@")(1)}"
//    case List( _@regex_3(name, email), rest@_*) => s"$name ${email.split("@")(1)}"
//    case _ => "invalid"
//    // Напишите нужные case
//  }
////  println(result)
//
//
//  val points: List[Int] = List(1, 3)
//  val chr1: List[Char] = List('x', 'x', 'x', 'x', 'x')
//  val chr2: List[Char] = List('y', 'y', 'y', 'y', 'y')
//
//
//  def cross(points: List[Int], pairs: List[(Char, Char)]): (String, String) = {
//    if (points.isEmpty) (pairs.unzip._1.mkString(""), pairs.unzip._2.mkString(""))
//    else cross(points.drop(1), pairs.take(points.head) ++ pairs.drop(points.head).map(x => (x._2, x._1)))
//  }
//
//  val result2 = cross(points, chr1 zip chr2)
//  println(result2._1)
//  println(result2._2)
//
//  type Point = (Int, Int)
//
//  type Ship = List[Point]
//
//  val x: Point = (5, 5)
//  val y: Ship = List((1, 1), (3, 3), (7, 7))
//
//  val (r,t) = y.unzip
//
//
//  println(Range(9,4).toList)
//  println((10 until 3).toList)
//
//
//
//}

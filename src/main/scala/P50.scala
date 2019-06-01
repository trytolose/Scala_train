//P50 (***) Huffman code.
//  First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman codes!
//  We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples.
//  E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)).
//  Our objective is to construct a list of (S, C) Tuples, where C is the Huffman code word for the symbol S.
//
//  scala> huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
//res0: List[String, String] = List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))


object Hello extends App{

  def huffman(list: List[(String, Int)]): List[(String, String)]= {
    val listMinusAdded = list.map(i => (i._1, i._2, -1))
    val code = huff(listMinusAdded).transpose.map(x => x.filter(_ > -1).reverse.mkString)
    list.map(_._1).zip(code)
  }


  def huff(list: List[(String, Int, Int)], acc: List[List[Int]]= List.empty): List[List[Int]] = {

    if (list.map(_._1).distinct.size == 1) acc
    else {
      val result = listTransform(list)
      huff(result, acc :+ result.map(x => x._3))
    }
  }

  def listTransform(list: List[(String, Int, Int)]): List[(String, Int, Int)] = {
    val(x1, x2) = list.map(r => (r._1, r._2)).distinct.sortBy(w => w._2).take(2) match {case List(x, y) => (x, y)}

    val result = list.map(x => (x._1, x._2, -1)).map(xx => if((xx._1, xx._2)==(x1._1, x1._2)) (x1._1++x2._1, x1._2+x2._2, 0) else xx)
      .map(xx => if((xx._1, xx._2)==(x2._1, x2._2)) (x1._1++x2._1, x1._2+x2._2, 1) else xx)
    result
  }


  val result = huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
  println(result)


}

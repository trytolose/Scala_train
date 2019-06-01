/* Некоторые генетические алгоритмы для генерации новых хромосом из старых используют приём под названием кроссинговер.

 Будем представлять хромосому с генами [xxxxx]   в виде списка List('x', 'x', 'x', 'x', 'x') .
 Тогда суть приёма заключается в следующем:

 Берутся две хромосомы одинаковой длины, например [xxxxx] и [yyyyy]. Списки для них будут выглядеть так:
 List('x', 'x', 'x', 'x', 'x')
 List('y', 'y', 'y', 'y', 'y')

 Выбираются так называемые `точки кроссинговера`. В нашем случае это некоторые индексы
 в диапазоне [1, длина списка генов хромосомы]. Пусть выбраны индексы 1 и 3.
 Для  каждого индекса, по возрастанию, хромосомы обмениваются своими частями, стоящими после этого индекса.
 В  нашем случае после кроссинговера по индексу 1:
 List('x', 'y', 'y', 'y', 'y')
 List('y', 'x', 'x', 'x', 'x')
  А после дальнейшего кроссинговера по индексу 3:
 List('x', 'y', 'y', 'x', 'x')
 List('y', 'x', 'x', 'y', 'y')
 Sample Input 1:
 1 3
 xxxxx
 yyyyy

 Sample Output 1:
 xyyxx
 yxxyy

 Sample Input 2:
 2 4 5
 aaaaaaa
 ddddddd

 Sample Output 2:
 aaddadd
 ddaadaa
*/


import scala.util.Random

object Cross extends App{

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1e6 + "ms")
    result
  }

  def crossVanil(points: List[Int], pairs: List[(Char, Char)]): (String, String) = {
    if (points.isEmpty) (pairs.unzip._1.mkString(""), pairs.unzip._2.mkString(""))
    else crossVanil(points.drop(1), pairs.take(points.head) ++ pairs.drop(points.head).map(x => (x._2, x._1)))
  }

  def crossV1(points: List[Int], pairs: List[(Char, Char)]): (String, String) = {
    if (points.isEmpty) (pairs.unzip._1.mkString(""), pairs.unzip._2.mkString(""))
    else crossV1(points.tail, pairs.drop(points.head).map(x => (x._2, x._1)):::pairs.take(points.head))
  }

  def crossV2(points: List[Int], pairs: List[(Char, Char)]): (String, String) = {
    if (points.isEmpty) (pairs.unzip._1.mkString(""), pairs.unzip._2.mkString(""))
    else crossV2(points.tail, pairs.zipWithIndex.filter(t => t._2 >= points.head).map(x => (x._1._2, x._1._1)))
  }


    def generateData(chrLen: Int, crossCount: Int): (List[Int], List[(Char, Char)]) = {
    val chr1 = Random.alphanumeric.take(chrLen).toList
    val chr2 = Random.alphanumeric.take(chrLen).toList
    val counts = Seq.fill(crossCount)(Random.nextInt(chrLen)).toList.distinct.sorted
    (counts, chr1 zip chr2)
  }

  val (ids, pairs) = generateData(100000, 1000)


  time{crossVanil(ids, pairs)}      //Elapsed time: 3829.742557ms
  time{crossV1(ids, pairs)}         //Elapsed time: 2245.928264ms
  time{crossV2(ids, pairs)}         //Elapsed time: 184.950546ms

}



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

// Функция для измерения времени выполнения
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1e6 + "ms")
    result
  }

// Наивный вариант
  def crossVanil(points: List[Int], pairs: List[(Char, Char)]): List[(Char, Char)]= {
    if (points.isEmpty) pairs
    else crossVanil(points.drop(1), pairs.take(points.head) ++ pairs.drop(points.head).map(x => (x._2, x._1)))
  }


  def crossV1(points: List[Int], pairs: List[(Char, Char)]): List[(Char, Char)] = {

//  Функция, создающая сперва бинарную маску, длиной с хромосому, единицы в точках кроссинговера
//  например List(0, 0, 0, 1, 0, 0, 1, 0)
//  А после возвращает куммулятивную сумму этой маски
//  например List(0, 0, 0, 1, 1, 1, 2, 2)

    def generateCumMask(ids: List[Int], m: Int, acc: List[Int] = List.empty): List[Int] = {
      if (m == 0) acc.scanLeft(0)(_+_).tail
      else generateCumMask(ids, m-1, if(ids.contains(m-1)) 1 +: acc else 0 +: acc )
    }

//  Если созмещаем пары хромосом и куммулятивную маску, если в конкретной точке сумма нечетная, то меняем буквы местами

    pairs.zip(generateCumMask(points, pairs.length)).map(x => if (x._2%2 != 0) (x._1._2,x._1._1) else x._1)

  }


//  Генерация хромосом для тестов
  def generateData(chrLen: Int, crossCount: Int): (List[Int], List[(Char, Char)]) = {

    val chr1 = Random.alphanumeric.take(chrLen).toList
    val chr2 = Random.alphanumeric.take(chrLen).toList
    val counts = Seq.fill(crossCount)(Random.nextInt(chrLen)).toList.distinct.sorted
    (counts, chr1 zip chr2)
  }

  // длина строки = 100000, кол-во точек кроссоверинга <= 10000
  val (ids, pairs) = generateData(100000, 10000)

//  val pairs = List('x', 'x', 'x', 'x', 'x') zip List('y', 'y', 'y', 'y', 'y')
//  val ids = List(1, 3)







  time{crossVanil(ids, pairs)}      //  Elapsed time: 24358.697572ms
  time{crossV1(ids, pairs)}         //  Elapsed time: 3063.789309ms


}



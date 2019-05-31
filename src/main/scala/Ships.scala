



  object Main extends App{
//    def main(args: Array[String]) {
      type Point = (Int, Int)
      type Field = Vector[Vector[Boolean]]
      type Ship = List[Point]
      type Fleet = Map[String, Ship]
      type Game = (Field, Fleet)

      def validateShip(ship: Ship): Boolean = { // определить, подходит ли корабль по своим характеристикам
        if (ship.size > 4) false
        val (x, y) = ship.unzip
        if(x.max >9 || y.max>9 || x.min <0 || y.min <0) false
        if(x.distinct.size != 1 && y.distinct.size != 1) false
        else true
      }

      def validatePosition(ship: Ship, field: Field): Boolean = {
        val (x, y) = ship.unzip
        val markedPoints = for{ i <- x.head-1 to x.last+1; j <- y.head-1 to y.last+1 } yield (i, j)
        fieldToPoints(field).filter(x => markedPoints.contains((x._1, x._2))).filter(y => y._3==true).isEmpty
      } // определить, можно ли его поставить

      def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = fleet++Map(name -> ship) // добавить корабль во флот

      def markUsedCells(field: Field, ship: Ship): Field = {
        val (x, y) = ship.unzip
        val markedPoints = for{ i <- x.head to x.last; j <- y.head to y.last } yield (i, j)

        val allPoints = fieldToPoints(field)

        val newPoints = allPoints.toList.filterNot( x => markedPoints.toList.contains((x._1, x._2)))++
          markedPoints.map(y => (y._1, y._2, true))

        pointsToField(newPoints.toSet)



      } // пометить клетки, которые занимает добавляемый корабль

      def tryAddShip(game: Game, name: String, ship: Ship): Game = {
        val (field, fleet) = game
        if (validateShip(ship)) if (validatePosition(ship, field)) {
          println(name)
          return (markUsedCells(field, ship), enrichFleet(fleet, name, ship))
        }
        game
      }

      def printField(field: Field): Unit = {
        field.map(x => x.collect({ case true => "X"; case false => "O"}).mkString("  ")).foreach(println)
      }

      def fieldToPoints(field: Field): Set[(Int, Int, Boolean)] = {
        field.zipWithIndex
          .flatMap(row_ind => row_ind._1.zipWithIndex
            .map(x => (row_ind._2, x._2, x._1)) ).toSet
      }
      def pointsToField(points: Set[(Int, Int, Boolean)]): Field = {
        points.groupBy(x => x._1).toList.sortBy(_._1)
          .map(y => y._2.toVector.sortBy(_._2).map(_._3)).toVector
      }

      val emptyField = Range(0, 10).toVector.map(_ => Range(0,10).map(_ => false).toVector)



    val ship_1 = List((1, 6), (1, 7), (1, 8))
    val ship_2 = List((2, 5), (3, 5), (4, 5), (5, 5))
    val ship_3 = List((9, 9))


    import scala.io.StdIn

    def readWhileStream(count: Int, pairs: List[(String, Ship)] = List.empty): List[(String, Ship)] = {

      def readSingleShip(len: Int, ship: Ship = List.empty ):Ship = {

        if (len == 0) ship

        else readSingleShip(len - 1, ship :+ (readLine().split(" ").map(_.toInt) match {case Array(a, b) => (a, b)}))
      }

      if (count == 0) pairs
      else {
        val first = readLine().split(" ")
        readWhileStream(count-1, pairs :+ (first.head, readSingleShip(first.last.toInt)))
      }

    }



    val game: Game = (emptyField, Map())
    val count = scala.io.StdIn.readInt()
    val newShips = readWhileStream(count)

    def addShips(game: Game, ships: List[(String, Ship)]): Game = {
      if (ships.size == 0) game
      else addShips(tryAddShip(game, ships.head._1, ships.head._2), ships.tail)
    }

    addShips(game, newShips)




    }
//}


object Test extends App{

  class Point(val x: Double, val y: Double, val z: Double)

  object Point {
    def apply(x: Double, y: Double, z: Double): Point = new Point(x, y, z)
    def show(p: Point): String = "${p.x} ${p.y} ${p.z}"
    def average(points: List[Point]): Point = {
      val mean: List[Double] = points.map(p => List(p.x, p.y, p.z)).transpose.map(x => x.sum/points.length)
      new Point(mean(0), mean(1), mean(2))

    }
  }

  val x = Point

}

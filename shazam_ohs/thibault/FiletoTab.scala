import java.io._
import scala.io.Source

object FiletoTab {

  def FiletoTab(i: Int): Array[Array[Array[Double]]] = {
    var res: Array[Array[Array[Double]]] = Array()
    var marq: Array[Array[Double]] = Array()
    var f1 = 0.0
    var f2 = 0.0
    var dt = 0.0
    var t = 0.0
    var cmp = 0
    for (line <- Source.fromFile("BDDE_" + files(i) + ".txt").getLines()) {
      if (cmp == 4) {
        cmp = 0
        marq = Array(Array(0.0, 0.0, 0.0), Array(0.0))
      }
      if (cmp == 0) {
        f1 = line.toDouble
        marq = Array(Array(f1, 0.0, 0.0), Array(0.0))
      }
      if (cmp == 1) {
        f2 = line.toDouble
        marq = Array(Array(f1, f2, 0.0), Array(0.0))
      }
      if (cmp == 2) {
        dt = line.toDouble
        marq = Array(Array(f1, f2, dt), Array(0.0))
      }
      if (cmp == 3) {
        t = line.toDouble
        marq = Array(Array(f1, f2, dt), Array(t))
        res = res ++ Array(marq)
      }
      cmp += 1
    }
    res
  }

}
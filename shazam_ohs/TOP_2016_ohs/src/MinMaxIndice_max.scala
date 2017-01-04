/* Ici 3 fonctions, qui renvoient ce que leur nom indique à partir de tableaux en entrée, sauf min 
 * qui ne prend que 2 entiers en entrée.
 */

object MinMaxIndice_max {

  def max(l: Array[Int]): Int = {
    var m: Int = 0
    for (i <- 0 to l.length - 1) {
      if (l(i) > m) {
        m = l(i)
      }
    }
    return (m);
  }

  def indice_max(l: Array[Double]): Double = {
    var m: Double = 0
    var res: Double = 0
    for (i <- 0 to l.length - 1) {
      if (l(i) > m) {
        m = l(i)
        res = i
      }
    }
    return (res);
  }

  def min(a: Int, b: Int): Int = {
    if (a < b) {
      return a
    }
    return b
  }

}
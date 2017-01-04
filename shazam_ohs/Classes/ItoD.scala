/* La fonction ItoD transforme un tableau de Int en tableau de Double
 * 
 */

object ItoD {

  def ItoD(tab: Array[Int]): Array[Double] = {
    var res: Array[Double] = Array()
    for (u <- 0 to tab.length - 1) {
      var temp: Double = tab(u)
      res = res ++ Array(temp)
    }
    return res
  }

}
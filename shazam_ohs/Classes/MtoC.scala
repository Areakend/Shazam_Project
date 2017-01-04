/* La fonction MtoC réduit la taille d'un tableau de 1024 valeurs en un tableau de 512 valeurs en
 * faisant une moyenne des valeurs 2 par 2.
 */

object MtoC {

  def MtoC(tab: Array[Array[Double]]): Array[Array[Double]] = {
    var tabres: Array[Array[Double]] = Array(Array())
    tabres(0) = tab(0)
    var res: Array[Double] = Array()
    for (j <- 1 to tab.length - 1) {
      res = Array()
      for (i <- 0 to 511) {
        res = res ++ Array((tab(j)(2 * i) + tab(j)(2 * i + 1)) / 2)
      }
      tabres = tabres ++ Array(res)
    }
    return tabres
  }

}
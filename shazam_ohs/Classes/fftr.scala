/* La fonction fftr prend en entr�e le tableau r�sultant de l'algorithme FFT, qui est compos� de valeurs de type
 * complexes, et transforme ce dernier en un tableau de r�els en faisant le module de chaque complexe.
 */

object fftr {

  def fftr(Tab: Array[complexes]): Array[Double] = {
    var l: Array[Double] = Array()
    var n = Tab.length
    for (y <- 1 to (n)) {
      l = l ++ Array((Tab(y - 1) mod (Tab(y - 1))).reel)
    }
    return l
  }

}
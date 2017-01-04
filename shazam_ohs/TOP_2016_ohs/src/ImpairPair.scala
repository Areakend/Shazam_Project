/* ImpairPair est une sous-fonction utilisée dans la fonction "fft" : elle prend en entrée un tableau de complexes
 * et renvoie deux tableaux des valeurs du tableau d'entrée : celui des indices pairs et celui des indices impairs
 */

object ImpairPair {

  def ImpairPair(tab: Array[complexes]): Array[Array[complexes]] = {
    var tab1: Array[complexes] = Array()
    var tab2: Array[complexes] = Array()
    for (i <- 0 to tab.length - 1) {
      if (i % 2 == 0) {
        tab1 = tab1 ++ Array(tab(i))
      } else {
        tab2 = tab2 ++ Array(tab(i))
      }
    }
    return Array(tab1, tab2)
  }

}
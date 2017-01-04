/* La fonction tri n'est ni plus ni moins qu'une réecriture en Scala de l'implémentation du tri par insertion
 * trouvé sur Wikipédia, sous différents langages, à cette adresses :
 * https://fr.wikibooks.org/wiki/Impl%C3%A9mentation_d%27algorithmes_classiques/Algorithmes_de_tri/Tri_par_insertion
 *
 * C'est une sous-fonction de la fonction "fft"
 */

object tri {
  
  def tri(tab: Array[(Int, complexes)]): Array[complexes] = {
    var res: Array[complexes] = Array()
    var n: Int = tab.length
    var el: Int = 0
    var j: Int = 0
    var temp: Int = 0
    var tuple = tab(0)
    for (i <- 1 to n - 1) {
      el = tab(i)._1
      tuple = tab(i)
      j = i
      while (j > 0 && el < tab(j - 1)._1) {
        tab(j) = tab(j - 1)
        j -= 1
      }
      tab(j) = tuple
    }
    for (i <- 0 to tab.length - 1) {
      res = res ++ Array(tab(i)._2)
    }
    res
  }
  
}
/* La fonction fft applique de mani�re r�cursive l'algorithme de Fast Fourier Transform
 * La r�alisation de cette fonction s'inspire d'informations trouv�es sur Internet ainsi que certains mod�les
 * d�j� pr�sents sur la toile. Elle utilise les fonctions auxiliaires "ImpairPair" et "tri"
 */

object fft {

  // Certaines fonctions auxiliaires n'�tant pas pr�sentes, la fonction est mise en commentaire pour �viter
  // les erreurs.

/*  
  
  def fft(tab: Array[complexes]): Array[complexes] = {
    if (tab.length == 0) { Array() }
    if (tab.length == 1) { tab }
    else {
      var n = tab.length
      var tabP = fft(ImpairPair(tab)(0))
      var tabIP = fft(ImpairPair(tab)(1))
      def Xk(table: Array[(Int, complexes)], k: Int = 0): Array[(Int, complexes)] = {
        var w = exp(complexes(0, -2 * Pi * k / n))
        if (k < (n / 2)) {
          Xk(Array((k + n / 2, tabP(k) - tabIP(k) * (w))) ++ Array((k, tabP(k) + tabIP(k) * (w))) ++ table, k + 1)
        } else { table }
      }
      tri(Xk(Array[(Int, complexes)]()))
    }
  } 

*/

}
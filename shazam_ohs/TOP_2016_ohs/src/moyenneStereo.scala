/* La fonction moyenneStereo prend en entr�e le tableau wav2D d'une musique : si celle si est en st�r�o
 * une conversion s'effectue (moyenne des 2 canaux) pour passer de st�r�o � mono.
 */

object moyenneStereo {

  // Certaines fonctions auxiliaires n'�tant pas pr�sentes, la fonction est mise en commentaire pour �viter
  // les erreurs.

/*  

  def moyenneStereo(wav2D: Array[Array[Int]]): Array[Array[Int]] = {
    var mono: Array[Array[Int]] = wav2D.slice(0, 0)
    if ((wav2D(0)(1) == 2)) {
      var n: Int = min(wav2D(1).length, wav2D(2).length)
      for (i <- 0 to n - 1) {
        mono = mono ++ Array(Array(wav2D(1)(i) + wav2D(2)(i)))
        println("Conversion en Mono " + i + "sur " + n)
      }
      return mono
    } else {
      return wav2D
    }
  }

*/

}
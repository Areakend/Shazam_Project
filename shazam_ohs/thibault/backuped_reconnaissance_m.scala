import java.io._
import scala.io.Source

object backuped_reconnaissance_m {

  def backuped_reconnaissance_m(wav2D: Array[Array[Int]]): String = {
    var ch_f = ""
    var Hz_f = ""
    if (wav2D(0)(1) == 1) { ch_f = "Mono" }
    if (wav2D(0)(1) == 2) { ch_f = "Stereo" }
    if (wav2D(0)(0) == 11025) { Hz_f = "11k" }
    if (wav2D(0)(0) == 22050) { Hz_f = "22k" }
    if (wav2D(0)(0) == 44100) { Hz_f = "44k" }
    var dirPath = "C:/Users/Thibault/Documents/Ecole/shazam_ohs/Musique/" + ch_f + "_" + Hz_f + "/"
    var fichiers: Array[String] = Utils.listFiles(dirPath)
    var tab_same: Array[Double] = Array()
    var E: Array[Array[Array[Double]]] = empreinte(wav2D)
    var M: Array[Array[Array[Double]]] = Array()
    for (i <- 0 to fichiers.length - 1) {
      M = FiletoTab(i, fichiers, dirPath)
      tab_same = tab_same ++ Array(memediff(E_occur(E, M)))
    }
    println(tab_same.deep)
    var indice_musique = indice_max(tab_same)
    if (tab_same(indice_musique.toInt) < 250) {
      return ("Musique inconnue")
    }
    return ("La musique est extraite de " + fichiers(indice_musique.toInt))

  }
  
}
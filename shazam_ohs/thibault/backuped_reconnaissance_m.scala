import java.io._
import scala.io.Source

object backuped_reconnaissance_m {

  def backuped_reconnaissance_m(wav2D: Array[Array[Int]]): String = {
    var tab_same: Array[Int] = Array()
    var E: Array[Array[Array[Double]]] = empreinte(wav2D)
    var M: Array[Array[Array[Double]]] = Array()
    for (i <- 0 to files.length - 1) {
      M = FiletoTab(i)
      tab_same = tab_same ++ Array(memediff(E_occur(E, M)))
    }
    println(tab_same.deep)
    var indice_musique = indice_max(tab_same)
    return (files(indice_musique))
  }

}
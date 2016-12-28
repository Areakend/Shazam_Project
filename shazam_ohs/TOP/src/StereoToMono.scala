object StereoToMono {

  def moyenneStereo(wav2D:Array[Array[Int]]):Array[Int] = {
    var mono:Array[Int] = Array()
    if ( (wav2D(0)(1) == 2) ) {
      mono = wav2D(1) ++ wav2D(2)
      return mono
    } else {
      return wav2D(1)
    }
  }
  
}
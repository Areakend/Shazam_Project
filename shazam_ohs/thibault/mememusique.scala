object mememusique {
  
  
  def mememusiquerec( E:Array[Array[Array[Double]]], M:Array[Array[Array[Double]]], T:Array[Array[Double]], o:Int, n:Int):Double = {
  if (T == Array()) {
    
  } else {
    
  }
}

  def E_occur( E:Array[Array[Array[Double]]], M:Array[Array[Array[Double]]], T:Array[Array[Double]] , k:Int, c:Int):Array[Array[Double]] = {
  if (k == E.length-1) {
    T
  } else {
    if (c == M.length-1) {
      E_occur( E, M, T, k+1, 0)
    } else {
      if (E(k)(0).deep == M(c)(0).deep) {
        T(k) = T(k) ++ Array(M(c)(1)(0))
        E_occur( E, M, T, k, c+1)
      } else {
        E_occur( E, M, T, k, c+1)
      }
      
    }
}
}
  
  
}
  
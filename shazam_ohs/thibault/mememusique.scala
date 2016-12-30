object mememusique {

  def E_queue( T:Array[Array[Double]]):Array[Array[Double]] = {
    var L_tmp:List[List[Double]] = Nil
    var T_res:Array[Array[Double]] = Array()
    for (k <- 0 to T.length-1) {    
      L_tmp = L_tmp ++ List(T(k).toList)
    }
    for (k <- 0 to T.length-1) {
      T_res = T_res ++ Array((L_tmp(k).tail).toArray)    
    }
    T_res
  }
  
  
  def E_occur( E:Array[Array[Array[Double]]], M:Array[Array[Array[Double]]], T:Array[Array[Double]] , k:Int, c:Int):Array[Array[Double]] = {
    if (k == E.length) {
      T
    } else {
      if (c == M.length) {
        
        E_occur( E, M, T ++ Array( Array(-1.0) ) , k+1, 0)
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
  
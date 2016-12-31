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

  def min(l: Array[Double]):Double = {
    var m : Double = l(0)
    for (i <- 0 to l.length-1) {
      if ( l(i) < m ) { m = l(i)}
    }
    return m
  }
  
  def pourcentage(M:Array[Array[Array[Double]]], E:Array[Array[Array[Double]]], T:Array[Array[Double]], o:Int, n:Int, k:Int, j:Int, m:Int, i:Int):Double = {
    if (k == M.length-1) {return (o/(o+n))}
    else {
      if (T(k) == Array() ) { pourcentage(M, E, T, o, n+1, k+1, 0, 0, 0) }
      else {
        if ( T(k)(i) == M(m)(1)(0)) { 
          if (E(j)(0).deep == M(m)(0).deep) { pourcentage(M, E, T, o+1, n, k+1, j+1, 0, 0) }
          else {
            if (m == M.length-1) { pourcentage(M, E, T, o, n+1, k+1, j+1, 0, 0) }
            else {  pourcentage(M, E, T, o, n, k, j, m+1, 0) }
          }    
        }
        else {
          if (i == T(k).length-1) { pourcentage(M, E, T, o, n+1, k+1, j, 0, 0 ) }
          else { pourcentage(M, E, T, o, n+1, k, j, 0, i+1) }
        }
      }
    }
  }
  
 /* def chemin_max(T:Array[Array[Double]]
  
  
  def mememusique( i : Int, E:Array[Array[Array[Double]]]):String = {
    var M : Array[Array[Array[Double]]] = Fempreinte(BDD(i))
    var T_occur : Array[Array[Double]] = E_queue( E_occur(E,M, Array(Array(-1.0)),0,0) )
  }
  */
  
  
}
  
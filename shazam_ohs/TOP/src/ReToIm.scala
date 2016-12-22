
object ReToIm {
  
  def ReToIm(Tab: Array[Int]): Array[complexes] = {
    var l : Array[complexes]= Array()
    var n =Tab.length
    for (y<- 1 to (n)) {
      l = l ++ Array( complexes( Tab(y-1),0 ) )
    }
    return l
	}

}
object fftr {
  def fftr(Tab: Array[complexes]): Array[Double] = {
    var l : Array[Double]= Array()
    var n =Tab.length
    for (y<- 1 to (n)) {
      l = l ++ Array( (Tab(y-1) mod (Tab(y-1))).reel ) 
      }
    return l
	}
}
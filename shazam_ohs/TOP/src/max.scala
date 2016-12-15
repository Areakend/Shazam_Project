object max {
  def max(l: Array[Int]) : Int = {
    var m : Int = 0
    for (i<-0 to l.length-1) {
            if (l(i)>m) {m=l(i)}
    }
    return(m);
  }

}
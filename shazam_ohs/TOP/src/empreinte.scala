object empreinte {
  def max(l: Array[Int]) : Int = {
    var m : Int = 0
    for (i<-0 to l.length-1) {
            if (l(i)>m) {m=l(i)}
    }
    return(m);
  }
var indices : Array[Int] = Array();
var wav2D : Array[Array[Int]] = Array(Array(10),Array(10));
var frequence : Array[Int] = fft(wav2D(1));
var amplitude : Array[Int] = wav2D(1);
var empreinte : Array[Array[Double]] = Array(Array());
var frame = wav2D(0)(0);
var dt : Double = 1/frame;

//Tableau des indices des grandes amplitudes

for (i<-0 to wav2D(1).length) {
  if (wav2D(1)(i) > (7/8)*max(wav2D(1)))
    indices = indices ++ Array(i);
}

//On récupère les frequences correspondantes

for (i<-0 to (frequence.length/2)) {
  var freq : Int = frequence(2*i);
  var freq2 : Int = frequence(2*i+1);
  empreinte = empreinte ++ Array( Array(freq,freq2,(indices(i+1)-indices(i))*dt, indices(i)*dt)) //*dt pour definir le temps et non interval d'indices
}
  

}
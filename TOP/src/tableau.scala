object tableau extends App {
import com.tncy.top.files.WavWrapper;
var filePath : String = "/home/etudiants/ohanian1u/TOP/WAV_Chansons_completes_Mono_11025Hz/Conquest_of_Spaces_Mono-11025Hz.wav";
var wrappedWav : WavWrapper = new WavWrapper(filePath);
var wav2D : Array[Array[Int]] = wrappedWav.getWav();

var frame = wav2D(0)(0)
var chanel = wav2D(0)(1)

var tinit = 0
var dt = frame*1
var empreintes : Array[Int] = [0,0,0,0,0]

for (i<- 0 to 9) {
  println(empreintes(i))
  empreintes(i) =  Array(Array(wav2D(1)(tinit),wav2D(1)(tinit + dt*i),dt),tinit)
  }

}


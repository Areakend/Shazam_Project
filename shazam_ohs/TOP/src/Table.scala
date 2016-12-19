object Table extends App {// Imports the API library
import com.tncy.top.files.WavWrapper;
 
// Source wav file
var filePath : String = "C:/Users/Zaven/Desktop/TOP/shazam_ohs/Musique/WAV_Chansons_completes_Mono_11025Hz.wav";
 
// Loads wrapped wav
var wrappedWav : WavWrapper = new WavWrapper(filePath);
// Gets the music content
var wav2D : Array[Array[Int]] = wrappedWav.getWav();
 
// Prints the wav's sample rate, number of channels and number of frames
println("The music's sample rate is: " + wav2D(0)(0) + " frames per second.");
println("The music's number of channels is: " + wav2D(0)(1) + " channels.");
println("The music's number of frames is: " + wav2D(0)(2) + " frames.");
print(wav2D(1)(2000))
//for (i<-0 to wav2D(1).length) {
//  println(wav2D(1)(i))
//}
}
object Table extends App {// Imports the API library
import com.tncy.top.files.WavWrapper;
 // Imports the API library
import com.tncy.top.files.Utils;
 
// Directory path
var directoryPath : String = "C:/Users/Zaven/Desktop/Projet/Musique/WAV_Chansons_completes_Mono_11025Hz";
 
// Gets the array of files and folders
var files : Array[String] = Utils.listFiles(directoryPath);
     
// Prints the name of the first file or folder of the directory, and its path
//if (files.length > 0){
//      println("The first file's name is : " + files(0) + " and its path is " + directoryPath + "/" + files(0));
//}

// Source wav file

var BDD : Array[Array[Array[Int]]] = Array(Array(Array()))
   for (i<-0 to files.length) {
     var filePath : String = "C:/Users/Zaven/Desktop/Projet/Musique/WAV_Chansons_completes_Mono_11025Hz/" + files(i);     
     var wrappedWav : WavWrapper = new WavWrapper(filePath);
     var wav2D : Array[Array[Int]] = wrappedWav.getWav();
     BDD = BDD ++ Array(wav2D)
   }
println(BDD)

 
// Loads wrapped wav

// Gets the music content

 
// Prints the wav's sample rate, number of channels and number of frames
//println("The music's sample rate is: " + wav2D(0)(0) + " frames per second.");
//println("The music's number of channels is: " + wav2D(0)(1) + " channels.");
//println("The music's number of frames is: " + wav2D(0)(2) + " frames.");
//print(wav2D(1)(2000))
//for (i<-0 to wav2D(1).length) {
//  println(wav2D(1)(i))
//}

}
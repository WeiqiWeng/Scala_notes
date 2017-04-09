package scala_note
import scala.io.Source
/*
  In this worksheet we will put things in a nutshell.
*/
object mnemonics {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(276); 

	// First we load a small word dictionary.
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt");System.out.println("""in  : scala.io.BufferedSource = """ + $show(in ));$skip(303); 
	
	// The word list may contain some special character such as '-', ','.
	// The method getLines will return an iterator. For the coming code, we need to
	// make the iterator a list and filter out the special characters.
	val words = in.getLines.toList filter (word => word forall (ch => ch isLetter));System.out.println("""words  : List[String] = """ + $show(words ));$skip(156); 
	
	// the mnemonics
	def mnem = Map(
		'2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  	'6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ");System.out.println("""mnem: => scala.collection.immutable.Map[Char,String]""");$skip(199); 
  	
  
  // invert mnemonic to have a map from character to number.
  // charToNum.size should be 26.
  val charToNum: Map[Char, Char] =
  	for {(num, str) <- mnem
  				itr <- str} yield itr -> num;System.out.println("""charToNum  : Map[Char,Char] = """ + $show(charToNum ));$skip(182); 
	
	// map a word to the corresponding number combination
	// Here we use the function call of a map collection.
	def wordToNum(word: String): String = word.toUpperCase map charToNum;System.out.println("""wordToNum: (word: String)String""");$skip(22); val res$0 = 
	
	wordToNum("Scala");System.out.println("""res0: String = """ + $show(res$0));$skip(164); 
	
	// find all the words that can be mapped to a given number combination
	val numToWord: Map[String, Seq[String]] = words groupBy wordToNum withDefaultValue Seq();System.out.println("""numToWord  : Map[String,Seq[String]] = """ + $show(numToWord ));$skip(297); 
	
	// encode a given number to all the possible list of words
	def encode(num: String): Set[List[String]] = {
		if (num.isEmpty) Set(List())
		else{
			for {
				split <- 1 to num.length
				word <- numToWord(num take split)
				rest <- encode(num drop split)
			} yield word :: rest
		}.toSet
	};System.out.println("""encode: (num: String)Set[List[String]]""");$skip(24); val res$1 = 
	
	encode("7225247386");System.out.println("""res1: Set[List[String]] = """ + $show(res$1))}
  
   

}

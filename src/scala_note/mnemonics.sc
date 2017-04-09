package scala_note
import scala.io.Source
/*
  In this worksheet we will put things in a nutshell.
*/
object mnemonics {

	// First we load a small word dictionary.
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator
	
	// The word list may contain some special character such as '-', ','.
	// The method getLines will return an iterator. For the coming code, we need to
	// make the iterator a list and filter out the special characters.
	val words = in.getLines.toList filter (word => word forall (ch => ch isLetter))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.
	
	// the mnemonics
	def mnem = Map(
		'2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  	'6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
                                                  //> mnem: => scala.collection.immutable.Map[Char,String]
  	
  
  // invert mnemonic to have a map from character to number.
  // charToNum.size should be 26.
  val charToNum: Map[Char, Char] =
  	for {(num, str) <- mnem
  				itr <- str} yield itr -> num
                                                  //> charToNum  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J 
                                                  //| -> 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5
                                                  //| , B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z 
                                                  //| -> 9, S -> 7)
	
	// map a word to the corresponding number combination
	// Here we use the function call of a map collection.
	def wordToNum(word: String): String = word.toUpperCase map charToNum
                                                  //> wordToNum: (word: String)String
	
	wordToNum("Scala")                        //> res0: String = 72252
	
	// find all the words that can be mapped to a given number combination
	val numToWord: Map[String, Seq[String]] = words groupBy wordToNum withDefaultValue Seq()
                                                  //> numToWord  : Map[String,Seq[String]] = Map(63972278 -> List(newscast), 2923
                                                  //| 7638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> List
                                                  //| (allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 8684
                                                  //| 37 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 336
                                                  //| 4646489 -> List(femininity), 3987267346279 -> List(extraordinary), 7855397 
                                                  //| -> List(pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 386
                                                  //| 583 -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 84
                                                  //| 7827 -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curlic
                                                  //| ue), 84863372658 -> List(thunderbolt), 46767833 -> List(imported), 26437464
                                                  //|  -> List(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(sp
                                                  //| oolers), 46636233 -> List(homemade), 7446768759 -> List(rigorously), 746446
                                                  //| 47 -> List(ringings), 633738 -> List(offset), 847825 -> List(visual), 77283
                                                  //| 2 -> List(Pravda), 4729
                                                  //| Output exceeds cutoff limit.
	
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
	}                                         //> encode: (num: String)Set[List[String]]
	
	encode("7225247386")                      //> res1: Set[List[String]] = Set(List(rack, ah, re, to), List(sack, ah, re, to
                                                  //| ), List(Scala, ire, to), List(sack, air, fun), List(rack, air, fun), List(r
                                                  //| ack, bird, to), List(pack, air, fun), List(pack, ah, re, to), List(pack, bi
                                                  //| rd, to), List(Scala, is, fun), List(sack, bird, to))
  
   

}
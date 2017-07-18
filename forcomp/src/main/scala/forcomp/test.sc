import forcomp.Anagrams.{Occurrences, Sentence, Word, dictionary, dictionaryByOccurrences}

import scala.collection.Map

val s = "Helaloaa"

def wordOccurrences(w: Word): Occurrences =
  w.toLowerCase.groupBy(c => c).toList.map(pair => (pair._1, pair._2.length)).sortWith((p1,p2) => p1._1 < p2._1)

wordOccurrences(s)

val s2 = List("Hello", "Howareyou", "eat", "ate")

def sentenceOccurrences(s: Sentence): Occurrences =
  wordOccurrences(s.reduce((str1, str2) => str1.concat(str2)))

sentenceOccurrences(s2)

val map = Map("ser" -> 2, "tez" ->3)

val x = List(('a', 2), ('d', 1), ('l', 1), ('r', 1))
val y = List(('r', 1), ('a', 2), ('d', 1), ('l', 1))


val mapX = x.groupBy(p => p._1).mapValues(list => list(0)._2) withDefaultValue 0
val mapY = y.groupBy(p => p._1).mapValues(list => list(0)._2) withDefaultValue 0


def subtract(x: Occurrences, y: Occurrences): Occurrences = {

  def subtractHelp(mapX: Map[Char,Int], y: List[(Char,Int)]): Map[Char, Int] = {
    y match {
      case Nil => mapX
      case i :: is => subtractHelp(mapX.updated(i._1, mapX(i._1) - i._2), is)
    }
  }

  val mapX = x.groupBy(p => p._1).mapValues(list => list(0)._2) withDefaultValue 0

  subtractHelp(mapX, y).filter(p => p._2 > 0).toList.sorted

}

subtract(x, y)

val combList = List(('a', 2), ('b', 2), ('c',1))

val words = List("Hey", "Yo")
val words2 = List("z", "a")

for (w1 <- words; w2 <- words2) yield w1.concat(" "+w2)

words.map(str => "z".concat(" " + str))

def combinations(occurrences: Occurrences): List[Occurrences] = {
  occurrences :: (for (occr <- occurrences) yield combinations(subtract(occurrences, List((occr._1,1))))).flatten.distinct
}

combinations(combList)

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  val occs = sentenceOccurrences(sentence)

  def occAnagrams(occurrences: Occurrences): List[Sentence] = {
    val res = (for( occ <- combinations(occurrences) if occ.length > 0 ) yield {
      (for( w <- dictionaryByOccurrences(occ) ) yield {
        if( subtract(occurrences, occ).length == 0 ) List(List(w))
        else {
          val test = (for( anag <- occAnagrams(subtract(occurrences, occ)) ) yield {
            w :: anag
          })
          test
        }
      }).flatten
    }).flatten
    res.asInstanceOf[List[Sentence]]
  }
  if( sentence.length == 0 ) List(List())
  else occAnagrams(occs)
}

sentenceAnagrams(List("Yes", "man"))

val z = List(List(1,2,3, 3), List('a'))

z.flatten.distinct
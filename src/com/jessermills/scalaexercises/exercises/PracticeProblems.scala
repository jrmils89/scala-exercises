package com.jessermills.exercises

import collection.mutable.HashMap

object PracticeProblems {

  def transformWords(words: List[String]) = {

    val hashMap = new HashMap[String, Boolean]()
    words
      .flatMap(shortenWordValidatingPreviousAcronyms(hashMap, _))
      .foreach(str => println(str))
  }

  private def shortenWordValidatingPreviousAcronyms(map: HashMap[String, Boolean], word: String) : Option[String] = {

    if (word.isEmpty)
      None
    else
      recursiveShortenWord(
        map = map,
        originalWord = word,
        shortenedWord = shortenWord(
          word = word,
          offset = 0),
        offset = 0
      )

  }

  private def recursiveShortenWord(map: HashMap[String, Boolean], originalWord: String, shortenedWord: String, offset: Int): Option[String] = {

    map.contains(shortenedWord) match {
      case false =>
        map.put(shortenedWord, true)
        Some(shortenedWord)
      case true =>

        val newOffset = offset + 1

        if (newOffset < originalWord.length) {

          val newShortenedWord = shortenWord(originalWord, newOffset)

          map.contains(newShortenedWord) match {
            case true => recursiveShortenWord(map, originalWord, newShortenedWord, newOffset)
            case false =>
              map.put(newShortenedWord, true)
              Some(newShortenedWord)
          }

        } else {
          None
        }

    }

  }

  private def shortenWord(word: String, offset: Int) : String = {

    val len = word.length
    val numberOffset = offset + 2
    val lastCharIndex = len - 1

    if (numberOffset >= lastCharIndex)
      word
    else {
      s"${word.substring(0, offset + 1) + (len - numberOffset) + word.charAt(lastCharIndex)}"
    }
  }

}
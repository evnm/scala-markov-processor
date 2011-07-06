package com.evnm.markovprocessor

import scala.util.Random

/*
 * An Ngram class.
 */
class Ngram(val leading_words: List[String], val choices: Map[String, Int]) {
  val count = choices.foldLeft(0) { (acc, kv) => acc + kv._2 }

  /*
   * Returns a copy of the Ngram with the argument string added to the list of
   * completions.
   */
  def +(str: String): Ngram = {
    choices get str match {
      case Some(i) => new Ngram(leading_words, choices + (str -> (i + 1)))
      case None => new Ngram(leading_words, choices + (str -> 1))
    }
  }


  /*
   * Picks a random completion word from a given ngram, with probabilities
   * weighted by the count of each completion word.
   */
  def randomWord = {
    val lst = choices.flatMap(t => List.fill(t._2)(t._1)).toList
    lst(Random.nextInt(lst.length))
  }
}

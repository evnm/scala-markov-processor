package com.evnm.markovprocessor

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
}

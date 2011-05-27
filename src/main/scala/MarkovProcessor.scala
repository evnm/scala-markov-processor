package com.evnm.markovprocessor

/*
 * A Markov processor class.
 */
class MarkovProcessor {
  /*
   * Comparator for lists of strings. Assumes lists are of equal length.
   */
  def stringListCompare(list1: List[String], list2: List[String]): Int = {
    for ((str1, str2) <- list1 zip list2) {
      if (str1 < str2) return -1
      else if (str1 > str2) return 1
    }
    return 0
  }
}

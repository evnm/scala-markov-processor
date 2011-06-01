package com.evnm.markovprocessor

import org.scalatest.FunSuite

class NgramSuite extends FunSuite {
  test("Ngram should have passed leading words") {
    val lst = List("a", "b", "c")
    val ngram = new Ngram(lst, Map())
    assert(ngram.leading_words == lst)
  }

  test("Ngram should have passed choices") {
    val m = Map("foo" -> 1, "bar" -> 2)
    val ngram = new Ngram(List(), m)
    assert(ngram.choices == m)
  }

  test("Ngram should have count equal to number of passed completions") {
    val m = Map("foo" -> 1, "bar" -> 2, "baz" -> 5)
    val ngram = new Ngram(List(), m)
    assert(ngram.count == 8)
  }

  test("An added completion word should be contained in the completion list") {
    val ngram = new Ngram(List(), Map()) + "foo"
    assert(ngram.choices == Map("foo" -> 1))
  }

  test("Adding a completion word should increment the count") {
    var ngram = new Ngram(List(), Map())
    for (i <- 1 until 10) {
      ngram += "foo"
      assert(ngram.count == i)
    }
  }
}

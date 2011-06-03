package com.evnm.markovprocessor

import org.scalatest.FunSuite
import com.evnm.markovprocessor.{MarkovProcessor => MP}

class MarkovProcessorSuite extends FunSuite {
  test("groupWords should return Nil when input list is Nil") {
    assert(MP.groupWords(0, Nil) == Nil)
    assert(MP.groupWords(3, Nil) == Nil)
  }

  test("groupWords should return Nil when n > length of input list") {
    assert(MP.groupWords(1, Nil) == Nil)
    assert(MP.groupWords(2, List("a")) == Nil)
    assert(MP.groupWords(3, List("a", "b")) == Nil)
  }

  test("groupWords should return appropriate list on valid input") {
    val lst = List((List("a", "b"), "c"), (List("b", "c"), "d"), (List("c", "d"), "e"))
    assert(MP.groupWords(3, List("a", "b", "c", "d", "e")) == lst)
  }
}

package com.evnm.markovprocessor

import scala.io.Source
import org.scalatest.FunSuite
import com.evnm.markovprocessor.{MarkovProcessor => MP}

class MarkovProcessorSuite extends FunSuite {
  /*
   * Test groupWords.
   */
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

  test("Grouping massive lists should not generate stack overflows") {
    val words = Source.fromFile("src/test/resources/hamlet.txt").mkString.split("( |\n)+").toList
    MP.groupWords(4, words)
  }

  /*
   * Test buildTree.
   */
  test("buildTree should return an EmptyNode when given an empty string") {
    MP.buildTree(3, "") match {
      case EmptyNode => assert(true)
      case _ => assert(false, "tree built from empty string should be an EmptyNode")
    }
  }

  test("buildTree should return a single leaf Node when given a string of n=3 words") {
    MP.buildTree(3, "oh hai there") match {
      case Node(Ngram(lw, c), EmptyNode, EmptyNode) => {
        assert(lw == List("oh", "hai"))
        assert(c == Map("there" -> 1))
      }
      case _ => assert(false, "tree built was invalid")
    }
  }

  test("buildTree should return an appropriate tree when given a non-trivial string") {
    MP.buildTree(4, "four score and seven years ago") match {
      case Node(Ngram(lw, c), left, right) => {
        assert(lw == List("four", "score", "and"))
        assert(c == Map("seven" -> 1))
        right match {
          case Node(Ngram(lw, c), left, EmptyNode) => {
            assert(lw == List("score", "and", "seven"))
            assert(c == Map("years" -> 1))
          }
          case _ => assert(false, "right should not be empty")
        }
        left match {
          case Node(Ngram(lw, c), l, r) => {
            assert(lw == List("and", "seven", "years"))
            assert(c == Map("ago" -> 1))
          }
          case _ => assert(false, "left should not be empty")
        }
      }
      case _ => assert(false, "tree built was invalid")
    }
  }

  /*
   * Test find.
   */
  test("find should return None on an empty tree") {
    assert(!MP.find(List(), EmptyNode).isDefined)
  }

  test("find should return a valid ngram option on a non-empty tree") {
    val result =
      MP.find(List("foo"), Node(Ngram(List("foo"),Map("bar" -> 1)), EmptyNode, EmptyNode))
    assert(result.isDefined)
    assert(result.get.leading_words == List("foo"))
    assert(result.get.choices == Map("bar" -> 1))
  }

  test("blah") {
    val words = Source.fromFile("src/test/resources/hamlet.txt").mkString
    println(MP.buildList(100, MP.buildTree(4, words)).mkString(" "))
  }
}

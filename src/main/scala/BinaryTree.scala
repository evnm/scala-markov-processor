package com.evnm.markovprocessor

import scala.util.Random

sealed abstract class BinaryTree[+T]
case object EmptyNode extends BinaryTree[Nothing]

case class Node[T](val value: T,
                   val left: BinaryTree[T],
                   val right: BinaryTree[T]
                 ) extends BinaryTree[T] with Iterable[T] {
  def iterator: Iterator[T] = {
    def inner(node: BinaryTree[T]): List[T] = {
      node match {
        case Node(value, left, right) => inner(left) ::: List(value) ::: inner(right)
        case _ => Nil
      }
    }
    inner(this).toIterator
  }
}


/*
 * Node companion object for Ngram trees.
 */
object Node {
  /*
   * Inserts a leading words/completion word combination into a tree of Ngrams.
   */
  def insert(node: BinaryTree[Ngram], leading_words: List[String],
             completion_word: String): Node[Ngram] = {
    node match {
      case EmptyNode =>
        Node(new Ngram(leading_words, Map(completion_word -> 1)), EmptyNode, EmptyNode)
      case Node(value, left, right) =>
        stringListCompare(leading_words, value.leading_words) match {
          case -1 =>
            Node(value, insert(left, leading_words, completion_word), right)
          case 0 =>
            Node(value + completion_word, left, right)
          case 1 => Node(value, left, insert(right, leading_words, completion_word))
        }
      case _ => throw new RuntimeException // Should never get here.
    }
  }


  /*
   * Inserts a list of (leading words, completion word) tuples into an
   * initially-empty Ngram tree.
   */
  def insertList(lst: List[(List[String], String)]): BinaryTree[Ngram] = lst match {
    case Nil => EmptyNode
    case (leading_words, completion_word) :: tail =>
      insert(insertList(tail), leading_words, completion_word)
  }


  /*
   * Finds a given list of leading words within an Ngram tree.
   */
  def find(lst: List[String], tree: BinaryTree[Ngram]): Option[Ngram] = {
    def traverse(node: BinaryTree[Ngram]): Option[Ngram] = {
      node match {
        case EmptyNode => None
        case Node(ngram, left, right) => {
          stringListCompare(lst, ngram.leading_words) match {
            case -1 => traverse(left)
            case 0 => Some(ngram)
            case 1 => traverse(right)
          }
        }
      }
    }

    traverse(tree)
  }


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

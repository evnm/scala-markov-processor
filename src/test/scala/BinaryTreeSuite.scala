package com.evnm.markovprocessor

import org.scalatest.FunSuite

class BinaryTreeSuite extends FunSuite {
  /*
   * Test basic Node functionality.
   */
  test("Node should have passed value") {
    val node = Node(42, EmptyNode, EmptyNode)
    assert(node.value == 42)
  }

  test("Node should have passed left child") {
    val left = Node(24, EmptyNode, EmptyNode)
    val node = Node(13, left, EmptyNode)
    assert(node.left == left)
  }

  test("Node should have passed right child") {
    val right = Node(24, EmptyNode, EmptyNode)
    val node = Node(13, EmptyNode, right)
    assert(node.right == right)
  }


  /*
   * Test insertion.
   */
  test("Inserting into an empty tree should yield a single-element tree") {
    val lst = List("foo", "bar")
    val node = Node.insert(EmptyNode, lst, "baz")
    assert(node.value.leading_words == lst)
    assert(node.value.count == 1)
    assert(node.value.choices == Map("baz" -> 1))
    assert(node.left == EmptyNode)
    assert(node.right == EmptyNode)
  }

  test("Inserting the same words twice should increment the appropriate count") {
    val lst = List("foo", "bar")
    var node = Node.insert(EmptyNode, lst, "baz")
    node = Node.insert(node, lst, "baz")
    assert(node.value.leading_words == lst)
    assert(node.value.count == 2)
    assert(node.value.choices == Map("baz" -> 2))
    assert(node.left == EmptyNode)
    assert(node.right == EmptyNode)
  }

  test("Inserting differing word lists should result in multiple nodes") {
    val lst1 = List("foo", "bar")
    val lst2 = List("what", "the")
    var node = Node.insert(EmptyNode, lst1, "baz")
    node = Node.insert(node, lst2, "frak")
    assert(node.value.leading_words == lst1)
    assert(node.value.count == 1)
    assert(node.value.choices == Map("baz" -> 1))

    assert(node.left == EmptyNode)
    node.right match {
      case Node(ngram, left, right) => {
        assert(ngram.leading_words == lst2)
        assert(ngram.count == 1)
        assert(ngram.choices == Map("frak" -> 1))
        assert(left == EmptyNode)
        assert(right == EmptyNode)
      }
      case _ => assert(false, "node.right should not be empty")
    }
  }

  test("Inserting a list should build an appropriate node") {
    val lst = List((List("foo"), "bar"), (List("bar"), "baz"), (List("baz"), "foo"))
    Node.insertList(lst) match {
      case Node(ngram, left, right) => {
        assert(ngram.leading_words == List("baz"))
        assert(ngram.count == 1)
        assert(ngram.choices == Map("foo" -> 1))

        left match {
          case Node(lngram, lleft, lright) => {
            assert(lngram.leading_words == List("bar"))
            assert(lngram.count == 1)
            assert(lngram.choices == Map("baz" -> 1))
            assert(lleft == EmptyNode)
            assert(lright == EmptyNode)
          }
          case _ => assert(false, "node.left should not be empty")
        }

        right match {
          case Node(rngram, rleft, rright) => {
            assert(rngram.leading_words == List("foo"))
            assert(rngram.count == 1)
            assert(rngram.choices == Map("bar" -> 1))
            assert(rleft == EmptyNode)
            assert(rright == EmptyNode)
          }
          case _ => assert(false, "node.right should not be empty")
        }
      }
      case _ => assert(false, "node should not be empty")
    }
  }


  /*
   * Test stringListCompare.
   */
  test("stringListCompare should return 0 for equal lists") {
    assert(Node.stringListCompare(List("foo", "bar", "baz"), List("foo", "bar", "baz")) == 0)
  }

  test("stringListCompare should return -1 if first list is 'less than' second") {
    assert(Node.stringListCompare(List("bar", "baz"), List("foo", "bar")) == -1)
  }

  test("stringListCompare should return 1 if first list is 'greater than' second") {
    assert(Node.stringListCompare(List("foo", "vermont"), List("foo", "bar")) == 1)
  }
}

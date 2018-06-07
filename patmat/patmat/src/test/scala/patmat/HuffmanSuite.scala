package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("times, counts the frequency of chars in a List") {
    val s = "hello world".toList
    val l = times(s)

    assert(l.contains(('h', 1)) === true)
    assert(l.contains(('e', 1)) === true)
    assert(l.contains(('l', 3)) === true)
    assert(l.contains(('o', 2)) === true)
    assert(l.contains(('w', 1)) === true)
    assert(l.contains(('r', 1)) === true)
    assert(l.contains(('d', 1)) === true)
    assert(l.contains((' ', 1)) === true)
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton, checks if the length of the list equals 1") {
    new TestTrees {
      assert(singleton(List(t1)))

      val l = makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
      assert(!singleton(l))
    }
  }

  test("sortByWeights, sorts a list of CodeTree by ascending weight") {
    val fork = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val trees = List(fork, Leaf('a', 2), Leaf('b', 3))
    val expected = List(Leaf('a', 2), Leaf('b', 3), fork)
    assert(sortByWeights(trees) === expected)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("until, does a thing to a list of CodeTree until a predicate is true") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val l = until(singleton, combine)(leaflist)
    val expected = List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7))
    assert(l === expected)
  }

  test("createCodeTree, does what it says on the tin") {
    val c = "ettxxxx".toList
    val l = createCodeTree(c)
    val expected = Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',4),List('e', 't', 'x'),7)
    assert(l === expected)
  }

  test("decode, should take a bit string and return the decode character string") {
    new TestTrees {
      assert(decode(t2, List(0,0, 1)) === List('a', 'd'))
    }
  }

  test("decoded secret") {
    assert(decodedSecret === List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l'))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode with longer text") {
    val s = "huffmanestcool".toList
    val e = encode(frenchCode)(s)
    assert(e === secret)
    val d = decode(frenchCode, e)
    assert(d === s)
  }

  test("codeBits, should encode a single char") {
    val t: CodeTable = List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1)))
    assert(codeBits(t)('a') === List(0,0))
    assert(codeBits(t)('b') === List(0,1))
    assert(codeBits(t)('d') === List(1))
  }

  test("quickEncode, should also encode a string") {
    val s = "huffmanestcool".toList
    val e = quickEncode(frenchCode)(s)
    assert(e === secret)
  }

}

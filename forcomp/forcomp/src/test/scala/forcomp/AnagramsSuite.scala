package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }


  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: order") {
    val x = List(('e', 1), ('d', 1), ('m', 1), ('n', 1), ('t', 1))
    val y = List(('d', 1))
    val expected = List(('e', 1), ('m', 1), ('n', 1), ('t', 1))
    assert(subtract(x, y) === expected)
  }

  test("subtract: empty") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard, lard).isEmpty)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: a only") {
    val a = List(('a', 1))
    val acomb = List(
      List(),
      List(('a', 1))
    )
    assert(combinations(a).toSet === acomb.toSet)
  }

  test("combinations: ab only") {
    val ab = List(('a', 1), ('b', 1))
    val abcomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('a', 1), ('b', 1))
    )
    assert(combinations(ab).toSet === abcomb.toSet)
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: icy") {
    val icy = List(('i', 1), ('c', 1), ('y', 1))
    val icycomb = Set(
      Set(),
      Set(('i', 1)),
      Set(('c', 1)),
      Set(('y', 1)),
      Set(('i', 1), ('c', 1)),
      Set(('i', 1), ('y', 1)),
      Set(('c', 1), ('y', 1)),
      Set(('i', 1), ('c', 1), ('y', 1))
    )
    val result = combinations(icy).map(_.toSet).toSet
    assert(result === icycomb)
  }

  test("take 1") {
    val abba = List(('a', 2), ('b', 2))
    val expected = List(('a', 1))
    assert(take(abba, 1).toSet === expected.toSet)
  }

  test("take 2") {
    val abba = List(('a', 2), ('b', 2))
    val expected = List(('a', 2))
    assert(take(abba, 2).toSet === expected.toSet)
  }

  test("take 3") {
    val abba = List(('a', 2), ('b', 2))
    val expected = List(('a', 2), ('b', 1))
    assert(take(abba, 3).toSet === expected.toSet)
  }

  test("drop 1") {
    val abba = List(('a', 2), ('b', 2))
    val expected = List(('a', 1), ('b', 2))
    assert(drop(abba, 1).toSet === expected.toSet)
  }

  test("drop 2") {
    val abba = List(('a', 2), ('b', 2))
    val expected = List(('b', 2))
    assert(drop(abba, 2).toSet === expected.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  test("sentence anagrams: small") {
    val sentence = List("icy")
    assert(sentenceAnagrams(sentence).toSet === Set(sentence))
  }

}

package patmat

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val t3 = Fork(Leaf('l',3),Fork(Leaf('o',2),Fork(Leaf('d',1),Fork(Leaf('r',1),Fork(Leaf('W',1),Fork(Leaf(' ',1),Fork(Leaf('H',1),Leaf('e',1),List('H', 'e'),2),List(' ', 'H', 'e'),3),List('W', ' ', 'H', 'e'),4),List('r', 'W', ' ', 'H', 'e'),5),List('d', 'r', 'W', ' ', 'H', 'e'),6),List('o', 'd', 'r', 'W', ' ', 'H', 'e'),8),List('l', 'o', 'd', 'r', 'W', ' ', 'H', 'e'),11)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
      assert(weight(t3) === 11)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
      assert(chars(t3) === List('l', 'o', 'd', 'r', 'W', ' ', 'H', 'e'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"hello, world\")") {
    val chars = string2Chars("hello, world")
    assert(times(chars) === List(('h',1), ('e',1), ('l',3), ('o',2), (',',1), (' ',1), ('w',1), ('r',1), ('d',1)))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
    val charFreq = times("Hello World".toList)
    assert(makeOrderedLeafList(charFreq) === List(Leaf('H',1), Leaf('e',1), Leaf(' ',1), Leaf('W',1), Leaf('r',1), Leaf('d',1), Leaf('o',2), Leaf('l',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t3, encode(t3)("HlWlllo ddHrrreee llHH".toList)) === "HlWlllo ddHrrreee llHH".toList)
    }
  }

  test("quick encode") {
    new TestTrees {
      assert(quickEncode(t1)("ab".toList) === List(0, 1))
    }
  }

  test("quick encode a long text") {
    new TestTrees {
      assert(quickEncode(t3)("lW drH".toList) === List(0,1,1,1,1,0,1,1,1,1,1,0,1,1,0,1,1,1,0,1,1,1,1,1,1,0))
    }
  }

  test("normal encode decodeSecret") {
    new TestTrees {
      assert(decode(frenchCode, encode(frenchCode)(decodedSecret)) === decodedSecret)
    }
  }

  test("quick encode decodeSecret") {
    new TestTrees {
      assert(decode(frenchCode, quickEncode(frenchCode)(decodedSecret)) === decodedSecret)
    }
  }

  test("decode with quickEncode") {
    new TestTrees {
      assert(decode(t3, quickEncode(t3)("HlWlllo ddHrrreee llHH".toList)) === "HlWlllo ddHrrreee llHH".toList)
    }
  }

  test("create code tree") {
    assert(createCodeTree("axxbbccc".toList) === Fork(Leaf('c',3),Fork(Leaf('b',2),Fork(Leaf('a',1),Leaf('x',2),List('a', 'x'),3),List('b', 'a', 'x'),5),List('c', 'b', 'a', 'x'),8))
  }


}

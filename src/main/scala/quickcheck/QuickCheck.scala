package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("meld - removing first min element returns non empty heap") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(a + 1, empty)
    val h12 = meld(h1, h2)
    isEmpty(deleteMin(h12)) == false
  }

  property("meld - heap will maintain correct order of elements when melded") = forAll { (a: Int, b: Int) =>
    val h = meld(insert(a, empty), insert(b, empty))
    val firstHeapMin = findMin(h)
    val secondHeapMin = findMin(deleteMin(h))
    firstHeapMin == Math.min(a, b) && secondHeapMin == Math.max(a, b)
  }

  property("insert - heap will maintain order of elements when inserted") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val firstHeapMin = findMin(h)
    val secondHeapMin = findMin(deleteMin(h))
    firstHeapMin == Math.min(a, b) && secondHeapMin == Math.max(a, b)
  }

  property("melding empty heap results with same heap") = forAll { (h: H) =>
    meld(h, empty) == h
  }

  property("all elements are sequenced in order") = forAll { (h: H) =>
    def heapElemSeq(heap: H, seq: Seq[A]): Seq[A] = {
      if (isEmpty(heap)) seq
      else heapElemSeq(deleteMin(heap), seq :+ findMin(heap))
    }

    val seq = heapElemSeq(h, Seq())
    seq == seq.sorted
  }

  property("delete min from single element heap results in empty heap") =
    forAll { a: Int =>
      val h1 = insert(a, empty)
      deleteMin(h1) == empty
    }

  property("melding non empty heap 3 times and deleting 3 mins, next min of melded heap and original heap are equal") =
    forAll { (a: Int, b: Int, c: Int, d: Int) =>
      val h = insert(a,insert(b, insert(c, insert(d, empty))))
      val hm = meld(meld(h, h), h)
      val h1 = deleteMin(deleteMin(deleteMin(hm)))
      val h2 = deleteMin(h)
      findMin(h1) == findMin(h2)
    }
}

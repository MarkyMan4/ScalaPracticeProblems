/*
 *  To run from command line:
 *      1. cd into practice directory
 *      2. sbt console
 *      3. import problems.Solutions
 *      4. call methods individually, e.g. Solutions.penultimate(List(1,2,3,4))
 */

package problems

object Solutions extends App {

    /* P01 - find the last element in a list
     *
     * scala> last(List(1, 1, 2, 3, 5, 8))
     * res0: Int = 8
     */
    def last(xs: List[Int]) : Int = {
        if(xs.tail == Nil) {
            return xs.head
        }
        
        return last(xs.tail)
    }

    /* P02 - find the second to last element of a list
     *
     * scala> penultimate(List(1, 1, 2, 3, 5, 8))
     * res0: Int = 5
     */
    def penultimate(xs: List[Int]) : Int = {
        if(xs.tail.tail == Nil) {
            return xs.head
        }

        return penultimate(xs.tail)
    }

    /* P03 - find the kth element of a list
     * if n > length of list, return last element
     *
     * scala> nth(2, List(1, 1, 2, 3, 5, 8))
     * res0: Int = 2
     */
    def nth(n: Int, xs: List[Int]) : Int = {
        if(n <= 0) {
            return xs.head
        }
        if(xs.tail == Nil) {
            return xs.head
        }

        return nth(n - 1, xs.tail)
    }

    /* P04 - find the number of elements of a list
     *
     * scala> length(List(1, 1, 2, 3, 5, 8))
     * res0: Int = 6
     */
    def length[A](xs: List[A]) : Int = {
        if(xs.tail == Nil) {
            return 1
        }

        return 1 + length(xs.tail)
    }

    /* P05 - reverse a list
     *
     * scala> reverse(List(1, 1, 2, 3, 5, 8))
     * res0: List[Int] = List(8, 5, 3, 2, 1, 1)
     */
    def reverse(xs: List[Int]) : List[Int] = {
        if(xs.tail == Nil) {
            return xs
        }

        return reverse(xs.tail) :+ xs.head
    }

    /* P06 - find out whether a list is a palindrome
     *
     * scala> isPalindrome(List(1, 2, 3, 2, 1))
     * res0: Boolean = true
     */
    def isPalindrome(xs: List[Int]) : Boolean = {
        if(xs.isEmpty || xs.tail == Nil) {
            return true
        }
        if(xs.head != last(xs)) {
            return false
        }

        return isPalindrome(removeLast(xs.tail))
    }

    // helper function for P06 to remove last element from list
    def removeLast(xs: List[Int]) : List[Int] = {
        if(xs.tail == Nil) {
            return List()
        }

        return xs.head +: removeLast(xs.tail)
    }

    /* P07 - flatten a nested list structure
     *
     * scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
     * res0: List[Any] = List(1, 1, 2, 3, 5, 8)
     */
    def flatten(xs: List[Any]) : List[Any] = xs flatMap {
        case x: List[Any] => flatten(x)
        case y => List(y)
    }

    /* P08 - eliminate consecutive duplicates of list elements
     *
     * scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     * res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
     */
    def compress[A](xs: List[A]) : List[A] = {
        if(xs.isEmpty) {
            return xs
        }

        return xs.head :: compress(xs.tail.dropWhile(_ == xs.head))
    }

    // other options for compress

    // def compress(xs: List[Symbol]) : List[Symbol] = {
    //     return xs.head +: compressHelper(xs, xs.head)
    // }

    // // Helper function for P08. Takes a list and the last added element so I can
    // // make sure an element doesn't get added if it was the last thing added to the list.
    // def compressHelper(xs: List[Symbol], lastAdded: Symbol): List[Symbol] = {
    //     if(xs.isEmpty) {
    //         return xs
    //     }
    //     if(xs.head == lastAdded) {
    //         return compressHelper(xs.tail, lastAdded)
    //     }
        
    //     return xs.head +: compressHelper(xs.tail, xs.head)
    // }

    // def compressFunctional[A](xs: List[A]) : List[A] = 
    //     xs.foldRight(List[A]()) {(head, tail) =>
    //         if(tail.isEmpty || head != tail.head) {
    //             print(head)
    //             head :: tail
    //         }
    //         else {
    //             tail
    //         }
    //     }

    /* P09 - Pack consecutive duplicates of list elements into sublists.
     *
     * scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     * res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)) 
     */
    def pack[A](xs: List[A]) : List[List[A]] = {
        if(xs.isEmpty) {
            return Nil
        }

        return List(xs.takeWhile(_ == xs.head)) ++ pack(xs.dropWhile(_ == xs.head))
    }

    /* P10 - Run-length encoding of a list.
     *
     * Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
     * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
     *
     * scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
     */
     def encode[A](xs: List[A]) : List[(Int, A)] = {
         return pack(xs) map {x => (length(x), x.head)}
     }

    /* P11 - Modified run-length encoding.
     *
     * Modify the result of problem P10 in such a way that if an element has no duplicates it is simply 
     * copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
     *
     * scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     * res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
     */
    def encodeModified[A](xs: List[A]) : List[Any] = {
        return encode(xs) map {
            x => {
                if(x._1 == 1) x._2
                else x
            }
        }
    }

    /* P12 - Decode a run-length encoded list.
     *
     * Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
     *
     * scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
     * res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
     */
    def decode[A](xs: List[(Int, A)]) : List[A] = xs flatMap {
        x => List.fill(x._1)(x._2)
    }
    
    /* P13 - Decode a run-length encoded list.
     *
     * Implement the so-called run-length encoding data compression method directly. 
     * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
     *
     * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
     * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
     */
    def encodeDirect[A](xs: List[A]) : List[(Int, A)] = {
        if(xs.isEmpty) {
            return Nil
        }

        return (xs.takeWhile(_ == xs.head).size, xs.head) +: encodeDirect(xs.dropWhile(_ == xs.head))
    }

    /* P14 - Duplicate the elements of a list.
     *
     * scala> duplicate(List('a, 'b, 'c, 'c, 'd))
     * res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
     */
    def duplicate[A](xs: List[A]) : Any = xs flatMap {
        x => x +: List(x)
    }

    /* P15 - Duplicate the elements of a list a given number of times.
     *
     * scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
     * res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
     */
    def duplicateN[A](n: Int, xs: List[A]) : List[A] = xs flatMap {
        x => List.fill(n)(x)
    }

    /* P16 - Drop every Nth element from a list.
     *
     * scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     * res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
     */
    def drop[A](n: Int, xs: List[A]) : List[A] = {
        if(xs.isEmpty) {
            return Nil
        }

        return xs.take(n - 1) ++ drop(n, xs.drop(n))
    }

    /* P17 - Split a list into two parts.
     *
     * scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     * res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     */
    def split[A](n: Int, xs: List[A]) : List[List[A]] = {
        List(xs.take(n), xs.drop(n))
    }

    /* P18 - Extract a slice from a list.
     *
     * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element 
     * up to but not including the Kth element of the original list. Start counting the elements with 0.
     *
     * scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     * res0: List[Symbol] = List('d, 'e, 'f, 'g)
     */
    def slice[A](i: Int, k: Int, xs: List[A]) : List[A] = {
        xs.drop(i).take(k - i)
    }

    /* P19 - Rotate a list N places to the left.
     *
     * scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     * res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
     *
     * scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
     * res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
     */
    def rotate[A](n: Int, xs: List[A]) : List[A] = {
        if(n >= 0) {
            xs.drop(n) ++ xs.take(n)
        }
        else {
            xs.reverse.take(n.abs).reverse ++ xs.reverse.drop(n.abs).reverse
        }
    }
    
    /* P20 - Remove the Kth element from a list.
     *
     * Return the list and the removed element in a Tuple. Elements are numbered from 0.
     *
     * scala> removeAt(1, List('a, 'b, 'c, 'd))
     * res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
     */
    def removeAt[A](n: Int, xs: List[A]) : (List[A], A) = {
        if(n < 0) {
            throw new NoSuchElementException
        }

        (xs.take(n) ++ xs.drop(n + 1), xs.drop(n).head)
    }
        
    /* P21 - Insert an element at a given position into a list.
     *
     * scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
     * res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
     */
    def insertAt[A](element: A, index: Int, xs: List[A]) : List[A] = {
        if(index < 0) {
            throw new NoSuchElementException
        }

        (xs.take(index) :+ element) ++ xs.drop(index)
    }

    /* P22 - Create a list containing all integers within a given range.
     *
     * scala> range(4, 9)
     * res0: List[Int] = List(4, 5, 6, 7, 8, 9)
     */
    def range(x: Int, y: Int) : List[Int] = {
        (x to y).toList
    }

    /* P23 - Extract a given number of randomly selected elements from a list.
     *
     * scala> randomSelect(3, List('a, 'b, 'c, 'd, 'e', 'f, 'g, 'h))
     * res0: List[Symbol] = List('e, 'd, 'a)
     */
    def randomSelect[A](n: Int, xs: List[A]) : List[A] = {
        if(n <= 0 || xs.isEmpty) {
            return Nil
        }

        val num = scala.util.Random.nextInt(xs.size)
        var removed = removeAt(num, xs)
        return removed._2 :: randomSelect(n - 1, removed._1)
    }

    /* P24 - Lotto: Draw N different random numbers from the set 1..M.
     *
     * scala> lotto(6, 49)
     * res0: List[Int] = List(23, 1, 17, 33, 21, 37)
     */
    def lotto(x: Int, y: Int) : List[Int] = {
        randomSelect(x, (1 to y).toList)
    }

    /* P25 - Generate a random permutation of the elements of a list.
     *
     * scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
     * res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
     */
    def randomPermute[A](xs: List[A]) : List[A] = {
        randomSelect(xs.size, xs)
    }
}

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

        return xs.head :: pack(xs.tail.dropWhile(_ == xs.head))
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
         return pack(xs) flatMap {

         }
     }
}

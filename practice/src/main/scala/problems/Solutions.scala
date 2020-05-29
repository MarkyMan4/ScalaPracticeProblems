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
    def length(xs: List[Int]) : Int = {
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
    def compress(xs: List[Symbol]) : List[Symbol] = {
        var i = 0;
        val compressed: List[Symbol] = List(xs.head)
        
        for(i <- length(xs)) {
            if(i != 0) {

            }
        }
    }
}

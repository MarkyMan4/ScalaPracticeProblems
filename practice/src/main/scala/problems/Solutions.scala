/*
 *  To run from command line:
 *      1. cd into practice directory
 *      2. sbt console
 *      3. import problems.Solutions
 *      4. call methods individually, e.g. Solutions.penultimate(List(1,2,3,4))
 */

package problems

object Solutions extends App {

    // P01 - find the last element in a list
    def last(xs: List[Int]) : Int = {
        if(xs.tail == Nil) {
            return xs.head
        }
        
        return last(xs.tail)
    }

    // P02 - find the second to last element of a list
    def penultimate(xs: List[Int]) : Int = {
        if(xs.tail.tail == Nil) {
            return xs.head
        }

        return penultimate(xs.tail)
    }

    // P03 - find the kth element of a list
    // if n > length of list, return last element
    def nth(n: Int, xs: List[Int]) : Int = {
        if(n <= 0) {
            return xs.head
        }
        if(xs.tail == Nil) {
            return xs.head
        }

        return nth(n - 1, xs.tail)
    }

    // P04 - find the number of elements of a list
    def length(xs: List[Int]) : Int = {
        if(xs.tail == Nil) {
            return 1
        }

        return 1 + length(xs.tail)
    }

    // P05 - reverse a list
    def reverse(xs: List[Int]) : List[Int] = {
        if(xs.tail == Nil) {
            return xs
        }

        return reverse(xs.tail) :+ xs.head
    }
}

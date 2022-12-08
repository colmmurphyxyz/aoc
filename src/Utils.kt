import java.io.File
import java.math.BigInteger
import java.security.MessageDigest
import java.util.*

/**
 * Reads lines from the given input txt file.
 */
fun readInput(name: String) = File("src", "$name.txt")
    .readLines()

/**
 * turns a stack upside-down
 */
fun <T> Stack<T>.invert(): Stack<T> {
    val invertedStack = Stack<T>()
    while (this.isNotEmpty()) {
        invertedStack.push(this.pop())
    }
    return invertedStack
}

inline fun <reified T> moveNItems(from: Stack<T>, to: Stack<T>, n: Int): Stack<T> {
    val tempStack = Array<T?>(n) { null }
    repeat (n) {
        tempStack[it] = from.pop()
    }
    for (i in (n - 1) downTo 0) {
        to.push(tempStack[i])
    }
    return to
}

fun <T> Stack<T>.printStack() {
    while (this.isNotEmpty()) {
        print(" ${this.pop()} ")
    }
    print("\n")
}

fun <T> Collection<Stack<T>>.printStacks() {
    for (stack in this) {
        stack.printStack()
    }
}

/**
 * Converts string to md5 hash.
 */
fun String.md5() = BigInteger(1, MessageDigest.getInstance("MD5").digest(toByteArray()))
    .toString(16)
    .padStart(32, '0')

/**
 * swap two elements of an IntArray at index i and j
 */
fun IntArray.swap(i: Int, j: Int) {
    this[i] = this[i] + this[j]
    this[j] = this[i] - this[j]
    this[i] = this[i] - this[j]
}
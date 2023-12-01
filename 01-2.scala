/*
Advent of Code 2023, day 1, part 2

...
It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

Equipped with this new information, you now need to find the real first and last digit on each line. For example:

two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen

In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.

What is the sum of all of the calibration values?
*/

import scala.io.StdIn
import scala.annotation.tailrec

val DIGITS_MAP = Map(
    "one" -> '1',
    "two" -> '2',
    "three" -> '3',
    "four" -> '4',
    "five" -> '5',
    "six" -> '6',
    "seven" -> '7',
    "eight" -> '8',
    "nine" -> '9',
)

@tailrec  // one long loopy boii
def replaceSpelledOutDigits(acc: String)(rest: String): String =
    if rest.isEmpty then acc
    else
        val spelledOutDigitsBeginning = DIGITS_MAP.keys.filter(rest.startsWith).toList
        spelledOutDigitsBeginning match
            case Nil => 
                replaceSpelledOutDigits(acc + rest.head)(rest.tail)
            case key :: _ =>  // shouldn't be more than one anyway
                val newRest = rest.drop(key.length)
                val replaced = acc + DIGITS_MAP(key)
                replaceSpelledOutDigits(replaced)(newRest)

def replaceSpelledOutDigits(line: String): String =
    replaceSpelledOutDigits("")(line)

@main 
def main() =
    val lines: Array[String] = Iterator
        .continually(StdIn.readLine())
        .takeWhile(_ != null)
        .toArray

    val linesWithDigitsReplaced: Array[String] = lines
        .map(replaceSpelledOutDigits)

    val digits: Array[String] = linesWithDigitsReplaced
        .map(_.filter(_.isDigit))

    val numbersFromFirstAndLastDigit: Array[Int] = digits
        .map(lineDigits =>
            val first = lineDigits.head
            val last = lineDigits.last
            s"$first$last".toInt
        )

    println(numbersFromFirstAndLastDigit.sum)


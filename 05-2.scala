/*

--- Part Two ---
Everyone will starve if you only plant such a small number of seeds. Re-reading the almanac, it looks like the seeds: line actually describes ranges of seed numbers.

The values on the initial seeds: line come in pairs. Within each pair, the first value is the start of the range and the second value is the length of the range. So, in the first line of the example above:

seeds: 79 14 55 13
This line describes two ranges of seed numbers to be planted in the garden. The first range starts with seed number 79 and contains 14 values: 79, 80, ..., 91, 92. The second range starts with seed number 55 and contains 13 values: 55, 56, ..., 66, 67.

Now, rather than considering four seed numbers, you need to consider a total of 27 seed numbers.

In the above example, the lowest location number can be obtained from seed number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77, temperature 45, humidity 46, and location 46. So, the lowest location number is 46.

Consider all of the initial seed numbers listed in the ranges on the first line of the almanac. What is the lowest location number that corresponds to any of the initial seed numbers?
*/

import scala.io.StdIn

object Day5Part2:
    trait KeyValue:
        def contains(key: Long): Boolean
        def apply(key: Long): Long
    
    case class RangedKeyValue(val keyStart: Long, val valueStart: Long, val length: Long) extends KeyValue:
        def contains(key: Long): Boolean = key >= keyStart && key < keyStart + length
        def apply(key: Long): Long = valueStart + (key - keyStart)
    
    object DefaultKeyValueRange extends KeyValue:
        def contains(key: Long): Boolean = true
        def apply(key: Long): Long = key
    
    class RangedMap(val ranges: Seq[RangedKeyValue]):
        def apply(key: Long): Long = ranges
            .find(_.contains(key))
            .getOrElse(DefaultKeyValueRange)(key)
    
    class RangedMapComposition(val maps: Seq[RangedMap]):
        def apply(key: Long): Long = maps.foldLeft(key)((acc, m) => m(acc))
    
    extension (lines: Array[String])
        def parseRangedMap: RangedMap =
            val ranges = lines
                .map(_.split(" ").map(_.toLong))
                .map(x => RangedKeyValue(x(1), x(0), x(2)))
            new RangedMap(ranges)

    case class SeedRange(val start: Long, val length: Long)
            
    @main
    def task05_2() =
        val input = Iterator.continually(StdIn.readLine()).takeWhile(_ != null).mkString("\n")
        val lines = input.split("\n\n")

        val seeds = lines(0).dropWhile(!_.isDigit).split(" ").map(_.toLong)
        val seedRanges = seeds.grouped(2).map(x => SeedRange(x(0), x(1))).toSeq
        
        val seedToSoil = lines(1).split("\n").drop(1).parseRangedMap
        val soilToFertilizer = lines(2).split("\n").drop(1).parseRangedMap
        val fertilizerToWater = lines(3).split("\n").drop(1).parseRangedMap
        val waterToLight = lines(4).split("\n").drop(1).parseRangedMap
        val lightToTemperature = lines(5).split("\n").drop(1).parseRangedMap
        val temperatureToHumidity = lines(6).split("\n").drop(1).parseRangedMap
        val humidityToLocation = lines(7).split("\n").drop(1).parseRangedMap

        val composition = RangedMapComposition(Seq(
            seedToSoil,
            soilToFertilizer,
            fertilizerToWater,
            waterToLight,
            lightToTemperature,
            temperatureToHumidity,
            humidityToLocation
        ))

        val lowestLocation = seedRanges.map(r =>
            (r.start to r.start + r.length - 1).map(composition(_)).min
        ).min

        println(lowestLocation)
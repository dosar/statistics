package com.example.loto

import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

class Strategy1(runResults: Array[RunResult], override val betSizeLimit: Int = 12, override val startFigure: Int = 1,
    override val endFigure: Int = 36) extends MetricsTypes
{
    def withTopNonZeroFigures(pastWindow: Int, skipWindow: Int, betWindow: Int) =
    {
        apply(pastWindow, skipWindow, betWindow)(topNonZeroFiguresGeneric)
    }

    def withTopNonZeroFiguresWithoutNotPopular(pastWindow: Int, skipWindow: Int, betWindow: Int): ArrayBuffer[(Array[Figure], (MaxIntersection, MaxIntersectionCount))] =
    {
        apply(pastWindow, skipWindow, betWindow)(topNonZeroFiguresGeneric)
    }

    def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: Array[RunResult] => Array[Int]) =
    {
        val startIndex = pastWindow
        val sliceSize = skipWindow + betWindow
        var index = startIndex
        val buffer = ArrayBuffer[(Array[Figure], (MaxIntersection, MaxIntersectionCount))]()
        while (index <= runResults.length - sliceSize)
        {
            val pastRrs = runResults.slice(index - pastWindow, index)
            val bet = betGenerator(pastRrs)
            val futureRrs = runResults.slice(index + skipWindow, index + sliceSize)
            val (maxIntersection, maxIntersectionCount) = getIntersections(futureRrs, bet)
            buffer += ((bet, if(maxIntersection == 0) (0, 0) else (maxIntersection, maxIntersectionCount)))
            index += sliceSize
        }
        buffer
    }

    def getIntersections(futureRrs: Array[RunResult], bet: Array[Figure]): (MaxIntersection, MaxIntersectionCount) =
    {
        var ind = 0
        var (maxIntersection, maxIntersectionCount) = (0, 0)
        while(ind < futureRrs.length)
        {
            val rr = futureRrs(ind)
            val intersection = intersectionSize(rr.result, bet)
            if(maxIntersection < intersection)
            {
                maxIntersection = intersection
                maxIntersectionCount = 1
            }
            else if (maxIntersection == intersection)
            {
                maxIntersectionCount += 1
            }
            ind += 1
        }
        (maxIntersection, maxIntersectionCount)
    }
}

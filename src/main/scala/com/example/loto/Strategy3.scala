package com.example.loto

import com.example.loto.metrics.MetricsTypes
import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

/*
* перестаем ставить по старой ставке если получили хит на 5 чисел
* */
class Strategy3(runResults: Array[RunResult], override val betSizeLimit: Int = 7, override val startFigure: Int = 1,
    override val endFigure: Int = 36)
extends MetricsTypes
{
    def withTopNonZeroFigures(pastWindow: Int, skipWindow: Int, betWindow: Int) =
    {
        apply(pastWindow, skipWindow, betWindow)(topNonZeroFiguresGeneric)
    }

    def withTopNonZeroFiguresWithoutNotPopular(pastWindow: Int, skipWindow: Int, betWindow: Int) =
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
            val (maxIntersection, maxIntersectionCount, indexIncrement) = getIntersections(futureRrs, bet)
            buffer += ((bet, if(maxIntersection == 0) (0, 0) else (maxIntersection, maxIntersectionCount)))
            index += indexIncrement
        }
        buffer
    }

    def getIntersections(futureRrs: Array[RunResult], bet: Array[Int]): (MaxIntersection, MaxIntersectionCount, SliceSize) =
    {
        var ind = 0
        var (maxIntersection, maxIntersectionCount) = (0, 0)
        while(ind < futureRrs.length)
        {
            val rr = futureRrs(ind)
            val intersection = intersectionSize(rr.result, bet)
            if(intersection == 5) return (5, 1, ind + 1)
            else if(maxIntersection < intersection)
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
        (maxIntersection, maxIntersectionCount, ind)
    }
}

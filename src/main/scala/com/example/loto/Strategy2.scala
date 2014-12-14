package com.example.loto

import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

class Strategy2(runResults: Vector[RunResult], override val topFiguresCount: Int = 12,
    override val startFigure: Int = 1, override val endFigure: Int = 36) extends MetricsTypes
{
    type PreviousRR = RunResult
    def strategy2(pastWindow: Int, skipWindow: Int, betWindow: Int)(extractor: Vector[RunResult] => Array[(Figure, HitCount)])(
        betGenerator: (Array[(Figure, HitCount)], PreviousRR) => Array[Figure]) =
    {
        val startIndex = pastWindow
        val sliceSize = skipWindow + betWindow
        var index = startIndex
        val buffer = ArrayBuffer[(Seq[Figure], (MaxIntersection, MaxIntersectionCount))]()
        val runResultsWithIndex = runResults.zipWithIndex
        while (index <= runResults.length - sliceSize)
        {
            val pastRrs = runResults.slice(index - pastWindow, index)
            val betCandidate = extractor(pastRrs)
            val futureRrs = runResultsWithIndex.slice(index + skipWindow, index + sliceSize)
            val (maxIntersection, maxIntersectionCount) = getIntersections(futureRrs, betCandidate)(betGenerator)
            buffer += ((betCandidate.map(_._1).toSeq, if(maxIntersection == 0) (0, 0) else (maxIntersection, maxIntersectionCount)))
            index += sliceSize
        }
        buffer
    }

    def getIntersections(futureRrs: Vector[(RunResult, Int)], betCandidate: Array[(Figure, HitCount)])(
        betGenerator: (Array[(Figure, HitCount)], PreviousRR) => Array[Figure]): (MaxIntersection, MaxIntersectionCount) =
    {
        var ind = 0
        var (maxIntersection, maxIntersectionCount) = (0, 0)
        while(ind < futureRrs.length)
        {
            val (rr, globalIndex) = futureRrs(ind)
            val bet = betGenerator(betCandidate, futureRrs(ind - 1)._1)
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

package com.example.loto

import com.example.loto.model.RunResult

import scala.collection.mutable.ArrayBuffer

/*
* перестаем ставить по старой ставке если получили хит на 5 чисел
* */
class Strategy4(runResults: Vector[RunResult], override val topFiguresCount: Int = 7, override val startFigure: Int = 16,
    override val endFigure: Int = 36)
extends MetricsTypes
{
    def withTopNonZeroFigures(pastWindow: Int, skipWindow: Int, betWindow: Int) =
    {
        apply(pastWindow, skipWindow, betWindow)(topNonZeroFigures)
    }

    def withTopNonZeroFiguresWithoutNotPopular(pastWindow: Int, skipWindow: Int, betWindow: Int) =
    {
        apply(pastWindow, skipWindow, betWindow)(topNonZeroFiguresWithoutNotPopular)
    }

    def apply(pastWindow: Int, skipWindow: Int, betWindow: Int)(betGenerator: Vector[RunResult] => Array[Figure]) =
    {
        val startIndex = pastWindow
        val sliceSize = skipWindow + betWindow
        var index = startIndex
        val buffer = ArrayBuffer[(Array[Figure], (IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus))]()
        while (index <= runResults.length - sliceSize)
        {
            val pastRrs = runResults.slice(index - pastWindow, index)
            val bet = betGenerator(pastRrs)
            val futureRrs = runResults.slice(index + skipWindow, index + sliceSize)
            val (statistics, indexIncrement) = getIntersectionStatistics(futureRrs, bet)
            buffer += ((bet, statistics))
            index += indexIncrement
        }
        buffer
    }

    type SliceSize = Int
    type IntersectionCount2 =Int; type IntersectionCount3 = Int; type IntersectionCount4 = Int; type IntersectionCount5 = Int
    type MoneyPlus = Int; type MoneyMinus = Int
    def getIntersectionStatistics(futureRrs: Vector[RunResult], bet: Array[Figure]):
        ((IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus), SliceSize) =
    {
        var ind = 0
        var (i2, i3, i4, i5, mplus, mminus) = (0, 0, 0, 0, 0, 0)
        val betSize = bet.size
        while(ind < futureRrs.length)
        {
            val rr = futureRrs(ind)
            val intersection = intersectionSize(rr.result, bet)
            if(intersection == 5) return ((i2, i3, i4, 1, 1000000 + mplus, mminus), ind + 1)
            else if(intersection == 2) i2 += 1
            else if(intersection == 3) i3 += 1
            else if(intersection == 4) i4 += 1
            if(intersection > 1) mplus += betWon(betSize, intersection)
            else mminus += betCost(betSize)
            ind += 1
        }
        ((i2, i3, i4, i5, mplus, mminus), ind)
    }

    def betCost(betSize: Int) =
    {
        if(betSize == 5) 30
        else if(betSize == 6) 180
        else if(betSize == 7) 630
        else if(betSize == 8) 1680
        else if(betSize == 9) 3780
        else if(betSize == 10) 7560
        else if(betSize == 11) 13860
        else if(betSize == 12) 23760
        else 0
    }

    def betWon(betSize: Int, intersectionSize: Int) =
    {
        if(betSize == 5)
        {
            if(intersectionSize == 2) 30
            else if (intersectionSize == 3) 300
            else if (intersectionSize == 4) 3000
            else 0
        }
        else if(betSize == 6)
        {
            if(intersectionSize == 2) 120
            else if (intersectionSize == 3) 990
            else if (intersectionSize == 4) 7200
            else 0
        }
        else if(betSize == 7)
        {
            if(intersectionSize == 2) 300
            else if (intersectionSize == 3) 2160
            else if (intersectionSize == 4) 12780
            else 0
        }
        else if(betSize >= 8)
        {
            if(intersectionSize == 2) 600
            else if (intersectionSize == 3) 3900
            else if (intersectionSize == 4) 19920
            else 0
        }
        else 0
    }
}

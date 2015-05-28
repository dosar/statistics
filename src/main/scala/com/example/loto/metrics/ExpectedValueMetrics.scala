package com.example.loto.metrics

import com.example.loto.array.ArrayPerformanceUtil._
import com.example.loto.model.RunResult
import com.example.loto.sorter.FiguresByHitSorter

/**
 * Created by alespuh on 20.02.15.
 */
trait ExpectedValueMetrics
{
    this: MetricsTypes =>

    def betCount: Int

    def topFromStatOneBet(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        val figures = FiguresByHitSorter.topFigures(figuresOccurencies(rrs))
        val result = new Array[Int](betSizeLimit)
        var ind = 0
        while(ind < figures.length)
        {
            val figure = figures(ind)
            val position = getPosition(figure)
            if(position != -1 && result(position) == 0)
                result(position) = figure
            ind += 1
        }
        result
    }

    def middleFromStatOneBet(rrs: Array[RunResult]): Array[Figure] = checkTallies
    {
        def setFigureInPosition(figures: Array[Int], result: Array[Int], ind: Int) =
        {
            val figure = figures(ind)
            val position = getPosition(figure)
            if (position != -1 && result(position) == 0)
                result(position) = figure
            ind + 1
        }

        val figures = FiguresByHitSorter.topFigures(figuresOccurencies(rrs))
        val result = new Array[Int](betSizeLimit)
        val startInd = (figures.length - betSizeLimit) / 2
        var ind = startInd
        while(ind < figures.length)
            ind = setFigureInPosition(figures, result, ind)
        ind = 0
        while(ind < startInd)
            ind = setFigureInPosition(figures, result, ind)
        result
    }

    private def getPosition(figure: Int): Int =
    {
        if (figure < 7)  0
        else if (figure >= 7 && figure <= 10) 1
        else if (figure >= 12 && figure <= 17) 2
        else if (figure >= 19 && figure <= 24) 3
        else if (figure >= 26 && figure <= 29) 4
        else if (figure > 29) 5
        else -1
    }

    def topFromStatMultiBet(rrs: Array[RunResult]): Array[Figure] =
    {
        val figures = FiguresByHitSorter.topFigures(figuresOccurencies(rrs))
        val result = new Array[Figure](5 * betCount)
        for(figure <- figures)
        {
            for(position <- 0 until 5)
            {
                if(matchPositionInterval(figure, position))
                    setFigureInPosition(result, figure, position)
            }
        }
        new Array[Int](1)
    }

    //position - o-based value
    private def matchPositionInterval(figure: Figure, position: Int) =
        figure <= stat(position * 2 + 1) && figure >= stat(position * 2)

    private def setFigureInPosition(result: Array[Figure], figure: Figure, position: Int) =
    {
//        for(betIndex <- 0 until betCount)
//        {
//            val index =
//        }
    }

    //массив из пяти пар. каждая i-ая пара - интервал рассчитанный из матожидания числа выпадающего в i-юу позицию и дисперсии
    //посчитано теоретически
    lazy val stat = Array(2, 10, 7, 17, 12, 24, 19, 29, 26, 34)
}

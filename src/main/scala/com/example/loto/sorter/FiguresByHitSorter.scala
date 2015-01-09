package com.example.loto.sorter

import com.example.loto.CommonImplicits.Incrementer
import com.example.loto.MoneyHitStatisticsType
import com.example.loto.array.ArrayPerformanceUtil

/**
 * Created by alespuh on 03.01.15.
 */
object FiguresByHitSorter extends MoneyHitStatisticsType
{
    //-1 потому что в figureHits есть неиспользуемый элемент
    def topFigures(figureHits: Array[Int])(implicit take: Int = figureHits.length - 1): Array[Int] =
        topFiguresWithHits(figureHits)._2

    //-1 потому что в figureHits есть неиспользуемый элемент
    def topFiguresWithHits(figureHits: Array[Int])(implicit take: Int = figureHits.length - 1): (Array[HitCount], Array[Figure]) =
    {
        val figures = ArrayPerformanceUtil.createArray(figureHits.length)(i => i)
        new PairArrayHeapSorter(figureHits, figures, take).sort
    }

    //-1 если нужно взять все
    def topFiguresWithFilter(figureHits: Array[Int], filter: HitCount => Boolean)(implicit take: Int = -1): Array[Int] =
    {
        val filteredFigureHits = new Array[Int](figureHits.length)
        val incrementer = new Incrementer(0)
        val filteredFigures = Array.fill(figureHits.length)(incrementer ++)
        var ind = 0
        var filteredInd = 0
        while(ind < figureHits.length)
        {
            if(filter(figureHits(ind)))
            {
                filteredFigureHits(filteredInd) = figureHits(ind)
                filteredFigures(filteredInd) = ind
                filteredInd += 1
            }
            ind += 1
        }
        val filteredTake = if(take == -1) filteredFigureHits.length else take
        new PairArrayHeapSorter(filteredFigureHits, filteredFigures, filteredTake).sort._2
    }
}

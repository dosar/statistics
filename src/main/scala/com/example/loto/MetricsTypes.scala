package com.example.loto

import com.example.loto.CommonImplicits.Incrementer
import com.example.loto.array.ArrayFiller
import com.example.loto.model.RunResult
import com.example.loto.sorter.{FiguresByHitSorter, PairArrayHeapSorter}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by alespuh on 08.12.14.
 */
trait MoneyHitStatisticsType
{
    type SliceSize = Int
    type IntersectionCount2 =Int; type IntersectionCount3 = Int; type IntersectionCount4 = Int; type IntersectionCount5 = Int
    type MoneyPlus = Int; type MoneyMinus = Int
    type Figure = Int
    type HitCount = Int
    type FigureHitsArray = Array[Int]
}

trait MetricsTypes extends MoneyHitStatisticsType
{
    type MaxIntersection = Int
    type MaxIntersectionCount = Int
    type Index = Int

    def topFiguresCount: Int
    def startFigure = 1
    def endFigure = 36

    /*
    * Int = Figure
    * */
    def intersectionSize(rrResult: Array[Int], bet: Array[Int]): Int =
    {
        var ind = 0
        var result = 0
        while(ind < bet.length)
        {
            val figure = bet(ind)
            if(figure == rrResult(0) || figure == rrResult(1) || figure == rrResult(2) || figure == rrResult(3) || figure == rrResult(4))
                result += 1
            ind += 1
        }
        result
    }

    /*
    * сверху цифру, потом пару для нее, и т.д.
    * */
    def piarFiguresOccurencies(rrs: Vector[RunResult], startFigure: Int = startFigure, endFigure: Int = endFigure): mutable.Map[Figure, HitCount] =
    {
        var ind = 0
        val figuresMap = new Array[Int](36)

        while(ind < rrs.length)
        {
            val rrResult = rrs(ind).result
            figuresMap(rrResult(0) - 1) += 1
            figuresMap(rrResult(1) - 1) += 1
            figuresMap(rrResult(2) - 1) += 1
            figuresMap(rrResult(3) - 1) += 1
            figuresMap(rrResult(4) - 1) += 1
            ind += 1
        }
        ind = startFigure - 1
        val result = mutable.Map[Int, Int]() withDefaultValue 0
        while(ind < endFigure)
        {
            result += (ind + 1) -> figuresMap(ind)
            ind += 1
        }
        result
    }

    /*
    * индекс - число, значение - количество хитов. нулевой индекс не используется
    * */
    def figuresOccurencies(rrs: Vector[RunResult], startFigure: Int = startFigure, endFigure: Int = endFigure): Array[HitCount] =
    {
        var ind = 0
        val figuresMap = Array.fill[Int](37)
        {
            val result = if(ind < startFigure || ind > endFigure) -1
            else 0
            ind += 1
            result
        }

        def updateMap(figure: Int) =
        {
            if(figure <= endFigure && figure >= startFigure)
                figuresMap(figure) += 1
        }

        ind = 0
        while(ind < rrs.length)
        {
            val rrResult = rrs(ind).result
            updateMap(rrResult(0))
            updateMap(rrResult(1))
            updateMap(rrResult(2))
            updateMap(rrResult(3))
            updateMap(rrResult(4))
            ind += 1
        }
        figuresMap
    }

    type Min = Int; type Max = Int
    def figureIntervals(rrs: Seq[RunResult]): (Array[Min], Array[Max]) =
    {
        var ind = 0
        val (figureMins, figureMaxs) = (Array.fill[Min](5)(40), new Array[Max](5))

        def updateMinMax(ind: Int, figure: Figure) =
        {
            if(figure < figureMins(ind))
                figureMins(ind) = figure
            if(figure > figureMaxs(ind))
                figureMaxs(ind) = figure
        }

        while(ind < rrs.length)
        {
            val rrResult = rrs(ind).result
            updateMinMax(0, rrResult(0))
            updateMinMax(1, rrResult(1))
            updateMinMax(2, rrResult(2))
            updateMinMax(3, rrResult(3))
            updateMinMax(4, rrResult(4))
            ind += 1
        }
        (figureMins, figureMaxs)
    }

    /*
    * Метрика
    * работаем в предположении, что endFigure = 36, а startFigure = 1
    * figureToIgnores - отсортирован по возрастанию и завершается 0
    * */
    def topNonZeroFiguresExceptSome(rrs: Vector[RunResult], figureToIgnores: Array[Figure]): Array[Figure] =
    {
        val figureHits = new Array[Int](endFigure - startFigure - figureToIgnores.size + 2)
        val figures = ArrayFiller.createFiguresArray(figureToIgnores)
        val figureIndexes = ArrayFiller.createArray(endFigure - startFigure + 2, -1)

        var ind = 0
        while(ind < figures.length)
        {
            figureIndexes(figures(ind)) = ind
            ind += 1
        }

        def updateHits(figure: Int) =
        {
            val figureIndex = figureIndexes(figure)
            if(figureIndex != -1)
                figureHits(figureIndex) += 1
        }

        ind = 0
        while(ind < rrs.length)
        {
            val rrResult = rrs(ind).result
            updateHits(rrResult(0))
            updateHits(rrResult(1))
            updateHits(rrResult(2))
            updateHits(rrResult(3))
            updateHits(rrResult(4))
            ind += 1
        }
        val result = new PairArrayHeapSorter(figureHits, figures, topFiguresCount).sort
//        val result = PairArrayInsertionSorter.sort(figureHits, figures, topFiguresCount)
        result._2
    }

    /*
    * Метрика
    * в том числе с нулями
    * */
    def middleOccurencyFigures(rrs: Vector[RunResult]) =
    {
        val occs = figuresOccurencies(rrs)
        val topFigures = FiguresByHitSorter.topFigures(occs)
        val result = new Array[Int](topFiguresCount)
        var occInd = (topFigures.length - result.length) / 2
        var resultInd = 0
        while(resultInd < result.length)
        {
            result(resultInd) = topFigures(occInd)
            occInd += 1
            resultInd += 1
        }
        result
    }

    /*
    * Метрика
    * */
    def zeroOccurencyFigures(rrs: Vector[RunResult]) =
    {
        val occs = figuresOccurencies(rrs)
        var array = ArrayBuffer[Figure]()
        var figure = startFigure
        while(figure <= endFigure && array.length < topFiguresCount)
        {
            val hits = occs(figure)
            if(hits == 0)
                array += figure
            figure += 1
        }
        array.toArray
    }

    /*
    * Метрика
    * betCandidate должен быть отсортирован в нужном порядке
    * */
    def topNonZeroFiguresWithoutPrevious(betCandidate: Array[(Figure, HitCount)], previousRunResult: RunResult): Array[Figure] =
    {
        val previous = previousRunResult.result
        val buffer = new Array[Figure](topFiguresCount)
        var fi = 0
        var arrInd = 0
        while(fi < betCandidate.length && arrInd < topFiguresCount)
        {
            val (figure, hitCount) = betCandidate(fi)
            val previousContainsFigure =
                figure == previous(0) || figure == previous(1) || figure == previous(2) || figure == previous(3) || figure == previous(4)
            if(hitCount != 0 && !previousContainsFigure)
            {
                buffer(arrInd) = figure
                arrInd += 1
            }
            fi += 1
        }
        buffer
    }
    /*
    * Метрика
    * */
    def topNonZeroFiguresGeneric1(rrs: Vector[RunResult]): Array[Figure] =
    {
        val figureHits = new Array[Int](endFigure - startFigure + 1)
        val incrementer = new Incrementer(startFigure)
        val figures = Array.fill(figureHits.length)(incrementer ++)

        def updateHits(figure: Int) =
        {
            val index = figure - startFigure
            if(index >= 0 && figure <= endFigure) figureHits(index) += 1
        }

        var ind = 0
        while(ind < rrs.length)
        {
            val rrResult = rrs(ind).result
            updateHits(rrResult(0))
            updateHits(rrResult(1))
            updateHits(rrResult(2))
            updateHits(rrResult(3))
            updateHits(rrResult(4))
            ind += 1
        }
        new PairArrayHeapSorter(figureHits, figures, topFiguresCount).sort._2
    }

    /*
    * Метрика
    * */

    def topNonZeroFiguresGeneric(rrs: Vector[RunResult]): Array[Figure] =
    {
        val occs = figuresOccurencies(rrs)
        var list = List[(Figure, HitCount)]()
        var figure = startFigure
        while(figure <= endFigure)
        {
            val hits = occs(figure)
            if(hits > 0)
                list = (figure, hits) :: list
            figure += 1
        }
        val sorted = list.sortBy(- _._2)
        sorted.map(_._1).take(topFiguresCount).toArray
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

    /*
    * Int = Figure
    * */
    def getIntersectionStatistics(futureRrs: Vector[RunResult], bet: Array[Int]):
        ((IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus), SliceSize) =
    {
        var ind = 0
        var (i2, i3, i4, i5, mplus, mminus) = (0, 0, 0, 0, 0, 0)
        val betSize = bet.size
        while(ind < futureRrs.length)
        {
            val rr = futureRrs(ind)
            val intersection = intersectionSize(rr.result, bet)
            if(intersection == 5)
                return ((i2, i3, i4, 1, 1000000 + mplus, mminus), ind + 1)
            else if(intersection == 2) i2 += 1
            else if(intersection == 3) i3 += 1
            else if(intersection == 4) i4 += 1
            if(intersection > 1) mplus += betWon(betSize, intersection)
            else mminus += betCost(betSize)
            ind += 1
        }
        ((i2, i3, i4, i5, mplus, mminus), ind)
    }
}

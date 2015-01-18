package com.example.loto

import com.example.loto.array.ArrayPerformanceUtil
import com.example.loto.betgenerator.FromMiddleOccurenciesBetGenerator
import com.example.loto.model.RunResult
import com.example.loto.sorter.{PairArrayInsertionSorter, FiguresByHitSorter, PairArrayHeapSorter}

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
    type StrategyStatistics = (IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus)
    type Bet = Array[Figure]
    type CombinedBet = Array[Figure]
}

trait MetricsTypes extends MoneyHitStatisticsType
{
    type MaxIntersection = Int
    type MaxIntersectionCount = Int
    type Index = Int

    def betSizeLimit: Int
    def startFigure = 1
    def endFigure = 36

    /*
    * Int = Figure
    * */
    def intersectionSize(rrResult: Array[Int], bet: Array[Int])(implicit betStart: Int = 0, betSize: Int = bet.length): Int =
    {
        var ind = betStart
        var result = 0
        while(ind < betSize)
        {
            val figure = bet(ind)
            if(figure == rrResult(0) || figure == rrResult(1) || figure == rrResult(2) || figure == rrResult(3)
                || figure == rrResult(4)) result += 1
            ind += 1
        }
        result
    }

    /*
    * возвращает массив массивов. представляет собой эмуляцию мэпа 'количество хитов' -> 'список чисел'
    * в каждом внутреннем массиве первый элемент - длина этого массива
    * метод является вспомогательным для fromMiddleOccurencies
    * */
    def backFigureOccurencies(rrs: Array[RunResult]): Array[Array[Figure]] =
    {
        import com.example.loto.array.ArrayPerformanceUtil._
        var figure = 0
        val figureHits = figuresOccurencies(rrs)
        val result = new Array[Array[Figure]](maxForArray(figureHits) + 1)
        while(figure < figureHits.length)
        {
            val hits = figureHits(figure)
            if(hits > -1)
            {
                if(result(hits) == null)
                    result(hits) = new Array[Int](rrs.length)
                safeSetArrayElement(result(hits), result(hits)(0) + 1, figure){ result(hits) = _ }
                result(hits)(0) += 1
            }

            figure += 1
        }
        result
    }

    //индекс - количество хитов, значение - числа выпавшие индекс раз
    //там где для хитов нет чисел - нулл
    def fromMiddleOccurencies(rrs: Array[RunResult]) =
    {
        val figuresByHit = backFigureOccurencies(rrs)
        new FromMiddleOccurenciesBetGenerator(figuresByHit, betSizeLimit, startFigure, endFigure).generate()
    }

    /*
    * индекс - число, значение - количество хитов. нулевой индекс не используется
    * */
    def figuresOccurencies(rrs: Array[RunResult], startFigure: Int = startFigure, endFigure: Int = endFigure): Array[HitCount] =
    {
        val figuresMap = ArrayPerformanceUtil.createArray(37){ ind => if(ind < startFigure || ind > endFigure) -1 else 0 }

        var ind = 0
        while(ind < rrs.length)
        {
            val rrResult = rrs(ind).result
            updateMap(figuresMap, rrResult(0))
            updateMap(figuresMap, rrResult(1))
            updateMap(figuresMap, rrResult(2))
            updateMap(figuresMap, rrResult(3))
            updateMap(figuresMap, rrResult(4))
            ind += 1
        }
        figuresMap
    }

    /*только по одному положению в тираже*/
    def partialFiguresOccurencies(rrs: Array[RunResult], figureInd: Int, startFigure: Int = startFigure, endFigure: Int = endFigure): Array[HitCount] =
    {
        val figuresMap = ArrayPerformanceUtil.createArray(37){ ind => if(ind < startFigure || ind > endFigure) -1 else 0 }

        var ind = 0
        while(ind < rrs.length)
        {
            val rrResult = rrs(ind).result
            updateMap(figuresMap, rrResult(figureInd))
            ind += 1
        }
        figuresMap
    }

    private def updateMap(figuresMap: Array[Int], figure: Int) =
    {
        if(figuresMap(figure) != -1)
            figuresMap(figure) += 1
    }

    type Min = Int; type Max = Int
    def figureIntervals(rrs: Array[RunResult]): (Array[Min], Array[Max]) =
    {
        var ind = 0
        val figureMins = ArrayPerformanceUtil.createArray(5)(x => 40)
        val figureMaxs = new Array[Max](5)

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
    def topNonZeroFiguresExceptSome(rrs: Array[RunResult], figureToIgnores: Array[Figure]): Array[Figure] =
    {
        val figureHits = new Array[Int](endFigure - startFigure - figureToIgnores.size + 2)
        val figures = ArrayPerformanceUtil.createFiguresArray(figureToIgnores)
        val figureIndexes = ArrayPerformanceUtil.createArray(endFigure - startFigure + 2)(i => -1)

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
        val result = new PairArrayHeapSorter(figureHits, figures, betSizeLimit).sort
//        val result = PairArrayInsertionSorter.sort(figureHits, figures, topFiguresCount)
        result._2
    }

    /*
    * Метрика
    * в том числе с нулями и с числами которые фильтруются, если они попадают
    * в середину (непонятно как такое может случиться с хитами в -1)
    * */
    def middleOccurencyFigures(rrs: Array[RunResult]) =
    {
        val topFigures = FiguresByHitSorter.topFigures(figuresOccurencies(rrs))
        val result = new Array[Int](betSizeLimit)
        val occInd = (topFigures.length - result.length) / 2
        scala.compat.Platform.arraycopy(topFigures, occInd, result, 0, betSizeLimit)
        result
    }

    /*
    * Метрика
    * откидываются числа с нулевыми хитами и из оставшегося массива берется середина
    * */
    def trueMiddleOccurencyFigures(rrs: Array[RunResult]): Array[Int] =
    {
        val (topHits, topFigures) = FiguresByHitSorter.topFiguresWithHits(figuresOccurencies(rrs))
        trueMiddleOccurencyFigures(topHits, topFigures)
    }

    protected def trueMiddleOccurencyFigures(topHits: Array[Int], topFigures: Array[Int]): Array[Int] =
    {
        val result = new Array[Int](betSizeLimit)
        val length = ArrayPerformanceUtil.firstNonNegativeMinInd(topHits)
        val occInd = (length - result.length) / 2
        scala.compat.Platform.arraycopy(topFigures, if(occInd < 0) 0 else occInd, result, 0, betSizeLimit)
        result
    }

    /*
    * Метрика
    * */
    def zeroOccurencyFigures(rrs: Array[RunResult]) =
    {
        val occs = figuresOccurencies(rrs)
        var array = ArrayBuffer[Figure]()
        var figure = startFigure
        while(figure <= endFigure && array.length < betSizeLimit)
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
        val buffer = new Array[Figure](betSizeLimit)
        var fi = 0
        var arrInd = 0
        while(fi < betCandidate.length && arrInd < betSizeLimit)
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
    def topNonZeroFiguresGeneric1(rrs: Array[RunResult]): Array[Figure] =
    {
        val occurencies = figuresOccurencies(rrs)
        val figures = ArrayPerformanceUtil.createArray(occurencies.length)(i => i)
        PairArrayInsertionSorter.sort(occurencies, figures)(betSizeLimit)._2
    }

    /*
    * Метрика
    * */
    def topNonZeroFiguresGeneric(rrs: Array[RunResult]): Array[Figure] =
        FiguresByHitSorter.topFigures(figuresOccurencies(rrs))(betSizeLimit)

    //индекс - размер ставки
    val betCost = Array(0, 0, 0, 0, 0, 30, 180, 630, 1680, 3780, 7560, 13860, 23760)

    //первый индекс - размер ставки, индекс внутреннего массива - размер пересечения
    val betWon = Array(
        Array(0, 0, 0, 0, 0), // betSize == 0
        Array(0, 0, 0, 0, 0), // betSize == 1
        Array(0, 0, 0, 0, 0), // betSize == 2
        Array(0, 0, 0, 0, 0), // betSize == 3
        Array(0, 0, 0, 0, 0), // betSize == 4 и т.д.
        Array(0, 0, 30, 300, 3000),
        Array(0, 0, 120, 990, 7200),
        Array(0, 0, 300, 2160, 12780),
        Array(0, 0, 600, 3900, 19920),
        Array(0, 0, 600, 3900, 19920),
        Array(0, 0, 600, 3900, 19920),
        Array(0, 0, 600, 3900, 19920),
        Array(0, 0, 600, 3900, 19920)
    )

    /*
    * Int = Figure
    * */
    def getIntersectionStatistics(futureRrs: Array[RunResult], bet: Array[Int]):
        ((IntersectionCount2, IntersectionCount3, IntersectionCount4, IntersectionCount5, MoneyPlus, MoneyMinus), SliceSize) =
    {
        var ind = 0
        var (i2, i3, i4, i5, mplus, mminus) = (0, 0, 0, 0, 0, 0)
        val betSize = bet.size
        while(ind < futureRrs.length)
        {
            mminus += betCost(betSize)
            val rr = futureRrs(ind)
            val intersection = intersectionSize(rr.result, bet)
            if(intersection == 5)
                return ((i2, i3, i4, 1, 1000000 + mplus, mminus), ind + 1)
            else if(intersection == 2) i2 += 1
            else if(intersection == 3) i3 += 1
            else if(intersection == 4) i4 += 1
            mplus += betWon(betSize)(intersection)
            ind += 1
        }
        ((i2, i3, i4, i5, mplus, mminus), ind)
    }
}

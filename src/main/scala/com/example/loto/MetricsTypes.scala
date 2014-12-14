package com.example.loto

import com.example.loto.model.RunResult

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by alespuh on 08.12.14.
 */
trait MetricsTypes
{
    type MaxIntersection = Int
    type MaxIntersectionCount = Int
    type Figure = Int
    type HitCount = Int
    type Index = Int

    def topFiguresCount: Int
    def startFigure = 17
    def endFigure = 36

    def intersectionSize(rrResult: Array[Figure], bet: Seq[Figure]): Int =
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

    def figuresOccurencies(rrs: Vector[RunResult]): mutable.Map[Figure, HitCount] =
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
    * в том числе с нулями
    * */
    def middleOccurencyFigures(rrs: Vector[RunResult]) =
    {
        val occs = figuresOccurencies(rrs)
        val arr = new Array[(Figure, HitCount)](endFigure - startFigure + 1)
        var figure = startFigure
        while(figure <= endFigure)
        {
            val hits = occs(figure)
            arr(figure - startFigure) = (figure, hits)
            figure += 1
        }
        val sorted = arr.sortBy(- _._2)
        val drop = (arr.length - topFiguresCount) / 2
        sorted.map(_._1).drop(drop).take(topFiguresCount).toArray
    }

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

    def topNonZeroFigures(rrs: Vector[RunResult]): Array[Figure] = topNonZeroFiguresGeneric(rrs, 1, 36)

    def topNonZeroFiguresWithoutNotPopular(rrs: Vector[RunResult]): Array[Figure] = topNonZeroFiguresGeneric(rrs, startFigure, endFigure)

    /*
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

    def topNonZeroFiguresGeneric(rrs: Vector[RunResult], startFigure: Figure, endFigure: Figure): Array[Figure] =
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
}

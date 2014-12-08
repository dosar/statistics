package com.example.loto

import com.example.loto.model.RunResult

import scala.collection.mutable

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

    def figuresOccurencies(rrs: Seq[RunResult], startFigure: Figure = 1): mutable.Map[Figure, HitCount] =
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
        val result = mutable.Map[Int, Int]()
        while(ind < figuresMap.length)
        {
            result += (ind + 1) -> figuresMap(ind)
            ind += 1
        }
        result
    }

    def topNonZeroFigures(rrs: Seq[RunResult]): Array[Figure] = topNonZeroFiguresGeneric(rrs, 1, 36)

    def topNonZeroFiguresWithoutNotPopular(rrs: Seq[RunResult]): Array[Figure] = topNonZeroFiguresGeneric(rrs, 16, 36)

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


    def topNonZeroFiguresGeneric(rrs: Seq[RunResult], startFigure: Figure, endFigure: Figure): Array[Figure] =
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

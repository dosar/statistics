package com.example.loto.betgenerator

import com.example.loto.metrics.MoneyHitStatisticsType

/**
 * Created by alespuh on 04.01.15.
 * figuresByHit - в качестве индекса - хиты, в качестве значений - массив чисел, первый элемент - длина,
 * остальные - числа, которые встретились хит раз.
 */
class FromMiddleOccurenciesBetGenerator(figuresByHit: Array[Array[Int]], take: Int, startFigure: Int, endFigure: Int)
    extends MoneyHitStatisticsType
{
    def generate(): Array[Int] =
    {
        while(resInd < take && currentHit >= 0 && currentHit <= maxHit)
        {
            val figures = figuresByHit(currentHit)
            if(figures == null) moveFurther
            else
            {
                val hitIndex = getHitIndex
                if(hitIndex < figures(0) + 1)
                {
                    if(setResultFigure(figures, hitIndex) && currentHit != startHit) changeMovement
                }
                else moveFurther
            }
        }
        result
    }

    def setResultFigure(figures: Array[Int], hitIndex: Int): Boolean =
    {
        val figure = figures(hitIndex)
        val setHappens = figure >= startFigure && figure <= endFigure
        if(setHappens)
        {
            result(resInd) = figure
            resInd += 1
        }
        incrementHitIndex
        setHappens
    }

    def getHitIndex = if(moveUp) upHitIndex else downHitIndex

    def incrementHitIndex = if(moveUp) upHitIndex += 1 else downHitIndex += 1

    def changeMovement =
    {
        if(moveUp) { currentHit = downHit} else { currentHit = upHit}
        moveUp = !moveUp
    }

    def moveFurther =
    {
        if(moveUp) { upHit += 1; currentHit = upHit; upHitIndex = 1 }
        else { downHit -= 1; currentHit = downHit; downHitIndex = 1 }
    }

    private val result = new Array[Int](take)
    private val maxHit: Int =
    {
        var ind = figuresByHit.length - 1
        while(ind >= 0 && figuresByHit(ind) == null)
            ind -= 1
        ind
    }

    private val startHit: Int =
    {
        var startInd = if(maxHit % 2 == 1) maxHit / 2 + 1 else maxHit / 2
        var ind = startInd
        while(ind < figuresByHit.length && figuresByHit(ind) == null)
            ind += 1
        ind
    }

    private var resInd = 0
    private var currentHit = startHit
    private var downHit = startHit - 1
    private var downHitIndex = 1
    private var upHit = startHit
    private var upHitIndex = 1
    private var moveUp = true
}

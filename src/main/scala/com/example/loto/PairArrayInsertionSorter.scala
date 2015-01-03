package com.example.loto

import com.example.loto.CommonImplicits.Incrementer

object PairArrayInsertionSorter
{
    /*
    * длины массивов должны совпадать, сортируем по первому массиву, от большого к малому
    * */
    def sort(master: Array[Int], slave: Array[Int], take: Int) =
    {
        var ind = 1
        val iterations = Math.min(master.length, take)
        while(ind < iterations)
        {
            var revInd = ind
            while(revInd - 1 >= 0 && master(revInd) > master(revInd - 1))
            {
                swap(master, revInd, revInd - 1)
                swap(slave, revInd, revInd - 1)
                revInd = revInd - 1
            }
            ind += 1
        }
        this.take(master, slave, take)(iterations)
    }

    def swap(input: Array[Int], oneIndex: Int, anotherIndex: Int)
    {
        val current = input(oneIndex)
        input(oneIndex) = input(anotherIndex)
        input(anotherIndex) = current
    }

    def take(master: Array[Int], slave: Array[Int], take: Int)(implicit iterations: Int = Math.min(master.length, take)) =
    {
        if(iterations == master.length) (master, slave)
        else
        {
            val (masterResult, slaveResult) = (new Array[Int](iterations), new Array[Int](iterations))
            var ind = 0
            while(ind < iterations)
            {
                masterResult(ind) = master(ind)
                slaveResult(ind) = slave(ind)
                ind += 1
            }
            (masterResult, slaveResult)
        }
    }
}

class PairArrayHeapSorter(master: Array[Int], slave: Array[Int], take: Int)
{
    type MasterArray = Array[Int]; type SlaveArray = Array[Int]

    def sort: (MasterArray, SlaveArray) =
    {
        createHeap
        var ind = master.length
        while(ind > 0)
        {
            swap(0, ind - 1)
            heapify(ind - 1)
            ind -= 1
        }
        PairArrayInsertionSorter.take(master, slave, take)
    }

    private def heapify(heapLength: Int)
    {
        var i = 0
        while(2 * i + 1 < heapLength)
        {
            var indexWithMaxValue = 2 * i + 1
            if(indexWithMaxValue + 1 < heapLength && master(indexWithMaxValue + 1) < master(indexWithMaxValue))
                indexWithMaxValue = indexWithMaxValue + 1
            if(master(i) > master(indexWithMaxValue))
            {
                swap(i, indexWithMaxValue)
                i = indexWithMaxValue
            }
            else return
        }
    }

    private def createHeap
    {
        var ind = 1
        while(ind < master.length)
        {
            var i = ind
            while(master(i) < master((i - 1) / 2))
            {
                swap(i, (i - 1) / 2)
                i = (i - 1) / 2
            }
            ind += 1
        }
    }

    private def swap(oneIndex: Int, anotherIndex: Int)
    {
        PairArrayInsertionSorter.swap(master, oneIndex, anotherIndex)
        PairArrayInsertionSorter.swap(slave, oneIndex, anotherIndex)
    }
}

object FiguresByHitSorter extends MoneyHitStatisticsType
{
    //-1 потому что в figureHits есть неиспользуемый элемент
    def topFigures(figureHits: Array[Int])(implicit take: Int = figureHits.length - 1): Array[Int] =
        topFiguresWithHits(figureHits)._2

    //-1 потому что в figureHits есть неиспользуемый элемент
    def topFiguresWithHits(figureHits: Array[Int])(implicit take: Int = figureHits.length - 1) =
    {
        val incrementer = new Incrementer(1)
        val figures = Array.fill(figureHits.length)(incrementer ++)
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
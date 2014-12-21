package com.example.loto

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

    def take(master: Array[Int], slave: Array[Int], take: Int)(iterations: Int = Math.min(master.length, take)) =
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

class PairArrayHeapSorter(master: Array[Int], slave: Array[Int], take: Int)
{
    def sort =
    {
        createHeap
        var ind = master.length
        while(ind > 0)
        {
            swap(0, ind - 1)
            heapify(ind - 1)
            ind -= 1
        }
        PairArrayInsertionSorter.take(master, slave, take)()
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
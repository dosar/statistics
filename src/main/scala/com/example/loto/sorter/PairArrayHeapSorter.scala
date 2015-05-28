package com.example.loto.sorter

import com.example.loto.array.ArrayPerformanceUtil

class PairArrayHeapSorter(master: Array[Int], slave: Array[Int], take: Int) extends PairArraySorter
{
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
        ArrayPerformanceUtil.take(master, slave, Math.min(master.length, take))
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
        swap(master, oneIndex, anotherIndex)
        swap(slave, oneIndex, anotherIndex)
    }
}

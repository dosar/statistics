package com.example.loto

object PairArraySorter
{
    /*
    * длины массивов должны совпадать, сортируем по первому массиву, от большого к малому
    * */
    def sort(master: Array[Int], slave: Array[Int], take: Int) =
    {
        var ind = 1
        val iterations = Math.min(master.length, take)
        val (masterResult, slaveResult) = (new Array[Int](iterations), new Array[Int](iterations))
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
        ind = 0
        while(ind < iterations)
        {
            masterResult(ind) = master(ind)
            slaveResult(ind) = slave(ind)
            ind += 1
        }
        (masterResult, slaveResult)
    }

    def swap(input: Array[Int], oneIndex: Int, anotherIndex: Int)
    {
        val current = input(oneIndex)
        input(oneIndex) = input(anotherIndex)
        input(anotherIndex) = current
    }
}

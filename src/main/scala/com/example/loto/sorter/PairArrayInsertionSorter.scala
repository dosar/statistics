package com.example.loto.sorter

import com.example.loto.array.ArrayPerformanceUtil

object PairArrayInsertionSorter extends PairArraySorter
{
    /*
    * длины массивов должны совпадать, сортируем по первому массиву, от большого к малому
    * */
    def sort(master: Array[Int], slave: Array[Int])(implicit take: Int = master.length - 1): (MasterArray, SlaveArray) =
    {
        val precisedTake = Math.min(master.length, take)
        new PairArrayInsertionSorter(precisedTake).sort(master, slave)
        ArrayPerformanceUtil.take(master, slave, precisedTake)
    }
}

class PairArrayInsertionSorter(take: Int) extends PairArraySorter
{
    private def sort(master: Array[Int], slave: Array[Int]): Unit =
    {
        if (master.length < 2) return
        val halfLength = master.length / 2

        val leftMasterArr = subArray(master, 0, halfLength)
        val leftSlaveArr = subArray(slave, 0, halfLength)

        val rightMasterArr = subArray(master, halfLength, master.length)
        val rightSlaveArr = subArray(slave, halfLength, master.length)

        sort(leftMasterArr, leftSlaveArr)
        sort(rightMasterArr, rightSlaveArr)

        var resultInd = 0
        var leftInd = 0
        var rightInd = 0
        while ((leftInd < leftMasterArr.length || rightInd < rightMasterArr.length) && resultInd < take)
        {
            val (maxMaster, corrSlave) =
            {
                if(rightInd >= rightMasterArr.length)
                {
                    val ind = leftInd
                    leftInd += 1
                    (leftMasterArr(ind), leftSlaveArr(ind))
                }
                else if(leftInd >= leftMasterArr.length)
                {
                    val ind = rightInd
                    rightInd += 1
                    (rightMasterArr(ind), rightSlaveArr(ind))
                }
                else
                {
                    if(leftMasterArr(leftInd) > rightMasterArr(rightInd))
                    {
                        val ind = leftInd
                        leftInd += 1
                        (leftMasterArr(ind), leftSlaveArr(ind))
                    }
                    else
                    {
                        val ind = rightInd
                        rightInd += 1
                        (rightMasterArr(ind), rightSlaveArr(ind))
                    }
                }
            }
            master(resultInd) = maxMaster
            slave(resultInd) = corrSlave
            resultInd += 1
        }
    }
}



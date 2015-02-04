package com.example.loto.sorter;

import com.example.loto.array.ArrayPerformanceUtil;

/**
 * Created by alespuh on 31.01.15.
 */
public class PairArrayQuickSorter
{
//  сортирует по убыванию master
    public static void quicksort(int[] master, int[] slave)
    {
        quicksort(master, slave, 0, master.length - 1);
        ArrayPerformanceUtil.reverseInPlace(master);
        ArrayPerformanceUtil.reverseInPlace(slave);
    }

//  сортирует по возрастанию master
    public static void quicksort(int[] master, int[] slave, int l, int r)
    {
        int i = l-1, j = r, p = l-1, q = r; int v = master[r];
        if (r <= l) return;
        for (;;)
        {
            while (master[++i] < v) ;
            while (v < master[--j]) if (j == l) break;
            if (i >= j) break;
            exch(master, slave, i, j);
            if (master[i] == v) { p++; exch(master, slave, p, i); }
            if (v == master[j]) { q--; exch(master, slave, j, q); }
        }
        exch(master, slave, i, r); j = i-1; i = i+1;
        for (int k = l; k < p; k++, j--) exch(master, slave, k, j);
        for (int k = r-1; k > q; k--, i++) exch(master, slave, i, k);
        quicksort(master, slave, l, j);
        quicksort(master, slave, i, r);
    }

    private static void exch(int[] master, int[] slave, int i, int j)
    {
        exch(master, i, j);
        exch(slave, i, j);
    }

    private static void exch(int[] input, int oneIndex, int anotherIndex)
    {
        int current = input[oneIndex];
        input[oneIndex] = input[anotherIndex];
        input[anotherIndex] = current;
    }
}

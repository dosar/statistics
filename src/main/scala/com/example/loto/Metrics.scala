package com.example.loto

import java.util
import java.util.Date

import com.orientechnologies.orient.core.record.impl.ODocument

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

/**
 * Created by alespuh on 25.11.14.
 */
object Metrics
{
    lazy val topFigures =
    {
        val results = (1 to 36).map(_ -> 0).toMap
        def update(map: Map[Int, Int], figure: Int) =
            map.updated(figure, results(figure) + 1)

        runResults.foldLeft(results)((map, rr) => rr.result.foldLeft(map)(update))
            .toList.sortBy(_._2).map(_._1)
    }

    lazy val runResults = RunResultService.list

    def graficData(figures: Seq[Int]) = runResults.sortBy(_.run).map(rr => rr.result.filter(figures.contains(_)))
}

case class RunResult(date: Date, run: Int, result : Array[Int])

object RunResultService extends OrientDb
{
    def from(doc: ODocument): RunResult =
    {
        val integers = doc.field[util.ArrayList[Integer]]("result")
        RunResult(doc.field[Date]("date"), doc.field[Int]("run"), integers.map(_.intValue()).toArray)
    }

    def list = tx
    { db =>
        val table = db.browseClass("loto_5_36")
        val result = ListBuffer[RunResult]()
        while(table.hasNext) result += from(table.next())
        result
    }
}
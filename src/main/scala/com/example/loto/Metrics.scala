package com.example.loto

import java.util.Date

import com.google.common.collect.{TreeMultiset}
import com.orientechnologies.orient.core.record.impl.ODocument
import scala.collection.JavaConversions._

/**
 * Created by alespuh on 25.11.14.
 */
object Metrics extends OrientDb
{
    lazy val topFigures = runResults
        .foldLeft(TreeMultiset.create[Int](com.google.common.collect.Ordering.natural()))(
            (acc, elem)=> {elem.result.foreach(acc.add(_)); acc})
        .iterator().toSeq

    lazy val runResults = tx
    { db =>
        val results = for(doc : ODocument <- db.browseClass("loto_5_36").iterator())
            yield RunResult(doc.field[Date]("date"), doc.field[Int]("run"), doc.field[Array[Int]]("result"))
        results.toSeq
    }

    def graficData(figures: Seq[Int]) = runResults.sortBy(_.run).map(rr => rr.result.filter(figures.contains(_)))
}

case class RunResult(date: Date, run: Int, result : Array[Int])

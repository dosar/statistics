package com.example.loto

import java.text.SimpleDateFormat

import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx
import com.orientechnologies.orient.core.record.impl.ODocument

import scala.io.Source

/**
 * Created by alespuh on 25.11.14.
 */
object LotoImporter extends OrientDb
{
    def fillDb = tx
    { db =>
        val source = Source.fromFile("/home/alex/work/statistics/src/main/resources/5_36.txt")
        val lines = source.getLines().toList
        val lines1 = lines.zipWithIndex.filter(_._2 % 3 == 0).map(_._1).toList
        val lines2 = lines.zipWithIndex.filter(_._2 % 3 == 1).map(_._1).toList
        val format = new SimpleDateFormat("dd.MM.yyyy hh:mm")
        for((line1, line2) <- lines1.zip(lines2))
        {
            val doc = new ODocument("loto_5_36")
            val run :: date :: Nil = line1.split(" / ").toList
            doc.field("run", run.toInt)
            doc.field("date", format.parse(date))
            doc.field("result", line2.split(" ").filter(_.trim != "").map(_.toInt))
            doc.save()
        }
    }
}
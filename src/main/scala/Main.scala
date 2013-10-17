package com.qubell.cli

import com.sun.xml.internal.messaging.saaj.util.Base64
import java.net.{URL, HttpURLConnection}
import scala.util.parsing.json.JSON

object Main {

  def main (args: Array[String]) = {
    args.toList match {
      case "-u" :: user :: pass :: rest => {
        val api = new API(user, pass)
        rest match {
          case "-org" :: orgId :: rest => {
            rest match {
              case "applications-list" :: _ => print (formatList(api.listApps(orgId), appHeaders))

            }
          }
        }
      }
    }
  }

  private val appHeaders  = List("id", "name")
  private def formatList (list: List[Any], headers: List[String]) = {
    val data = list.map({case x: Map[String, String] => headers.map(h => x.get(h).getOrElse("") )} )
    Tabulator.format(headers :: data) + "\n"
  }
}

class API(username: String, password: String) {
  def listApps(orgId: String) = {
    val response = getResponse(s"https://express.qubell.com/api/1/organizations/$orgId/applications")
    JSON.parseFull(response).get.asInstanceOf[List[Any]]
  }

  private def getClient(url: String) = {
    val authStr = username + ":" + password
    val base64 = Base64.encode(authStr.getBytes("UTF-8"))
    val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.setDoOutput(true)
    connection.setRequestProperty("Authorization", "Basic " + new String(base64, "UTF-8"))

    connection
  }

  private def getResponse(url: String): String = {
    scala.io.Source.fromInputStream(getClient(url).getInputStream).getLines().mkString("\n")
  }
}

//http://codereview.stackexchange.com/questions/5138/formatting-as-a-table-in-scala
object Tabulator {

  def format(table: Seq[Seq[Any]]) = table match {
    case Seq() => ""
    case _ =>
      val cellSizes = for (row <- table) yield
        (for (cell <- row) yield
          if (cell == null) 0 else cell.toString.length)
      val colSizes = for (col <- cellSizes.transpose) yield col.max
      val rows = for (row <- table) yield formatRow(row, colSizes)
      formatRows(rowSeparator(colSizes), rows)
  }

  def formatRow(row: Seq[Any], colSizes: Seq[Int]) = {
    val cells = (for ((item, size) <- row.zip(colSizes)) yield
      if (size == 0) "" else ("%" + size + "s").format(item))
    cells.mkString("|", "|", "|")
  }

  def formatRows(rowSeparator: String, rows: Seq[String]): String = (
    rowSeparator ::
      rows.head ::
      rowSeparator ::
      rows.tail.toList :::
      rowSeparator ::
      List()).mkString("\n")


  private def rowSeparator(colSizes: Seq[Int]) =
    colSizes map { "-" * _ } mkString("+", "+", "+")
}

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
          case "org" :: orgId :: rest => {
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
    headers.mkString("\t") + "\n" +
      headers.map(x => "-" * x.size).mkString("\t") + "\n" +
    list.map({case map: Map[String, Any] => {
       headers.map(x => map.get(x).get).mkString("\t")
      }
    }).mkString("\n") + "\n"
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

case class Application (id: String, name: String)

package com.mwt.forms 

import java.net.URI

import com.twitter.util.Await
import com.twitter.finagle.http.Request
import com.twitter.finatra.http.Controller
import com.twitter.finatra.response.Mustache
import com.twitter.finatra.request.FormParam

// Downloader
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.{Request, Response, Method}
import com.twitter.finagle.Http
import com.twitter.finagle.service.RetryBudget
import com.twitter.util.{Duration, Future}

// Parser
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, DocumentType, Element}

// Collection Conversions
import collection.JavaConverters._

case class Result(
  title: Option[String], 
  version: Option[String],
  links: Map[String, Int],
  hasLogin: Boolean,
  headlines: Map[String, Int])

@Mustache("foo")
case class RenderableResult(
  title: String, 
  version: String,
  links: String,
  hasLogin: String,
  headlines: String)

case class UrlRequest(
  @FormParam url: String) 

class AnalyzeController extends Controller {
  get("/") { _: Request => response.ok.view("form.mustache", None)}

  post("/analyze") { request: UrlRequest =>
    val host = new URI(request.url).getHost().toString
    println(s"Fetching data for ${request.url}")
    Downloader.download(request.url)
      .map { r => println(r); r} 
      .map { response => Parser.load(host, response.contentString)}
      .map { r => Parser.render(r) }
      .map { r => response.ok.view("results.mustache", r)}
      .handle { case e => println(e.toString)}
  }
}

/**
 * Custom parser for HTML sites
 */
object Parser {
  def render(result: Result) = RenderableResult(
      if(result.title.isDefined) result.title.get else "no-title",
      if(result.version.isDefined) result.version.get else "no-version",
      if(result.hasLogin) "has login" else "no login",
      result.links.toString,
      result.headlines.toString)

  /**
   * Extracts doctype optionally
   */
  def version(doc: Document) = doc.childNodes()
      .asScala
      .filter(_.isInstanceOf[DocumentType])
      .headOption
      .map { s => s.toString }

  /**
   * Extracts title of the page
   */
  def title(doc: Document) = doc.select("title")
    .asScala
    .headOption
    .map(_.text)

  /**
   * Returns links of the page
   */
  def links(doc: Document) = doc.select("a").asScala

  /**
   * Grouped links, internal and external
   */
  def groupedLinks(host: String, links: Seq[Element]) = {
    def href(link: Element) = link.attr("href")

    Map("internal" -> links.filter { l => isToHost(host, l) }.size,
      "external" -> links.filter { l => !isToHost(host, l) }.size)
  }

  /**
   * @todo multiple predicates to check
   */
  def hasLogin(doc: Document) = {
    doc.select("#login").asScala.size > 0 
  }

  /**
   * All the headlines, grouped in level
   */
  def headlines(doc: Document) = {  
    def select(selector: String) = doc.select(selector).asScala.size
    Range(1,5).map { i => (s"h$i" -> select(s"h$i")) }.toMap 
  }

  def isToHost(host: String, link: Element) = 
    link.attr("href").indexOf(host) > 0 

  def parse(html: String): Document = Jsoup.parse(html)

  def load(host: String, html: String): Result = {
    val doc = parse(html)
    Result(title(doc), 
      version(doc), 
      groupedLinks(host, links(doc)), 
      hasLogin(doc),
      headlines(doc))
  }
}

/**
 * Takes care of downloading
 */
object Downloader {

  def port(uri: URI) = if(uri.getPort() < 0) 80 else uri.getPort()

  /**
   * Download site
   */
  def download(url: String): Future[Response] = {
    val budget: RetryBudget = RetryBudget.Empty
    val uri = new URI(url)
    val client = Http.client.newService(s"${uri.getHost()}:${port(uri)}")

    val path = s"${uri.getPath()}"
    val fullPath = if(Option(uri.getQuery()).isDefined) 
      s"$path?${uri.getQuery()}"
    else path

    val req = Request(Method.Get, fullPath)
    val hm = req.headerMap
    hm.put("User-Agent", "curl/7.43.0")
    hm.put("Accept", "*/*")
    hm.put("Host", s"${uri.getHost()}")
    client(req)
  }
}

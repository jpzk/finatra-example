package com.mwt.forms

import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers { 
  import Parser._

  behavior of "Parser"
  
  val testHtml = fromFile("./src/test/resources/test.html").mkString

  def fromFile(fileName: String) = scala.io.Source.fromFile(fileName)
  def doc = Parser.parse(testHtml)

  val ExpVersion = Some("<!doctype html>")
  val ExpTitle = Some("Golem.de: IT-News für Profis")
  val ExpIntLinks = 456
  val ExpExtLinks = 4
  val ExpHeadlines = Map (
        "h1" -> 2,
        "h2" -> 26,
        "h3" -> 25,
        "h4" -> 0)

  it should "read the version if it exists" in { 
    val version = Parser.version(doc)
    version.isDefined shouldEqual true
    version shouldEqual ExpVersion
  }

  it should "read title if it exists" in {
    val title = Parser.title(doc)
    title.isDefined shouldEqual true
    title shouldEqual ExpTitle
  }

  it should "read links in" in {
    val links = Parser.links(doc)
  }

  it should "group internal, external links" in {
    val groups = Parser.groupedLinks("golem.de", Parser.links(doc))
    groups("internal") shouldEqual ExpIntLinks
    groups("external") shouldEqual ExpExtLinks
  }

  it should "group headlines count" in {
    val groups = Parser.headlines(doc)
    groups shouldEqual ExpHeadlines
  }

  it should "load" in {
    val result = Parser.load("golem.de", testHtml)
    result.title.isDefined shouldEqual true
    result.version.isDefined shouldEqual true
  }
}


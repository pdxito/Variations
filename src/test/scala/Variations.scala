package com.marcus

import java.awt.event.MouseEvent

import org.htmlcleaner.{HtmlCleaner, TagNode}
import processing.core.{PApplet, PConstants, PFont}
import http.requests._
import scala.io.Source
import java.io._
import java.net.URL

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.text.SimpleDateFormat
import java.util.{Calendar, Date}
import java.time.LocalDateTime


case class info(min: Double, max: Double, avg: Double, total: Double)
case class Node(name: String, parent:Option[Node], var children: Option[ListBuffer[Node]] = None)

class Variations extends PApplet {
  var timer: Float = 0
  var drawBackground = true
  val backgroundColor = 128
  var symWithVolatility: Map[String, Array[Double]] = new HashMap[String, Array[Double]]
  var symWithSector: Map[String, String] = null
  var sectorsWithStocks: Map[String, Array[(String, String)]] = null
  var stockDataMap = new HashMap[String, Array[String]]
  var sectors: Array[String] = null
  var currentSector: String = ""
  var sectorIndex = 0
  val offsetY = 50
  val offsetX = 150
  var sectorMap = new HashMap[Range, String]()
  var blockWidth = 1f
  var dateIndexes: Array[String] = null
  var blackAndWhite = false
  var useRatioForHeight = true
  val rightMargin = 25
  var daysBack = 15
  var doLog = false
  var sb: StringBuilder = new StringBuilder()
  var parents = Set[String]()
  val outputsDir = "/Users/marcus.vincent/Development/stocks/outputs"
  val baseDir = "/Users/marcus.vincent/Development/stocks"
  val sourceStocksFile = "/Users/marcus.vincent/Development/stocks/Workbook3.txt"
  val downloadDataDir = "/Users/marcus.vincent/Development/stocks/data/"
  val maxSimilarityDelta = .05f
  val similarityDays = 10
  val numDaysToComputeForOrdering = 5
  val numDaysToRetrieve = 31
  val PRICE_CLOSE = 4
  val RATIO_HIGH = 6
  val RATIO_CLOSE = 9
  val colorWhite = color(255,255,255)
  val colors = Map("Telecommunications Services" -> (color(190, 235, 159), colorWhite)
    , "Financials" -> (color(255, 97, 56), colorWhite)
    , "Materials" -> (color(121, 189, 143), colorWhite)
    , "Energy" -> (color(0, 163, 136), colorWhite)
    , "Industrials" -> (color(189,0,255), colorWhite),
    "Consumer Staples" -> (color(4, 99, 128), colorWhite),
    "Utilities" -> (color(255,154,0), colorWhite),
    "Real Estate" -> (color(240,55,45), colorWhite),
    "Information Technology" -> (color(215, 66, 244), colorWhite),
    "Health Care" -> (color(159, 180, 204), colorWhite),
    "Consumer Discretionary" -> (color(219, 65, 5), colorWhite))


  case class Similarity(sym1: String, sym2: String, similarity: Double)

  override def setup() = {
    val m = Source.fromFile(sourceStocksFile, "UTF-8").getLines().drop(1).map { l =>
      val a = l.split("\t")
      (a(0), a(2))
    }.toArray


    sectorsWithStocks = m.groupBy(_._2)

    //sectors.foreach(println(_))
    sectors = sectorsWithStocks.keySet.toArray
    symWithSector = m.toMap
    //PFont.list().foreach(println(_))
    currentSector = sectors(sectorIndex)

    loadData

  }



  def genJsonFromMaven(fullPath: String): Node = {

    var tree = Source.fromFile(fullPath, "UTF-8").getLines().toArray
    var lastIndex = 0

    val baseLine = tree(7).replace("[INFO] ", " ")
    var parentKey = baseLine
    var currentParent = Node(baseLine, None)
    var root = currentParent
    var parents = Set[String]()

    tree.drop(8).foreach { l: String =>
      if (!l.startsWith("[INFO] -----------------")) {
        val line = l.replace("[INFO] ", " ")
        val dep = if (line.contains("version selected")) {
          val l = line.substring(0, line.lastIndexOf("compile") + 7)
          l.substring(l.lastIndexOf(" ") + 1)
        }
        else {
          line.substring(line.lastIndexOf(" ") + 1)
        }

        val tokenCount = (line.lastIndexOf(' ') + 1) / 3

        (tokenCount) match {
          case c if c == lastIndex => // Same index as last
            val newNode = Node(dep, Some(currentParent.parent.get))
            currentParent.parent.get.children.get += newNode
            currentParent = newNode
          case d if d > lastIndex => // Drilled down a level, can never be more than one at a time
            // Add this to the current parent
            val newNode = Node(dep, Some(currentParent))

            currentParent.children match {
              case Some(c) =>
              case _ =>
                currentParent.children = Some(new ListBuffer[Node]())
            }

            currentParent.children.get += newNode
            currentParent = newNode
          case e if e < lastIndex => // Up some number of levels
            // How many levels up to find the correct parent?
            val delta = lastIndex - tokenCount
            for (x <- 0 to delta) {
              try {
                currentParent = currentParent.parent.get
              }
              catch {
                case ex: Exception =>
                  println(ex)
              }
            }
            val newNode = Node(dep, Some(currentParent))
            currentParent.children match {
              case Some(c) =>
              case _ =>
                currentParent.children = Some(new ListBuffer[Node]())
            }
            currentParent.children.get += newNode
            currentParent = newNode
        }

        lastIndex = tokenCount
        // println(tokenCount)
      }
    }
    root
  }

  def renderJson(n: Node): Unit = {
    n.children match {
      case Some(nodes) if nodes.length > 0 =>
        //println(s"'${n.name}':[${n.children.foreach(renderJson)}],")
        sb.append(s"""{"name":"${n.name.trim}","children":[{""")
        sb.append(n.children.get.foreach(renderJson))
        sb.append("]},")
      case _ =>
        //println(s"'${n.name}':''")
        sb.append( s"""{"name":"${n.name.trim}","size":0},""")
    }
  }

  def renderFlat(n: Node, root: String): Unit = {
    n.children match {
      case Some(nodes) if nodes.length > 0 =>

        n.children.get.foreach{child =>
          if (!parents.contains(root)) {
            sb.append(root.replace(".", "|").replace(":compile", "")
              //  + .replace("|com", "|c").replace("org|", "o|")
              + ",\n")
            parents += root
          }
          renderFlat(child, root +"~" + child.name)}
      case _ =>
        // Don't append duplicate parents
        val cleaned = root.replace(".", "|")
        //  .replace(":compile", "").replace("|com", "|c").replace("org|", "o|")

        sb.append(s"${cleaned},100\n")

    }
  }


  override def mouseMoved() = {
    fill(255)
    rect(0, 0, 400, 20)
    fill(0)

    val index = Math.floor((mouseX - offsetX) / blockWidth).toInt
    var sym = ""
    // Write stock ticker symbol
    if (mouseX >= offsetX) {
      sectorMap.collectFirst {
        case i if i._1.contains(mouseY) =>
          sym = s"${i._2.replace(".txt","")}"
          text(sym, 25, 5)
      }
    }

    // Write date
    if (mouseY >= offsetY && dateIndexes != null && index >= 0 && (index < dateIndexes.length)) {
      val date = s"${dateIndexes(index)}"
      text(date, 125, 5)

      if (sym.length > 0) {
        val dataRow = stockDataMap(sym).find(_.contains(date)).get.split("\t")
        text(s"${dataRow(RATIO_CLOSE)} / ${dataRow(PRICE_CLOSE)}", 250, 5)
      }
    }


  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(f=> f.isFile && !f.getName.contains(".DS_Store")).toList
    } else {
      List[File]()
    }
  }

  def downloadStockData(url: String): Array[String] = {
    var stories = new ListBuffer[String]
    val cleaner = new HtmlCleaner

    val props = cleaner.getProperties
    val rootNode = cleaner.clean(new URL(url))
    val ab = rootNode.getElementListByName("tr",true).toArray

    val arrayOfInfo = ab.map(n => n.asInstanceOf[TagNode].getText.toString.replace("\n","|"))
      .map(_.split('|'))
      .map(a=>a.map(_.trim))
      .map(a=>a.filter(_.length > 0))
      .map(_.mkString("\t"))

    arrayOfInfo
  }

  def calculateSimilarity(sym1: String, sym2: String, days: Int): Double = {
    // Load the two files and look at their performance ratios side by side.  When they match within a 10% range
    // consider that a match.  Sum up the number of matches.  The more matches the more similar
    val data1 = loadStrings(s"$outputsDir/$sym1")
    val data2 = loadStrings(s"$outputsDir/$sym2")
    var similaritySum = 0.0
    try {
      for (i <- 0 until days) {
        val ratio1 = data1(i).split("\t")(RATIO_CLOSE).toDouble
        val ratio2 = data2(i).split("\t")(RATIO_CLOSE).toDouble

        if (Math.abs(ratio1 - ratio2) <= maxSimilarityDelta) {
          similaritySum += Math.abs(ratio1 - ratio2)
        }
      }
      similaritySum

      //      if (similaritySum > 5) {
      //        //println(s"$sym1\t$sym2\t$similaritySum")
      //      }
    } catch {
      case ex: Exception =>
        println(s"Error with $sym1 or $sym2")
        0
    }
  }

  override def keyPressed = {
    key match {
      case 'e' =>
        exportCurrentSectorDetails
      case 'p' =>
        currentSector = sectors(sectorIndex)
        sectorIndex += 1
        if (sectorIndex >= sectors.length) {
          sectorIndex = 0
        }
        drawAllRatios()
      case 'j' =>
        val files = getListOfFiles("/Users/marcus.vincent/Development/d3/maven")

        files.foreach{f =>
          val j =  renderJson(genJsonFromMaven(f.getAbsolutePath))
          val json = sb.toString.replace("{{","{").replace(",()","").dropRight(1).replace(":compile","")
          //.replace("com.","c.").replace("org.","o.")

          val path = s"/Users/marcus.vincent/Development/d3/flare/${f.getName.replace(".txt","")}.json"
          Files.write(Paths.get(path), json.getBytes(StandardCharsets.UTF_8))
          sb.clear()
        }

        renderFlat(genJsonFromMaven("/Users/marcus.vincent/Development/d3/maven/mvn_ql_lib.txt"), "com|webtrends:data-explore-api")
        val tt = sb.toString.replace("~",".")
        //.replace("webtrends","wt").replace("wookiee","wk").replace("data-explore-api","dapi")
        //.replace("specs2:specs2-junit_2","su2")
        var k = 3
      case 'l' => doLog = !doLog
      case 'f' =>
        // Find similiar patterns
        val d = new File(outputsDir)
        val symbols = d.listFiles.filter(f => f.isFile && !f.isHidden && !f.getName.contains("curl_urls")) map { f =>
          f.getName
        }

        val comparisons = symbols.combinations(2).toArray
        val similarities = comparisons.map { c =>
          (Similarity(c(0), c(1), calculateSimilarity(c(0), c(1), similarityDays)))
        }.sortBy(s => s.similarity).reverse

        val simData = similarities.map ( s=> s"${s.sym1}\t${s.sym2}\t${"%1.2f".format(s.similarity)}\t${symWithSector(s.sym1)}\t${symWithSector(s.sym2)}")
        //val simData = similarities.map ( s=> s"${s.sym1}\t${s.sym2}\t${s.similarity}")
        saveStrings(s"$baseDir/similarities.txt", simData)
        println("Done with comparisons.")
      case 'b' =>
        blackAndWhite = !blackAndWhite

      case 'u' =>
        // Update data
        val customFormat = new SimpleDateFormat("MMM+dd,+YYYY")
        val today = new Date()
        val startingDate = new Date(today.getTime - (1000 * 60 * 60 * 24 * numDaysToRetrieve))
        //val startDate = customFormat.format(startingDate)
        val endDate = customFormat.format(today)

        import java.util.Calendar
        var dt = new Date()
        val c = Calendar.getInstance
        c.setTime(dt)
        c.add(Calendar.DATE, -numDaysToRetrieve)
        dt = c.getTime
        var startDate = customFormat.format(dt)

        val updateStocks = Set("WYND")
        //intersect(updateStocks)
        //sectorsWithStocks.flatMap(_._2).keySet.intersect(updateStocks).toArray.par.map { sym =>
        sectorsWithStocks.flatMap(_._2).keySet.toArray.par.map { sym =>
          //println(s"Starting: $sym")
          val url = s"https://www.investopedia.com/markets/api/partial/historical/?Symbol=$sym&Type=%20Historical+Prices&Timeframe=Daily&StartDate=$startDate&EndDate=$endDate"
          println(url)
          val data = downloadStockData(url)
          println(s"Completed: $sym")
          val toSaveData = mergeData(data, sym)
          saveStrings(s"$baseDir/data/$sym", toSaveData)
        }

        println("All data updated")
        val k = 3
      case 'r' =>
        // Calculate All Ratios for all stocks
        generateAllRatios

      case 'c' =>
        var yOffset = 0
        val yBuffer = 30

        // Open the file for stocks to compare
        val symbols = sectorsWithStocks.get("Information Technology").map(k => k.map(_._1).toList).get
        //val symbols = List("AAPL", "HPE", "GOOG")

        var localDaysBack = 10
        // Draw the background
        background(128)
        // How many stocks comparing?
        val stockCount = symbols.size
        val opacity = 1.0f /// stockCount.toFloat
      val blockWidth = ((this.width - offsetX) / localDaysBack.toFloat).toInt
        val blockHeight = (this.height - yBuffer) / stockCount.toFloat
        val daysOffset = 0

        // Draw each s
        symbols.foreach { sym =>
          val y = yBuffer + yOffset * blockHeight

          // Load ratio data file for the stock. There are no headers in this file
          val data = loadStrings(s"$outputsDir/$sym")

          val c = color(255, 255, 255)
          // (symbolData: Array[String], xLoc: Int, yLoc: Int, blockWidth: Int, blockHeight: Int, yOffset: Int, daysBack: Int, opacity: Float, blockColor: Int) = {

          //rect(offsetX + x * blockWidth, yOffset, blockWidth, height)
          drawRatioChart(daysOffset, data, offsetX, y.toInt, blockWidth, blockHeight.toInt, localDaysBack, opacity, c)
          yOffset += 1
        }
      case ',' =>
        daysBack -=1
        generateAllRatios
        drawAllRatios()
      case '.' =>
        daysBack +=1
        generateAllRatios
        drawAllRatios()
      case 'd' =>
        drawAllRatios()
      case 's' =>

        val stocksOfInterest = List("MS", "C", "SPGI", "PKI","LOW","KMX","JEC","TSCO","ORCL","EXPE","PYPL","AVGO")
        drawAllRatios(Some(stocksOfInterest))
      case _ =>
    }
  }

  def exportCurrentSectorDetails: Unit = {
    // Symbol, date1, date2, date3, date4
    // ORCL,.5,.25,1.0,.75

    var dataRows = List[String]()

    val header = "," + stockDataMap(sectorsWithStocks(currentSector).head._1).map { dataRow =>
      dataRow.split("\t")(0)
    }.mkString(",")

    dataRows :+= header

    sectorsWithStocks(currentSector).foreach { sym =>
      val data = stockDataMap(sym._1)
      val rowData = sym._1 + "," + data.map { dataRow =>
        dataRow.split("\t")(RATIO_CLOSE)
      }.mkString(",")

      dataRows :+= rowData
    }

    val pw = new PrintWriter(new File(s"$baseDir/flattened.csv"))
    pw.write(dataRows.mkString("\n"))
    pw.close
  }

  def generateAllRatios = {
    // Generate Ratios
    val d = new File(s"$baseDir/data")

    sectorsWithStocks.flatMap(_._2).keySet.toArray.par.foreach { sym =>
      generateRatiosForStock(sym)
    }

    val x = symWithVolatility.toArray.sortBy { s => s._2(3) }.reverse

    val y = x

  }

  def drawAllRatios(stocksOfInterest: Option[List[String]] = None) = {
    loadData()
    val myFont = createFont("Georgia", 20)
    textFont(myFont)
    textAlign(PConstants.LEFT, PConstants.CENTER)

    background(backgroundColor)

    var blockHeight = 2
    val outputsDirectory = new File(outputsDir)
    var stockCount = 0

    //val sectorsOfInterest = sectors.keys.toList // List("Information Technology","Materials")
    val sectorsOfInterest = List(currentSector)

    //println(sectors.keySet.mkString("\n"))


    if (stocksOfInterest.isDefined) {
      drawStocksOfInterest(stocksOfInterest.get)
    } else {
      // Get the total number of stocks filtered down
      sectorsWithStocks.keys.filter(sectorsOfInterest.contains(_)).foreach { k =>
        val companies = sectorsWithStocks(k).map(_._1)

        val stocksInSector = sectorsWithStocks.flatMap(_._2).keySet.toArray.filter { sym =>
          companies.contains(sym)
        }.size

        stockCount += stocksInSector
      }

      //calculate height based on number of stocks.
      blockHeight = ((this.height - offsetY) / stockCount.toFloat).toInt

      drawSectors(outputsDirectory, sectorsOfInterest, blockHeight)
    }


  }

  def drawStocksOfInterest(stocksOfInterest: List[String]) = {
    var yOffset = 0

    dateIndexes = stockDataMap(stocksOfInterest(0)).map(_.split("\t")(0))
    sectorMap = sectorMap.empty
    blockWidth = (this.width.toFloat - offsetX - rightMargin) / daysBack.toFloat

    val blockHeight = ((this.height - offsetY) / stocksOfInterest.length.toFloat).toInt

    stocksOfInterest.foreach { s =>
      val sectorName = symWithSector(s)
      val c1 = if (blackAndWhite) color(255) else colors(sectorName)._1
      val c2 = if (blackAndWhite) color(0) else colors(sectorName)._2
      fill(c1)

      drawChart(s, offsetY + yOffset * blockHeight, blockHeight, c1, c2)
      val y = offsetY + yOffset * blockHeight
      // Keep track of at what Y coordinate a stock is layed out at.
      sectorMap += (Range(y + 2, 2 + y + blockHeight) -> s)
      //println(s"${y + 2},${2+ y+height}, ${f._1}")
      yOffset += 1
    }
    writeDateHeaders
  }

  def drawSectors(outputsDirectory: File, sectorsOfInterest: List[String], height: Int) = {

    var yOffset = 0
    sectorMap = sectorMap.empty

    // Iterate over each sector if there are multiples
    sectorsWithStocks.keys.filter(sectorsOfInterest.contains(_)).foreach { sectorName =>

      // All the companies in one sector
      val companies = sectorsWithStocks(sectorName).map(_._1)

      val c1 = if (blackAndWhite) color(255) else colors(sectorName)._1
      val c2 = if (blackAndWhite) color(0) else colors(sectorName)._2
      fill(c1)

      // Write out the sector name
      text(sectorName, 0, 5 + offsetY + yOffset * height)

      val stocksInSector = sectorsWithStocks.flatMap(_._2).keySet.toArray.filter { sym =>
        companies.contains(sym)
      } map { f =>
        (f, stockDataMap(f))
      }

      // Sort the stocks with grouped together by ratio performance
      val stocksInSectorSorted = stocksInSector.sortBy { d =>
        val infoArray = d._2.map(_.split("\t"))
        infoArray.take(numDaysToComputeForOrdering).foldLeft(0D) { (total, n) =>
          total + n(RATIO_CLOSE).toDouble // The index to accumulate
        }
      }.reverse

      // Keep track of index to date.
      // Take the first stock in the sectory, drop the header line, and pull out the first column (the date)
      dateIndexes = stocksInSector.take(1).map(_._2.drop(0).map(_.split("\t")(0))).flatten

      blockWidth = (this.width.toFloat - offsetX - rightMargin) / daysBack.toFloat

      var dataRows = List[String]()
      val header = "," + stockDataMap(sectorsWithStocks(currentSector).head._1).map { dataRow =>
        dataRow.split("\t")(0)
      }.mkString(",")

      dataRows :+= header

      stocksInSectorSorted foreach { f =>
        val data = stockDataMap(f._1)

        drawChart(f._1, offsetY + yOffset * height, height, c1, c2)

        val y = offsetY + yOffset * height
        // Keep track of at what Y coordinate a stock is layed out at.
        sectorMap += (Range(y + 2, 2 + y + height) -> f._1)
        //println(s"${y + 2},${2+ y+height}, ${f._1}")
        yOffset += 1

        val rowData = f._1 + "," + data.map { dataRow =>
          dataRow.split("\t")(RATIO_CLOSE)
        }.mkString(",")

        dataRows :+= rowData
      }

      writeDateHeaders

      // Right out the file that drives the chart
      val pw = new PrintWriter(new File(s"$baseDir/${sectorName}_flattened.csv"))
      pw.write(dataRows.mkString("\n"))
      pw.close


    }
  }

  def writeDateHeaders = {
    val myFont = createFont("Georgia", 12)
    textFont(myFont)
    textAlign(PConstants.LEFT, PConstants.CENTER)
    fill(0)
    var i = 0
    dateIndexes foreach { d =>
      // Write out the date name
      //rect(offsetX + x * blockWidth, yOffset, blockWidth, height)
      val x = offsetX + (i * blockWidth)
      text(convertDate(d), x, offsetY - 10)
      i += 1
    }

    textFont(createFont("Georgia", 20))
  }

  def convertDate(d: String) = {
    d.take(6).replace("May ", "05/").replace("Jun ", "06/")
  }

  def drawRatioChart(daysOffset: Int, symbolData: Array[String], xLoc: Int, yLoc: Int, blockWidth: Int, blockHeight: Int, daysBack: Int, opacity: Float, blockColor: Int) = {
    // Open the file in outputs directory which has ratio calculated already.
    var x = xLoc
    val y = yLoc
    val w = blockWidth
    val h = blockHeight
    var dayRatio = 1.0f

    // Simply draw daysBack buckets over the width of the screen with opacity applied
    symbolData.drop(daysOffset).take(daysBack).foreach { dayOfData =>
      // 0 Jun 11 2018
      // 1 48.15
      // 2 48.39
      // 3 47.97
      // 4 48.19
      // 5 12438013.00
      // 6 1.00
      // 7 1.00
      // 8 1 .00
      // 9 1.00

      // Split the day into fields
      val stockData = dayOfData.split("\t").drop(6)

      val dayRatio = stockData(0).toFloat
      val op = opacity * dayRatio
      println(s"Opacity is: $op")
      fill(blockColor, op * 255)
      println(s"$x,$y,$w,$h")
      rect(x, y ,w ,h)
      x += blockWidth
    }
  }

  def mergeData(newData: Array[String], symbol: String): Array[String] = {
    // Load the existing file, if not present just return the newData

    val updatedData = loadStrings(s"$baseDir/data/$symbol") match {
      case existingData if existingData != null =>
        // Existing data found. See what the last date recorded was
        val lastRecord = existingData.drop(1).head.split("\t")(0)

        // Find the 'lastRecord' in the new data.
        val dataToBeAdded = newData.drop(1).takeWhile{ dataRow =>
          val row = dataRow.split("\t")
          if (row(0) == lastRecord) false else true
        }
        // Take header, new rows, and existing data (minus header)
        existingData.take(1) ++ dataToBeAdded ++ existingData.drop(1)
      case _ => newData
    }
    updatedData.filter(_.contains("Dividend") == false)
  }


  def downloadData(sym: String): Unit = {
    println(s"$sym started")
    val get = new GetRequest(s"http://download.macrotrends.net/assets/php/stock_data_export.php?t=$sym")
    get.addHeader("User-Agent","(KHTML, like Gecko) Chrome/66.0.0.181 Safari/537.36")
    get.send()
    val content = get.getContent

    val dataArray = content.split("\n").drop(10)//.map(_.split(","))
    saveStrings(s"$downloadDataDir$sym",dataArray)
    println(s"$sym completed")

  }

  def generateRatiosForStock(sym: String) = {
    println(s"Computing Ratio for: $sym")
    val stockData = readStock(sym, daysBack).map { a =>
      (a(0), Array(a(1).toDouble, a(2).toDouble, a(3).toDouble, a(4).toDouble,
        a(5).toDouble))
    }.toArray

    val openInfo = getMinMaxValues(0, stockData, daysBack)
    val highInfo = getMinMaxValues(1, stockData, daysBack)
    val lowInfo = getMinMaxValues(2, stockData, daysBack)
    val closeInfo = getMinMaxValues(3, stockData, daysBack)

    val openSpread = openInfo.max - openInfo.min
    val highSpread = highInfo.max - highInfo.min
    val lowSpread = lowInfo.max - lowInfo.min
    val closeSpread = closeInfo.max - closeInfo.min

    val comparisons = Array(openInfo.max, highInfo.max, lowInfo.max, closeInfo.max)
    val spreads = Array(openSpread, highSpread, lowSpread, closeSpread)

    val stockRatios = ratios(stockData, comparisons, spreads)

    // Get volatility which is the percentage that the spread is based on the average
    // Spread of $10 of average of $100 would mean 10% volatility
    //
    val volatilities = Array(spreads(0) / openInfo.avg,
      spreads(1) / highInfo.avg,
      spreads(2) / lowInfo.avg,
      spreads(3) / closeInfo.avg)

    symWithVolatility += (sym -> volatilities)

    val enhancedStockInfo = stockRatios.map { m =>
      s"${m._1}\t${m._2.map(n=>f"${n}%1.2f").mkString("\t")}"
    }.mkString("\n")

    val pw = new PrintWriter(new File(s"$outputsDir/$sym"))
    pw.write(enhancedStockInfo)
    pw.close
    println(s"Finished Ratio for: $sym")
  }

  def loadData() = {
    stockDataMap.empty
    val d = new File(outputsDir)

    d.listFiles.filter(f => f.isFile && !f.isHidden && !f.getName.contains("curl_urls")) foreach { f =>
      val info = Source.fromFile(s"/$f", "UTF-8").getLines().toArray

      if (info.length > 2) {
        stockDataMap += (f.getName() -> info.map(_.replace(",","")))
      }
    }
  }

  def drawChart(sym: String, yOffset: Int, height: Int, c1: Int, c2: Int) = {
    // Calculate width of each day by width of window / samples

    val stockData = stockDataMap(sym)
    noStroke()
    val infoArray = stockData.map(_.split("\t"))
    //rectMode(PConstants.CORNER)
    println(s"Processing: $sym")
    for (dayIndex <- 0 until daysBack) {

      val ratioValue = infoArray(dayIndex)(RATIO_CLOSE).toFloat

      fill(lerpColor(c1, c2, ratioValue))
      //stroke(lerpColor(c1, c2, v))

      if (useRatioForHeight) {
        rect(offsetX + dayIndex * blockWidth, yOffset + height, blockWidth, -height * ratioValue)
      } else {
        rect(offsetX + dayIndex * blockWidth, yOffset, blockWidth, height)
      }
    }


  }

  override def draw() = {

  }

  def readStock(sym: String, numDays: Int) = {
    Source.fromFile(s"$baseDir/data/$sym", "UTF-8")
      .getLines().drop(1).filter(!_.contains("Dividend")).take(numDays).map(_.replace(",","").split("\t"))
  }

  def getMinMaxValues(col: Int, data: Array[(String, Array[Double])], numDays: Int) = {

    val sum = data.take(numDays).foldLeft(0D) { (total, n) =>
      total + n._2(col)
    }

    val min = data.take(numDays).foldLeft(Double.MaxValue) { (test, n) =>
      math.min(test, n._2(col))
    }

    val max = data.take(numDays).foldLeft(Double.MinValue) { (test, n) =>
      math.max(test, n._2(col))
    }

    val avg = sum / numDays.toFloat
    info(min, max, avg, sum)
  }

  def ratios(data: Array[(String, Array[Double])], comparisons: Array[Double], spreads: Array[Double]) = {
    data.map { d =>
      (d._1, d._2
        :+ 1 - (comparisons(0) - d._2(0)) / spreads(0) // open
        :+ 1 - (comparisons(1) - d._2(1)) / spreads(1) // high
        :+ 1 - (comparisons(2) - d._2(2)) / spreads(2) // low
        :+ 1 - (comparisons(3) - d._2(3)) / spreads(3) // adj. close
      )
    }
  }

  def compareAll(combinations: Array[Array[(String, Array[String])]]) = {
    val x = combinations.map(c => compare(c(0), c(1),7)).sortBy(_._2).reverse.filter(cr => cr._2 > .98f && cr._2 <= 1.02)
    val k = 234

  }
  def compare(data: (String, Array[String]), dataCompare: (String, Array[String]), column: Int) = {
    // Take the ratio

    val avgRatio = data._2.zip(dataCompare._2).map(t => t._1.split(",")(column).toFloat / t._2.split(",")(column).toFloat).sum / data._2.length.toFloat
    (s"${data._1} -> ${dataCompare._1}", avgRatio)


  }
  override def settings(): Unit = {
    size(1000,800)
  }
}

object Variations  {
  def main(args: Array[String]): Unit = {
    val pArgs = Array("Variations")
    val mp = new Variations
    PApplet.runSketch(pArgs, mp)
  }
}

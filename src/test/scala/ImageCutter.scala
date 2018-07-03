import java.io.{BufferedReader, InputStreamReader}

import processing.core.{PApplet, PConstants, PImage}
import java.io.File

class ImageCutter extends PApplet {
  var timer: Float = 0
  var drawBackground = true
  val convert_path = "/usr/local/bin/convert"
  val baseImgPath = "/Users/marcus.vincent/Development/Variations/Logos_group"
  var img = new PImage  // Declare variable "a" of type PImage


  override def draw() = {
    //background(255,255,0)
    //image(img, 0, 0)
  }

  def getListOfFiles(dir: String):List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(f=> f.isFile && !f.getName.contains(".DS_Store")).toList
    } else {
      List[File]()
    }
  }


  def magic(baseImg: String, outImg: String, cmds: List[String]) = {

    val inputImg = s"$baseImgPath/$baseImg"
    val outputImg = s"$baseImgPath/$outImg"

    val params = List(convert_path,inputImg) ::: cmds ::: List(outputImg)

    doMagic(params)
  }

  def doMagic(args: List[String]):String = {
    val pb = new ProcessBuilder(scala.collection.JavaConversions.seqAsJavaList(args))

    pb.redirectErrorStream(true)

    val p = pb.start()
    val br = new BufferedReader( new InputStreamReader(p.getInputStream))
    val x = br.lines().toArray()
    x.mkString("\n")
  }

  def extractBlobs(fld: String) = {
    val files = getListOfFiles(fld)

    files.foreach(f => findBlobs(f.getName.replace(".jpg","")))
  }

  def findBlobs(baseName: String) = {

    // Crank the threshold up to have distince boundary edges
    magic(s"$baseName.jpg", s"temp/${baseName}_bw.png", List("-threshold", "50%"))

    // Blur image alot!
    magic(s"temp/${baseName}_bw.png", s"temp/${baseName}_blur50.png", List("-blur", "0x42"))

    // Contrast
    magic(s"temp/${baseName}_blur50.png", s"temp/${baseName}_highcontrast.png", List("-contrast-stretch", "12%"))
    // Clamped black and white
    magic(s"temp/${baseName}_highcontrast.png", s"temp/${baseName}_clampedbw.jpg", List("-threshold", "50%"))

    // Identify blobs into a string object structure that defines the blobs bounding boxes
    val blobs = magic(s"temp/${baseName}_clampedbw.jpg", s"temp/${baseName}_blob.jpg",
      List("-colorspace", "gray",
        "-negate",
        "-define", "connected-components:verbose=true",
        "-define", "connected-components:area-threshold=250",
        "-connected-components", "16", "-auto-level"))

    val buffer = 100
    val rects = getBlobBoundaries(blobs, buffer)

    // Draw rectangles around each blob
    magic(s"temp/${baseName}.png", s"temp/${baseName}_blob_rect.jpg", List("-fill", "none", "-stroke", "red") ::: rects)

    extractImages(s"${baseName}.jpg", blobs, buffer)

    background(random(0,255).toInt)

  }

  def extractImages(baseName: String, blobs: String, buffer: Int) = {
    var c = 1
    val info = blobs.split(("\n")).drop(2)
    info.foreach { info2 =>
      val a = info2.trim.split(" ")
      val box = a(1)
      val bInfo = box.split("\\+")
      val x = bInfo(0).split(("x"))(0).toInt
      val y = bInfo(0).split(("x"))(1).toInt

      val boundary = s"${x + buffer}x${y + buffer}+${bInfo(1).toInt - buffer / 2}+${bInfo(2).toInt - buffer / 2}"
      val result = magic(s"${baseName}",s"output/${baseName}_$c.jpg",List("-crop",boundary,"+repage"))
      c = c+1
    }
  }

  def getBlobBoundaries(blobs: String, buffer: Int) = {
    val info = blobs.split(("\n")).drop(2)
    var l = List[String]()

    info foreach { info2 =>
      val a = info2.trim.split(" ")
      val box = a(1)
      val bInfo = box.split("\\+")
      val x = bInfo(0).split(("x"))(0).toInt
      val y = bInfo(0).split(("x"))(1).toInt
      val xStart = bInfo(1).toInt - buffer
      val yStart = bInfo(2).toInt - buffer
      val x2 = x + bInfo(1).toInt + buffer
      val y2 = y + bInfo(2).toInt + buffer
      l :::= List("-draw", s"rectangle $xStart,$yStart $x2,$y2")
    }
    l
  }

  override def keyPressed: Unit = {
    key match {
      case 'b' =>
        extractBlobs("/Users/marcus.vincent/Development/Variations/Logos_group")
      case 'j' =>
        img.loadPixels()
        val edgeImg = createImage(100, 100, PConstants.RGB)

        for (x <- 1 to edgeImg.width)
          for (y <- 1 to edgeImg.height) {
            edgeImg.set(x,y,img.get(x,y))
          }

        edgeImg.updatePixels()

        edgeImg.save("/Users/marcus.vincent/Development/Variations/redthing.png")
      case _ =>
    }
  }

  override def setup() = {
    img = loadImage("/Users/marcus.vincent/Development/Variations/line.png") // Load the image into the program
    frame.setResizable(true)
  }

  override def settings(): Unit = {
    size(270,270)
  }
}

object ImageCutter  {
  def main(args: Array[String]): Unit = {
    val pArgs = Array("ImageCutter")
    val mp = new ImageCutter
    PApplet.runSketch(pArgs, mp)
  }
}


// Convert an image to simply black and white
// convert logos.JPG -threshold 50% logos_bw.jpg

// Blur image alot!
// convert logos_bw.jpg -blur 0x50 blur50.jpg

// Make the contrast high to get big solid blobs
// convert blur50.jpg -contrast-stretch 10%  b1.jpg

// Make it black and white again to clamp it using threshold
// convert b1.jpg -threshold 50% b2.jpg

// Find connected objects using connected-components, since each is now a blob a single entity is found
// convert b2.jpeg -define connected-components:verbose=true -auto-level b3.jpg

// Identify blobs into a string object structure that defines the blobs bounding boxes
// convert b2.jpg -colorspace gray -negate -define connected-components:verbose=true -define connected-components:area-threshold=250 -connected-components 16 -auto-level b3.jpg

/*
Objects (id: bounding-box centroid area mean-color):
  0: 2595x3224+0+0 1308.3,1596.9 6958043 gray(0)
  552: 468x471+1044+221 1278.5,457.7 200015 gray(255)
  16034: 411x510+175+2629 378.8,2880.8 193193 gray(255)
  15738: 443x461+1897+2606 2118.0,2834.6 186445 gray(255)
  9692: 594x320+974+1484 1269.9,1643.0 178432 gray(255)
  50: 432x538+195+173 416.1,480.0 160433 gray(255)
  1306: 445x403+1902+253 2119.9,449.3 133434 gray(255)
  16762: 411x415+1035+2668 1232.1,2880.7 126869 gray(255)
  9092: 376x460+1918+1444 2107.1,1714.3 118376 gray(255)
  8768: 237x637+257+1406 380.4,1762.2 111040 gray(255)
 */

// draw rectangles around each blob
// convert b3.jpg -fill none -stroke red -draw "rectangle 1044,221 1512,672" x.jpg
// x,y, xOffseet,yOffset centX,centY area
// note that the rectangle is x,y (x+offsetX),(y+offsetY)
// convert logos_bw.jpg -fill none -stroke red -draw "rectangle 1044,221 1512,692" -draw "rectangle 175,2629 586,3139" -draw "rectangle 1897,2606 2340,3067" -draw "rectangle 974,1484 1568,1804" -draw "rectangle 195,173 627,711" -draw "rectangle 1902,253 2347,656" -draw "rectangle 1035,2668 1446,3083" -draw "rectangle 1918,1444 2294,1904" -draw "rectangle 257,1406 494,2043"  x.jpg

// extract out a part of the image
// convert logos_bw.jpg -crop 468x471+1044+221 +repage out1.jpg
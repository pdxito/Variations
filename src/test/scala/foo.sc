/*
u: Update raw data and merge
r: Generate Ratios from raw data
d: Draw ratio data
f: Find similarities
s: Draw similiar stocks
p: Next sector
,: One less day
.: One more day

TODO: Draw dates across top so don't have to scroll
 */


import spray.json._
import scala.io.Source
import spray.json.DefaultJsonProtocol._

val info = Source.fromFile("/Users/marcus.vincent/Development/sankeySimple/inf_funnel.json", "UTF-8").getLines().toArray
val x = info.mkString("")

val jsValue = """{"name": "spinach", "color": "green"}""".parseJson



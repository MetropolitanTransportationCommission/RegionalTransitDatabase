#' get a Route Pattern ID
#' @param dataframe
#' @returns a vector which combines the agency id, route id, and direction id in a string 

# if (!require(devtools)) {
#   install.packages('devtools')
# }
# devtools::install_github("rtopojson", username="tombuckley")

library(readr)
library(rgdal)
library(gdalUtils)
library(rjson)
library(rtopojson)
library(rgeos)

#'write a spatial dataframe to the current working directory as a geopackage (with date in name-seconds since the epoch)
#'@param spatial dataframe
#'@return nothing
write_to_geopackage_with_date <- function(spdf) {
  library(rgdal)
  the_name <- deparse(substitute(spdf))
  writeOGR(spdf,
           paste0(format(Sys.time(),"%s"),the_name,"_",".gpkg"),
           driver="GPKG",
           layer = the_name, 
           overwrite_layer = TRUE)
}

d1 <- "C:/projects/RTD/RegionalTransitDatabase"
setwd(d1)

j1 <- fromJSON(readLines("http://projects.planbayarea.org/assets/js/rtpLines.json"))

name <- names(j1$objects)
geometries <- j1$objects[[name]]$geometries
arcs <- j1$arcs
scale <- j1$transform$scale
translate <- j1$transform$translate

spdf2 <- arcs2sp.line(arcs,scale,translate)

#get shape index (18) in browser, with text filter, add 1, b/c r indexes at 1
geneva_arcs <- j1$objects$linesRTP$geometries[[18+1]]$arcs

#filter both sources down to just what needs to be added
spdf2 <- spdf2[(geneva_arcs+1),]

proj4string(spdf2) <- CRS("+proj=longlat +datum=WGS84")
spdf2 <- spTransform(spdf2, CRS("+init=epsg:26910"))

spdf2_a <- gBuffer(spdf2,width=804.672)
spdf2_b <- gBuffer(spdf2,width=402.336)

geneva_route_1_2_mile <- SpatialPolygonsDataFrame(spdf2_a,data=data.frame(x=1,row.names=row.names(spdf2_b)))
geneva_route_1_4_mile <- SpatialPolygonsDataFrame(spdf2_b,data=data.frame(x=1,row.names=row.names(spdf2_b)))

write_to_geopackage_with_date(geneva_route_1_2_mile)
write_to_geopackage_with_date(geneva_route_1_4_mile)


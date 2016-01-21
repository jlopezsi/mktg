oldwd<-getwd()
setwd("../eurostatAtlas")
getwd()
install.packages(c("rgdal", "RColorBrewer", "sp", "GISTools", "classInt", "maptools","SmarterPoland"))
library("rgdal")
library("RColorBrewer")
library("sp")
library("GISTools")
library("classInt")
library("maptools")
library("SmarterPoland")
# create a new empty object called 'temp' in which to store a zip file
# containing boundary data
temp <- tempfile(fileext = ".zip")
# now download the zip file from its location on the Eurostat website and
# put it into the temp object
download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/NUTS_2010_60M_SH.zip", mode="wb",
             temp)

download.file("http://epp.eurostat.ec.europa.eu/cache/GISCO/geodatafiles/NUTS_2010_60M_SH.zip", 
              destfile = "NUTS_2010_60M_SH.zip")
# now unzip the boundary data
unzip("NUTS_2010_60M_SH.zip")
unzip(temp)
#These are not the only boundary data available - 
#the full range of shapefiles and geodatabases can be explored here: 
#http://epp.eurostat.ec.europa.eu/portal/page/portal/gisco_Geographical_information_maps/popups/references/administrative_units_statistical_units_1


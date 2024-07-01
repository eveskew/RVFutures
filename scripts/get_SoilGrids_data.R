library(XML)
library(rgdal)
library(gdalUtils)

# Get SoilGrids data for east Africa region

#==============================================================================


# Bounding box for the study area in Homolosine projection
bb <- c(3e6, 1e6, 5e6, -2e6) 
# Projection string for Homolosine projection
igh <- "+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs"

quantity <- "mean"
wcs.service <- "SERVICE=WCS"
wcs.version <- "VERSION=2.0.1" 

# Loop through variables of interest and pull down data
for(voi in c("bdod", "cfvo", "clay", "nitrogen", "phh2o", "sand", "silt", "soc")) {
  
  for(depth in c("0-5cm", "5-15cm", "15-30cm")) {
    
    voi.layer <- paste(voi, depth, quantity, sep = "_")
    wcs.path <- paste0("https://maps.isric.org/mapserv?map=/map/", voi, ".map")
    wcs <- paste(wcs.path, wcs.service, wcs.version, sep = "&")
    l1 <- newXMLNode("WCS_GDAL")
    l1.s <- newXMLNode("ServiceURL", wcs, parent = l1)
    l1.l <- newXMLNode("CoverageName", voi.layer, parent = l1)
    saveXML(l1, file = "./sg.xml")
    
    file.out <- paste0("data/rasters/soil/SoilGrids/", voi.layer, ".tif")
    
    gdal_translate(
      "./sg.xml", file.out,
      tr = c(250, 250), 
      projwin = bb, projwin_srs = igh, 
      co = c("TILED=YES", "COMPRESS=DEFLATE", "PREDICTOR=2", "BIGTIFF=YES"),
      verbose = TRUE
    )
    
    file.remove("./sg.DC.xml")
    file.remove("./sg.xml")
  }
}

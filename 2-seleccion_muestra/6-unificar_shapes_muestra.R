# load packages
library(raster)
library(rgdal)
library(sp)
library(rgeos)
library(maptools)

##### Merge muestra polygons

states_muestra = list.dirs(path = "/LUSTRE/sacmod/validacion_madmex/2017-12-09_muestra_datos_n_1229",
    full.names = TRUE, recursive = FALSE)

for (state in states_muestra)
{
  # list of shapes in state SAMPLE folder
  shps_muestra = list.files(state, pattern="\\.shp$",full.names = TRUE)
  
  state_name = basename(state)
  
  print(state_name)
  
  shapefile_1 = readOGR(shps_muestra[1],ogrListLayers(shps_muestra[1])[1],verbose=FALSE)
  for (i in 2:length(shps_muestra))
  {
    # read next shapefile
    shapefile_2 = readOGR(shps_muestra[i],ogrListLayers(shps_muestra[i])[1],verbose=FALSE)
    
    # adjust polygon ids
    idx_1 = 1:nrow(shapefile_1)
    idx_2 = (max(idx_1)+1):(max(idx_1)+length(shapefile_2))
    
    shapefile_1 <- spChFIDs(shapefile_1[,c("oid","edo","tile","predicted")], as.character(idx_1))
    shapefile_2 <- spChFIDs(shapefile_2[,c("oid","edo","tile","predicted")], as.character(idx_2))
    
    shapefile_1 <- spRbind(shapefile_1,shapefile_2)
  }
  
  # create output directory if it doesn't exist
  dir.create(paste0("/LUSTRE/sacmod/validacion_madmex/2017-12-09_muestra_datos_merged_n_1229/", state_name), showWarnings = FALSE)
  
  # write to disk
  writeOGR(shapefile_1, paste0("/LUSTRE/sacmod/validacion_madmex/2017-12-09_muestra_datos_merged_n_1229/",state_name,"/",state_name,"_muestra.shp"),
           paste0(state_name,"_muestra"), driver="ESRI Shapefile",overwrite_layer = TRUE)
}


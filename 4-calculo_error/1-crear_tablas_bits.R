# PENDIENTE!!!
# cambiar en readOGR stringsAsFactors = FALSE y por tanto cambiar función 
# homogeneizar


### mapa bits
library(rgdal) # leer shapes
library(rgeos) # calcular áreas
library(dplyr)
library(purrr)
library(sp)
library(rslurm)

# Los shapes están organizados en carpetas por estado, dentro de cada carpeta
# están los tiles con polígonos en cada estado

read_edo <- function(path_edo){
    paths_files <- list.files(path_edo, recursive = FALSE)
    tiles_numbers <- unique(readr::parse_number(paths_files))
    map(tiles_numbers, function(i) try(read_ogr(path_edo = path_edo, 
        tile = i )))
}

read_ogr <- function(path_edo, tile){
    print(path_edo)
    print(tile)
    shp_tile <- NULL
    try(shp_tile  <- readOGR(path_edo, 
        stringr::str_c(tile, "_interpreta_finalcut")))
    if(is.null(shp_tile)){
        return(data.frame(oid = NA, id = NA, cluster = NA, predicted = NA, 
            interpreta = NA, area_r = NA, edo = basename(path_edo), tile = tile))
    }else{
        shp_tile$area_r <- gArea(shp_tile, byid = TRUE)
        # shp_tile$area_r2 <- sapply(shp_tile@polygons, function(x) x@Polygons[[1]]@area)
        shp_tile$edo <- basename(path_edo)
        shp_tile$tile <- tile
        return(shp_tile@data)
    }
}

# primera entrega (descargados de FTP eoss)
# bits_chiapas <- read_edo("/LUSTRE/sacmod/validacion_madmex/datos/chiapas")
# segunda entrega
# # ni mi compu ni en el cluster pude leer el gdb (muy grande)
# # mapa_ref <- readOGR("CSR25_CHP_2015.gdb", layer = "CSR25_CHI_2015")
# # guardé la tabla de atributos desde QGis
# cr25_chp_2015 <- read_csv("datos_entrega/cr25_chp_2015.csv")
# no incluye variable tile y no puedo unir a tabla de muestra

# tercera entrega Chiapas
bits_chiapas <- read_edo("/LUSTRE/MADMEX/mapa_referencia_2015/final/chiapas")
bits_chiapas_df <- map_df(bits_chiapas, homogeneizar_bits) 

save(bits_chiapas, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2018-01-17_lista_bits_chiapas.RData")
save(bits_chiapas_df, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2018-01-17_bits_chiapas_df.RData")

# distintas columnas:
# "predicted", "area", "oid", "id", "interpreta", "area_r", "edo", "tile"
# "oid", "id", "cluster", "predicted", "interpreta", "area_r", "edo", "tile"  

### Tile con una observación: 1546311
# Tile vacío: 1547215

# Oaxaca

bits_oaxaca <- read_edo("/LUSTRE/MADMEX/mapa_referencia_2015/final/oaxaca")
# revisar columnas
map(bits_oaxaca, colnames)

bits_oaxaca_df <- map_df(bits_oaxaca, homogeneizar) 
bits_oaxaca_df <- bits_oaxaca_df %>% 
    select(edo, oid, id, tile, predicted, interpreta, area_r)

save(bits_oaxaca, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2018-01-24_lista_bits_oaxaca.RData")
save(bits_oaxaca_df, file = "datos_procesados/2018-01-24_bits_oaxaca_df.RData")

# PENDIENTE!!!
# cambiar en readOGR stringsAsFactors = FALSE y por tanto cambiar función 
# homogeneizar

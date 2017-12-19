### Revisión marco
library(rgdal) # leer shapes
library(rgeos) # calcular áreas
library(tidyverse)
library(sp)
library(raster)

# cargamos funciones
source("procesamiento/funciones_leer_shapes.R")

# load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/marco_muestral.Rdata")
load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_marco_muestral.Rdata")

# estratos bien definidos
marco_muestral %>% 
    group_by(area_cat) %>% 
    summarise(
        media_area = mean(area), 
        min_area = min(area), 
        max_area = max(area)
        )

marco_muestral %>%
    group_by(edo) %>%
    summarise(area_edo = sum(area)) %>%
    write.table(sep = ",")

load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_pais_df.Rdata")

pais_df %>%
    group_by(edo) %>%
    summarise(area_edo = sum(area_r, na.rm=TRUE)) %>%
    write.table(sep = ",")

# ahora calculamos el área con los shapes que se usaron para cortar los tiles
paths_edos_inegi <- list.dirs("/LUSTRE/MADMEX/eodata/footprints/shapes_estados_mexico_proyeccion_inegi_lcc", 
    recursive = FALSE)
length(paths_edos_inegi)

pais_df <- map_df(paths_edos_inegi, read_edo_shp)
write.table(pais_df, sep = ",")


# crear marco muestral a partir de polígonos etiquetados por madmex y
# homogeneizados a un tamaño mínimo de media hectárea.

# 1. Leemos los shapes por estado, para cada estado hay un directorio con un 
#       shapefile para cada tile Rapid Eye
# 2. Iteramos sobre los edos y calculamos el área de cada polígono
# 3. Creamos un data.frame donde cada linea representa a un polígono y las
#       variables son estado, área, área categorizada, clase madmex. 

library(rgdal) # leer shapes
library(rgeos) # calcular áreas
library(dplyr)
library(purrr)
library(sp)
library(rslurm)

# Los shapes están organizados en carpetas por estado, dentro de cada carpeta
# están los tiles con polígonos en cada estado

setwd("/LUSTRE/sacmod/validacion_madmex/pruebas_rslurm/")

read_edo <- function(path_edo){
    paths_files <- list.files(path_edo, recursive = FALSE)
    tiles_numbers <- unique(readr::parse_number(paths_files))
    map_df(tiles_numbers, function(i) try(read_ogr(path_edo = path_edo, tile = i)))
}

read_ogr <- function(path_edo, tile){
    print(path_edo)
    print(tile)
    shp_tile <- NULL
    try(shp_tile  <- readOGR(path_edo, tile))
    if(is.null(shp_tile)){
        return(data.frame(id = NA, area_r = NA, edo = basename(path_edo), tile = tile))
    }else{
        shp_tile$area_r <- gArea(shp_tile, byid = TRUE)
        # shp_tile$area_r2 <- sapply(shp_tile@polygons, function(x) x@Polygons[[1]]@area)
        shp_tile$edo <- basename(path_edo)
        shp_tile$tile <- tile
        return(shp_tile@data)
    }
}

# shapes cortados por Erick
paths_edos <- list.dirs("/LUSTRE/MADMEX/tw/entrega2/", recursive = FALSE)
params_df <- dplyr::data_frame(path_edo = paths_edos)

sjob <- slurm_apply(read_edo, params = params_df, jobname = "test_job", 
    nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus"), 
    add_objects = c("read_ogr"))
print_job_status(sjob)
res_raw <- get_slurm_out(sjob, outtype = "raw", wait = FALSE)
#save(res_raw, "/LUSTRE/sacmod/validacion_madmex/pruebas_rslurm/res_raw.RData")

pais_df <- map_df(res_raw, ~.x)
# save(pais_df, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_pais_df.Rdata")

load("datos_procesados/2017-08-18_pais_df.Rdata")
pais_df_tidy <- pais_df %>%
    rename(id_original = id) %>% 
    dplyr::mutate(id = 1:n()) %>%
    dplyr::select(id_original, oid, tile, id, clase = predicted, edo, area = area_r) 

pais_df_tidy %>% 
    filter(area < 5000) %>%
    dplyr::summarise(
        area_total = sum(area), 
        area_porcentaje = round(100 * area_total / sum(pais_df_tidy$area, na.rm = TRUE), 1), 
        poligonos_total = n(), 
        poligonos_porcentaje = round(100 * poligonos_total / nrow(pais_df_tidy), 1))

# los polígonos de menos de media hectárea representan el 37% de los polígonos
# y el 4% del área total

pais_df_tidy %>% 
    filter(clase %in% c(0, 98, 99)) %>%
    dplyr::summarise(
        area_total = sum(area), 
        area_porcentaje = round(100 * area_total / sum(pais_df_tidy$area, na.rm = TRUE), 1), 
        poligonos_total = n(), 
        poligonos_porcentaje = round(100 * poligonos_total / nrow(pais_df_tidy), 1))

# Los polígonos en estas clases son únicamente el 0.1% y el 0.1% del área total

aux <- pais_df_tidy %>%
    filter(area >= 5000) 

marco_muestral <-  aux %>%
    filter(!(clase %in% c(0, 98, 99))) %>% 
    mutate(area_cat = case_when(
        .$area <= 50000 ~ "[5000,50000]",  
        .$area <= 100000 ~ "(50000,100000]", 
        .$area <= 500000 ~ "(100000,500000]", 
        TRUE ~ "(500000,1200000]"))

marco_muestral %>%
    group_by(area_cat) %>% 
    summarise(
        min = min(area), 
        median = median(area), 
        max = max(area)
    )

table(marco_muestral$area_cat)

# creamos los estratos
glimpse(marco_muestral)
marco_muestral <- marco_muestral %>%
    mutate(estrato = paste(clase, edo, area_cat, sep = "-"))

# save(marco_muestral, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/marco_muestral.Rdata")
save(marco_muestral, file = "datos_procesados/2018-03-01_marco_muestral.Rdata")


### estratos sin área para muestra más chica
load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_marco_muestral.Rdata")

marco_muestral <- marco_muestral %>% 
    select(-estrato, -area_cat) %>% 
    mutate(estrato = paste(clase, edo, sep = "-"))

save(marco_muestral, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-09_marco_muestral.Rdata")

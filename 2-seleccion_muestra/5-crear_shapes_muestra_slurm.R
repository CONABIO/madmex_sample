# Tomar muestra
library(rgdal) # leer shapes
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(rslurm)

setwd("/LUSTRE/sacmod/validacion_madmex/procesamiento/muestra_rslurm")

# para cada edo-tile extrae los polígonos en la muestra
extrae_shp <- function(edo_val, tile_val, dir_edos){
    poligonos <- filter(muestra_datos, edo == edo_val, tile == tile_val)
    path_shp = str_c("/LUSTRE/MADMEX/tw/entrega2/", edo_val)
    # ruta para guardar shape
    ruta_shp <- str_c(dir_edos, edo_val)
    # leer shape 
    shp <- readOGR(path_shp, tile_val)
    shp$edo <- edo_val
    shp$tile <- tile_val
    writeOGR(shp[shp$oid %in% poligonos$oid, ], ruta_shp, tile_val, 
        driver = "ESRI Shapefile", verbose = FALSE, overwrite_layer = TRUE)
    "listo"
}

# revisa la tabla de la muestra y crea carpetas con los shapes por estado
# hay un shape por cada tile incluído 
calcula_params <- function(file_muestra_datos){
    load(file_muestra_datos)
    # extraer shapes para muestra
    edo_tile <- muestra_datos %>% 
        distinct(edo, tile) %>% 
        arrange(edo, tile)
    
    # creamos directorios 
    edos <- unique(edo_tile$edo)
    dir_edos <- str_c("/LUSTRE/sacmod/validacion_madmex/", 
        tools::file_path_sans_ext(basename(file_muestra_datos)), "/")
    dir.create(dir_edos)
    walk(edos, ~dir.create(str_c(dir_edos, .)))
    

    # para RSLURM los nombres de las columnas deben coinicidir con nombres de los 
    #   argumentos de la función
    edo_tile <- edo_tile %>% 
        mutate(dir_edos = dir_edos)
    colnames(edo_tile) <- c("edo_val", "tile_val", "dir_edos")

    return(list(edo_tile = edo_tile, muestra_datos = muestra_datos))
}


# muestra calculada en crear_shapes_muestra.R
# n =1000
file_muestra_datos <- "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-08_muestra_datos_n_1659.Rdata"
params <- calcula_params(file_muestra_datos)
muestra_datos <- params$muestra_datos
sjob <- slurm_apply(extrae_shp, params = params$edo_tile, jobname = "shapes_job", 
    nodes = 3, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "4", ntasks = "4"), add_objects = "muestra_datos")

print_job_status(sjob)


# n =2000
file_muestra_datos <- "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-08_muestra_datos_n_1718.Rdata"
params <- calcula_params(file_muestra_datos)
muestra_datos <- params$muestra_datos
sjob <- slurm_apply(extrae_shp, params = params$edo_tile, jobname = "shapes_job", 
    nodes = 3, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "4", ntasks = "4"), add_objects = "muestra_datos")

print_job_status(sjob)

# n = 5000
file_muestra_datos <- "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-08_muestra_datos_n_5097.Rdata"
params <- calcula_params(file_muestra_datos)
muestra_datos <- params$muestra_datos
sjob <- slurm_apply(extrae_shp, params = params$edo_tile, jobname = "shapes_job", 
    nodes = 3, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "4", ntasks = "4"), add_objects = "muestra_datos")

print_job_status(sjob)

## n = 1229
file_muestra_datos <- "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-09_muestra_datos_n_1229.Rdata"
params <- calcula_params(file_muestra_datos)
muestra_datos <- params$muestra_datos
sjob <- slurm_apply(extrae_shp, params = params$edo_tile, jobname = "shapes_job", 
    nodes = 3, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "4", ntasks = "4"), add_objects = "muestra_datos")

print_job_status(sjob)

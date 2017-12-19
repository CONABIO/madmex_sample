library(rgdal) # leer shapes
library(rgeos) # calcular áreas
library(dplyr)
library(purrr)
library(sp)
library(rslurm)

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
        # shp_tile$area_r <- gArea(shp_tile, byid = TRUE)
        shp_tile$edo <- basename(path_edo)
        shp_tile$tile <- tile
        return(shp_tile@data)
    }
}

# shapes cortados por Erick
paths_edos <- list.dirs("/LUSTRE/sacmod/validacion_madmex/muestra_shp_slurm_chica", recursive = FALSE)
params_df <- dplyr::data_frame(path_edo = paths_edos)

sjob <- slurm_apply(read_edo, params = params_df, jobname = "verificar_chica_job", 
    nodes = 3, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "3", ntasks = "3"), add_objects = c("read_ogr"))
print_job_status(sjob)
res_raw <- get_slurm_out(sjob, outtype = "raw", wait = FALSE)

muestra_df <- map_df(res_raw, ~.x)
head(muestra_df)

setwd("/LUSTRE/sacmod/validacion_madmex/pruebas_rslurm/_rslurm_verificar_chica_job")

results_list <- list.files(".", "results", 
    full.names = TRUE) %>% 
    map(readRDS) %>% 
    flatten_df()


#### Verificar muestra merged

read_edo <- function(path_edo){
    print(path_edo)
    shp_edo  <- readOGR(path_edo, stringr::str_c(basename(path_edo), "_muestra"))
    # shp_tile$area_r <- gArea(shp_tile, byid = TRUE)
    shp_edo$edo <- basename(path_edo)
    return(shp_edo@data)
}


paths_edos <- list.dirs("/LUSTRE/sacmod/validacion_madmex/2017-12-09_muestra_datos_merged_n_1229", 
    recursive = FALSE)
params_df <- dplyr::data_frame(path_edo = paths_edos)

sjob <- slurm_apply(read_edo, params = params_df, jobname = "verificar_merged_chica_job", 
    nodes = 3, cpus_per_node = 2, slurm_options = list(partition = "optimus", 
        nodes = "4", ntasks = "4"))
print_job_status(sjob)
res_raw <- get_slurm_out(sjob, outtype = "raw", wait = FALSE)
muestra_df <- do.call(rbind, res_raw)


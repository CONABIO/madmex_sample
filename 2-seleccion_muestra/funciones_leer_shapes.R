# funciones para leer shapes

# recibe el path a la carpeta de un estado y crea un data.frame que reune
# la información de todos los tiles del estado, incluyendo: area (m2), prediction 
# (clase de cobertura) y estado
read_edo <- function(path_edo){
    paths_files <- list.files(path_edo, recursive = FALSE)
    tiles_numbers <- unique(parse_number(paths_files))
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

# calcula el área de cada estado
read_edo_shp  <- function(path_edo){
    print(path_edo)
    shp_edo <- readOGR(path_edo, basename(path_edo))
    shp_edo$area_r <- gArea(shp_edo, byid = TRUE)
    shp_edo@data
}

# Tomar muestra
library(rgdal) # leer shapes
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# se utilizan los óptimos numéricos ya que demostraron más estabilidad que los 
# analíticos al hacer pruebas (Fernando).

# cargamos marco muestral
load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_marco_muestral.Rdata")

# cargamos asignaciones óptimas n=15,000, p = 0.5
# asignacion_op <- readRDS("/LUSTRE/sacmod/optimizacion_madmex/asignacion_optima_p_0.5.RData")
# asignacion_op <- readRDS("datos_procesados/asignacion_optima_p_0.5.RData")

# cargamos asignaciones óptimas n=10,000, p = 0.5
asignacion_op <- readRDS("/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-10-28_asignacion_optima_p_0.5_n_9271.RData")

# redondeamos asignación para tener enteros
asignacion_df <- asignacion_op$df_optimos %>%
    mutate(n_muestra = round(optimo_numerico)) %>% 
    select(estrato, n_muestra)

# al redondear la muestra aumenta de n = 9271 a n = 9573

# calculamos tamaño de muestra para los estratos que se van a censar 
# estos son los que tienen tamaño de muestra <=10
tamano_muestra <- marco_muestral %>% 
    left_join(asignacion_df) %>% 
    group_by(estrato) %>% 
    summarise(
        n_poligonos = n(), 
        n_muestra = first(n_muestra),
        n_muestra = ifelse(is.na(n_muestra), n_poligonos, n_muestra),
        area_estrato = sum(area)) %>% 
    ungroup()

glimpse(tamano_muestra)

# tamaño final = 10302
sum(tamano_muestra$n_muestra)

# sacamos muestra
set.seed(38573)
muestra <- marco_muestral %>% 
    left_join(tamano_muestra, by = "estrato") %>% 
    split(.$estrato) %>% 
    map_df(~sample_n(., size = first(.$n_muestra)))

# save(muestra, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-04_muestra.Rdata")
load("/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-04_muestra.Rdata")

# cargamos datos originales para unir info de shape
load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_pais_df.Rdata")

pais_df_tidy <- pais_df %>%
    dplyr::mutate(id = 1:n()) %>%
    dplyr::select(id, clase = predicted, oid, edo, tile, area = area_r) 

muestra_datos <- left_join(muestra, pais_df_tidy) 

save(muestra_datos, file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-04_muestra_datos.Rdata")

################################################################################

### Lo hacemos función
library(rgdal) # leer shapes
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

seleccionar_muestra <- function(df_optimos, 
    file_marco_muestral = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_marco_muestral.Rdata", 
    seed = 38573){
    
    # cargamos marco muestral
    load(file = file_marco_muestral)
    
    # redondeamos asignación para tener enteros
    asignacion_df <- df_optimos %>%
        mutate(n_muestra = round(optimo_numerico)) %>% 
        select(estrato, n_muestra)
    
    # al redondear la muestra aumenta de n = 9271 a n = 9573
    
    # calculamos tamaño de muestra para los estratos que se van a censar 
    tamano_muestra <- marco_muestral %>% 
        left_join(asignacion_df) %>% 
        group_by(estrato) %>% 
        summarise(
            n_poligonos = n(), 
            n_muestra = first(n_muestra),
            n_muestra = ifelse(is.na(n_muestra), n_poligonos, n_muestra),
            area_estrato = sum(area)) %>% 
        ungroup()
    
    # tamaño final
    tamano_final <- sum(tamano_muestra$n_muestra)
    print("tamaño final:")
    print(tamano_final)
    
    # sacamos muestra
    set.seed(seed)
    muestra <- marco_muestral %>% 
        left_join(tamano_muestra, by = "estrato") %>% 
        split(.$estrato) %>% 
        map_df(~sample_n(., size = first(.$n_muestra)))
    
    # cargamos datos originales para unir info de shape
    load(file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-08-18_pais_df.Rdata")
    
    pais_df_tidy <- pais_df %>%
        dplyr::mutate(id = 1:n()) %>%
        dplyr::select(id, clase = predicted, oid, edo, tile, area = area_r) 
    
    muestra_datos <- left_join(muestra, pais_df_tidy) 
    
    save(muestra_datos, file = stringr::str_c("/LUSTRE/sacmod/validacion_madmex/datos_procesados/",
        Sys.Date(), "_muestra_datos_n_", tamano_final, ".Rdata"))
    
    return(muestra_datos)
}

asignacion_op <- readRDS("/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-08_lista_resultados_optimizacion_n_mayor_2.RData")

### Necesitamos al menos dos observaciones por estrato para poder calcular 
# errores estándar

# muestra tamaño n = 1000 --> muestras de tamaño cero en algunos estratos!
df_optimos_n_1000 <- asignacion_op$constantes_optimizacion_p_0.5_n_1000$df_optimos
muestra_datos <- seleccionar_muestra(df_optimos_n_1000)


# muestra tamaño n = 2000 --> muestras de tamaño cero en algunos estratos!
df_optimos_n_2000 <- asignacion_op$constantes_optimizacion_p_0.5_n_2000$df_optimos
muestra_datos <- seleccionar_muestra(df_optimos_n_2000)


# muestra tamaño n = 5000 --> muestras de tamaño cero en algunos estratos!
df_optimos_n_5000 <- asignacion_op$constantes_optimizacion_p_0.5_n_5000$df_optimos
muestra_datos <- seleccionar_muestra(df_optimos_n_5000)


#### Para poder tener muestras chicas creamos estratos sin usar área categórica
# obtenemos n = 622 estratos con un tamaño mínimo de muestra de 1229

file_marco_muestral <- "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-09_marco_muestral.Rdata"
load(file_marco_muestral)
df_optimos <- marco_muestral %>% 
    select(estrato) %>% 
    count(estrato) %>% 
    mutate(optimo_numerico = pmin(n, 2))

muestra_datos <- seleccionar_muestra(df_optimos = df_optimos, 
    file_marco_muestral = file_marco_muestral)



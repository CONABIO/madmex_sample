library(rgdal) # leer shapes
library(rgeos) # calcular áreas
library(dplyr)
library(purrr)
library(sp)
library(quickcount)

# 1. Select a sample from the ~15000 polygons that BITS will classify
# selected sample for accuracy measurements
muestra <- readOGR("datos_muestra/2017-09-05_madmex_validacion_n_15000/nacional", 
    "muestra_nacional", stringsAsFactors = FALSE)
# add id
muestra$id_sample <- stringr::str_c(muestra$oid, muestra$edo, muestra$tile, 
    sep = "-")
# create stratum variable 
muestra_estrato <- muestra@data %>%
    mutate(estrato = stringr::str_c(edo, predicted, sep = "-"))

# ignore Chiapas and Oaxaca
submuestra <- muestra_estrato %>% 
    filter(!(edo %in% c("Oaxaca", "Chiapas"))) 
# sample size is minimum between stratum size and 3
submuestra_sizes <- submuestra %>% 
    count(estrato) %>% 
    mutate(sample_size = pmin(n, 3)) 
# stratified random sampling
submuestra_n_1705 <- select_sample_str(submuestra, submuestra_sizes, 
    stratum = estrato, sample_size = sample_size, seed = 719873)
# shape sample
submuestra_n_1705_shp <- muestra[muestra$id_sample %in% 
        submuestra_n_1705$id_sample, ]
writeOGR(submuestra_n_1705_shp, "datos_procesados/", "muestra_revision", 
    driver = "ESRI Shapefile", verbose = FALSE, overwrite_layer = TRUE)

# increase sample size to ~3000
submuestra_2 <- submuestra %>% 
    filter(!(id_sample %in% submuestra_n_1705$id_sample))
submuestra_n_1195 <- select_sample_prop(submuestra_2, 
    stratum = estrato, frac = 0.105, seed = 719873)

# shape sample
submuestra_n_1195_shp <- muestra[muestra$id_sample %in% 
        submuestra_n_1195$id_sample, ]
writeOGR(submuestra_n_1195_shp, "datos_procesados/", "muestra_revision_complemento", 
    driver = "ESRI Shapefile", verbose = FALSE, overwrite_layer = TRUE)

# Select a sample from the ~7000 polygons BITS already classified

# Shape con los puntos de la muestra, se "perdieron" 41
validation <- readOGR("datos_entrega/validation/", 
    "muestra_nacional_points_orig_usv6_ief11_bits_4326", 
    stringsAsFactors = FALSE)
validation$id_sf <- stringr::str_c(validation$oid, validation$edo, 
    validation$tile, sep = "-")

# Revisión BITS y revisión muestra
revision_conabio <- readOGR("datos_entrega/revision_vs_conabio/", 
    "mdr_revision_20180404_unique_choices_vs_pedro", stringsAsFactors = FALSE)

revision_unique <- readOGR("datos_entrega/revision/", 
    "mdr_revision_20180404_unique_choices", stringsAsFactors = FALSE)

# 7139 distinct points according to identifier
revision_multiple <- readOGR("datos_entrega/revision/", 
    "mdr_revision_20180404_multiple_choices", stringsAsFactors = FALSE)

reviewed_polygons <- unique(c(revision_unique$identifier, 
    revision_multiple$identifier))

# 7942 reviwed polygons -> 7403 available for sample
# Exclude polygons in Oaxaca and Chiapas
sampling_frame <- validation@data %>% 
    mutate(
        identifier = as.numeric(nid), 
        predicted = as.numeric(predicted)
        ) %>% 
    filter(nid %in% reviewed_polygons, !(edo %in% c("Oaxaca", "Chiapas"))) %>%
    mutate(estrato = stringr::str_c(edo, predicted, sep = "-"))

sample_strata <- sampling_frame %>% 
    count(estrato) %>% 
    mutate(sample_size = pmin(n, 3)) 

# stratified random sampling
muestra_n_1671 <- select_sample_str(sampling_frame, sample_strata, 
    stratum = estrato, sample_size = sample_size, seed = 719873)
# complement sample
sample_complement <- sampling_frame %>% 
    filter(!(identifier %in% muestra_n_1671$identifier))
muestra_n_1333 <- select_sample_prop(sample_complement, 
    stratum = estrato, frac = 0.23, seed = 719873)

# selected sample for accuracy measurements
muestra <- readOGR("datos_muestra/2017-09-05_madmex_validacion_n_15000/nacional", 
    "muestra_nacional", stringsAsFactors = FALSE)
# add id
muestra$id_sf <- stringr::str_c(muestra$oid, muestra$edo, muestra$tile, 
    sep = "-")
ids_df <- validation@data %>% 
    select(identifier = nid, id_sf)
muestra@data <- left_join(muestra@data, ids_df)
# shape sample
muestra@data <- muestra@data[, c("id_sf", "identifier")]
muestra_n_1671_shp <- muestra[muestra$id_sf %in% muestra_n_1671$id_sf, ]
muestra_n_1333_shp <- muestra[muestra$id_sf %in% muestra_n_1333$id_sf, ]

writeOGR(muestra_n_1671_shp, "datos_procesados/muestra_revision_bits", 
    "muestra_revision", driver = "ESRI Shapefile", verbose = FALSE, 
    overwrite_layer = TRUE)

writeOGR(muestra_n_1333_shp, "datos_procesados/muestra_revision_bits", 
    "muestra_revision_complemento", driver = "ESRI Shapefile", verbose = FALSE, 
    overwrite_layer = TRUE)

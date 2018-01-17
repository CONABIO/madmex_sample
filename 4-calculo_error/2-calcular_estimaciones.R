library(rgdal)
library(tidyverse)

# datos revisados por experto
chiapas <- readOGR("datos_entrega/2017-11-06_chiapas/", "ChisSegmVal_p")
chiapas_val <- chiapas@data %>% 
    mutate(oid = as.numeric(as.character(oid_1))) %>% 
    select(oid, edo, tile, predicted, Interpr1_p, Interpr2_p)
glimpse(chiapas_val)    
# variables: 
# predicted - MADMEX
# Interpr1_p, Interpr2_p (interpretaciones Pedro)

# necesitamos calcular M_h el tamaño de cada cluster
load("datos_procesados/2017-08-18_marco_muestral.Rdata")

estratos_area <- marco_muestral %>% 
    filter(edo == "Chiapas") %>% 
    group_by(estrato) %>% 
    summarise(
        M_h = sum(area), 
        N_h = n()
    )
rm(marco_muestral)

# preparamos los datos, agregando el estrato
load("datos_procesados/2017-08-18_pais_df.Rdata")
chiapas_df <- filter(pais_df, edo == "Chiapas")
rm(pais_df)


chiapas_val_df <- chiapas_val %>% 
    left_join(chiapas_df, by = c("oid", "tile")) %>% 
    select(oid, tile, predicted = predicted.y, edo = edo.x, area_r, 
        Interpr1_p, Interpr2_p) %>% 
    mutate(area_cat = case_when(
        .$area_r <= 50000 ~ "[5000,50000]",  
        .$area_r <= 100000 ~ "(50000,100000]", 
        .$area_r <= 500000 ~ "(100000,500000]", 
        TRUE ~ "(500000,1200000]"),
        estrato = paste(predicted, edo, area_cat, sep = "-"), 
        y_h = (predicted == Interpr1_p | predicted == Interpr2_p)
    ) %>% 
    left_join(estratos_area)

glimpse(chiapas_val_df)


## % área clasificado correcto por Madmex (predicted)
source("4-calculo_error/funciones_estimadores.R")
p_hs <- chiapas_val_df %>% 
    group_by(estrato) %>%
    do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
    left_join(estratos_area, by = c("estrato"))

strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
    N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)

### Bits
load(file = "datos_procesados/2018-01-17_bits_chiapas_df.RData")
chiapas_bits_df <- chiapas_val_df %>% 
    left_join(bits_chiapas_df, by = c("oid", "tile")) %>% 
    rename(area_r = area_r.x)

p_hs <- chiapas_bits_df %>% 
    mutate(y_h = (interpreta == Interpr1_p | interpreta == Interpr2_p))%>% 
    left_join(estratos_area) %>% 
    group_by(estrato) %>%
    do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
    left_join(estratos_area, by = c("estrato"))

strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
    N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)


### Madmex
p_hs <- chiapas_bits_df %>% 
    mutate(y_h = (predicted.y == Interpr1_p | predicted.y == Interpr2_p))%>% 
    left_join(estratos_area) %>% 
    group_by(estrato) %>%
    do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
    left_join(estratos_area, by = c("estrato"))

strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
    N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)

### bosque no bosque
## madmex
p_hs <- chiapas_bits_df %>% 
    mutate(y_h = ((predicted.y <= 27) == (Interpr1_p <= 27) | 
            (predicted.y <= 27) == (Interpr2_p <= 27)))%>% 
    left_join(estratos_area) %>% 
    group_by(estrato) %>%
    do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
    left_join(estratos_area, by = c("estrato"))

strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
    N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)

## bits
p_hs <- chiapas_bits_df %>% 
    mutate(y_h = ((interpreta <= 27) == (Interpr1_p <= 27) | 
            (interpreta <= 27) == (Interpr2_p <= 27)))%>% 
    left_join(estratos_area) %>% 
    group_by(estrato) %>%
    do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
    left_join(estratos_area, by = c("estrato"))

strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
    N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)




### antes de funciones (para revisar)

# estimador de área correcta
chiapas_est <- chiapas_val_df %>% 
    mutate(y = (predicted == Interpr1_p | predicted == Interpr2_p)) %>% 
    group_by(estrato) %>% 
    summarise(
        n = n(),
        p_area = 100 * sum(area_r * y) / sum(area_r)
    ) %>% 
    left_join(estratos_area)

# estimación de área correcta (32 clases)
chiapas_est %>% 
    summarise(sum(p_area * area) / sum(area))

# estimación de error estándar
chiapas_val_df %>% 
    mutate(y_h = (predicted == Interpr1_p | predicted == Interpr2_p)) %>% 
    group_by(estrato) %>% 
    mutate(
        p_hat_h = sum(area_r * y_h) / sum(area_r), 
        aux = area_r ^ 2 * (y_h - p_hat_h) ^ 2
    ) %>% 
    summarise(
        suma_interior_h = sum(aux),
        m_bar_h = mean(area_r),
        n_h = n(), 
    ) %>% 
    left_join(estratos_area) %>% 
    mutate(suma_ext = (1 - n_h / N_h) * (area / sum(area)) ^ 2 * 1 / 
            (n_h * m_bar_h ^ 2) * 1 / (n_h - 1) * suma_interior_h) %>%
    summarise(sum(suma_ext, na.rm = T))

sqrt(0.002)

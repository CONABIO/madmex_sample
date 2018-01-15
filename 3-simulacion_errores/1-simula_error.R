library(dplyr)
library(purrr)
library(tidyr)

# a diferencia de la función en 2-seleccionar_muestra.R se eliminó la semilla
# y la línea que guarda la muestra en LUSTRE
seleccionar_muestra <- function(df_optimos, marco_muestral){
    # redondeamos asignación para tener enteros
    asignacion_df <- df_optimos %>%
        mutate(n_muestra = round(optimo_numerico)) %>% 
        select(estrato, n_muestra)

    # calculamos tamaño de muestra para los estratos que se van a censar 
    tamano_muestra <- marco_muestral %>% 
        left_join(asignacion_df, by = "estrato")
    
    # tamaño final
    tamano_final <- sum(tamano_muestra$n_muestra)
    print("tamaño final:")
    print(tamano_final)
    
    # sacamos muestra
    muestra <- tamano_muestra %>% 
        split(.$estrato) %>% 
        map_df(~sample_n(., size = first(.$n_muestra)))
    
    return(muestra)
}

# cargamos el marco muestral
file_marco_muestral <- "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-09_marco_muestral.Rdata"
load(file_marco_muestral)

# simulamos un error en la población con p = 0.5
set.seed(1819374)
marco_muestral_sim <- marco_muestral %>% 
    mutate(error = sample(c(0, 1), size = nrow(marco_muestral), replace = TRUE))
   
# Descripción de datos simulados
# % área correcta 
marco_muestral_sim %>% 
    summarise(
        p_poligonos = mean(error), 
        p_area = sum(error * area) / sum(area)
    )
#  p_poligonos    p_area
#  0.5000223 0.5000531

df_optimos <- marco_muestral %>% 
    select(estrato) %>% 
    count(estrato) %>% 
    mutate(optimo_numerico = pmin(n, 2))

# simulamos la selección de n muestras
muestras_sim <- rerun(50, seleccionar_muestra(df_optimos = df_optimos, 
    marco_muestral = marco_muestral_sim))

save(muestras_sim, 
    file = "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2018-01-12_sims_muestras.Rdata")

load("datos_procesados/2018-01-11_sims_muestras.Rdata")

# estimamos usando una sola muestra
load("datos_procesados/2017-12-09_marco_muestral.Rdata")
estratos_area <- marco_muestral %>% 
    group_by(estrato) %>% 
    summarise(
        M_h = sum(area), 
        N_h = n()
    ) %>% 
    ungroup()

evalua_muestra <- function(muestra){
    p_hs <- muestra %>% 
        left_join(estratos_area) %>% 
        group_by(estrato) %>%
        do(ratio_est(M = .$area, y = .$error, N = .$N_h[1])) %>%
        left_join(estratos_area, by = c("estrato"))

    strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
        N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)   
}


# estimamos con muestras simuladas
est <- map(muestras_sim, evalua_muestra)
mean(est)    
sd(est)

bind_rows(!!!est) %>% 
    summarise(mean(p_hat), sd(p_hat), mean(se_p_hat))


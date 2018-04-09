library(tidyverse)
library(rgdal)
library(survey)

source("4-calculo_error/funciones_estimadores.R")
load("datos_procesados/2017-08-18_marco_muestral.Rdata")
load("datos_procesados/2017-08-18_pais_df.Rdata")


calcular_ests <- function(estado, folder_datos_experto, capa_datos_experto, file_bits_edo){
    edo_shp <- readOGR(folder_datos_experto, capa_datos_experto)
    edo_val <- edo_shp@data %>% 
        mutate(oid = as.numeric(as.character(oid_1))) %>% 
        select(oid, edo, tile, predicted, Interpr1_p, Interpr2_p)

    # variables: 
    # predicted - MADMEX
    # Interpr1_p, Interpr2_p (interpretaciones Pedro)

    # necesitamos calcular M_h el tamaño de cada cluster
    # load("datos_procesados/2017-08-18_marco_muestral.Rdata")

    estratos_area <- marco_muestral %>% 
        filter(edo == estado) %>% 
        group_by(estrato) %>% 
        summarise(
            M_h = sum(area), 
            N_h = n()
        )
    # rm(marco_muestral)

    # preparamos los datos, agregando el estrato, oid es necesario para unir los 
    # datos y no está incluída en el marco muestral (valdria la pena cambiar eso)
    # load("datos_procesados/2017-08-18_pais_df.Rdata")
    edo_df <- filter(pais_df, edo == estado)
    # rm(pais_df)

    edo_val_df <- edo_val %>% 
        left_join(edo_df, by = c("oid", "tile")) %>% 
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



    ## % área clasificado correcto por Madmex (predicted)
    p_hs <- edo_val_df %>% 
        group_by(estrato) %>%
        do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
        left_join(estratos_area, by = c("estrato"))

    est_madmex_area <- strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
        N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)

    ### Bits
    x <- load(file = file_bits_edo)
    bits_edo_df <- get(x)
    edo_bits_df <- edo_val_df %>% 
        left_join(bits_edo_df, by = c("oid", "tile")) %>% 
        rename(area_r = area_r.x)

    p_hs <- edo_bits_df %>% 
        mutate(y_h = (interpreta == Interpr1_p | interpreta == Interpr2_p)) %>% 
        left_join(estratos_area) %>% 
        group_by(estrato) %>%
        do(ratio_est(M = .$area_r, y = .$y_h, N = .$N_h[1])) %>%
        left_join(estratos_area, by = c("estrato"))

    est_bits_area <- strat_est(p_h = p_hs$p_hat, n_h = p_hs$n, M_h = p_hs$M_h,
        N_h = p_hs$N_h, s2_h = p_hs$s2_p_hat)
    
    ## % polígonos clasificados correcto Bits
    p_porcent <- edo_bits_df %>% 
        mutate(y_h = (interpreta == Interpr1_p | interpreta == Interpr2_p))%>% 
        left_join(estratos_area) %>% 
        group_by(estrato) %>%
        summarise(
            n_h = n(),
            N_h = first(N_h),
            prop_h = N_h / sum(estratos_area$N_h), 
            f_h = n_h / first(N_h),
            p_h = mean(y_h),
            p_aux_h = prop_h * p_h, 
            var_aux = (1 - f_h) * prop_h ^ 2 * p_h * (1 - p_h) / (n_h - 1)
        )
    
    ests_bits_porcent <- c(p_hat = sum(p_porcent$p_aux_h), 
        se_p_hat = sqrt(sum(p_porcent$var_aux[p_porcent$N_h > 1])))
    
    # Madmex
    p_porcent <- edo_bits_df %>% 
        mutate(y_h = (predicted.x == Interpr1_p | predicted.x == Interpr2_p)) %>% 
        left_join(estratos_area) %>% 
        group_by(estrato) %>%
        summarise(
            n_h = n(),
            N_h = first(N_h),
            prop_h = N_h / sum(estratos_area$N_h), 
            f_h = n_h / first(N_h),
            p_h = mean(y_h),
            p_aux_h = prop_h * p_h, 
            var_aux = (1 - f_h) * prop_h ^ 2 * p_h * (1 - p_h) / (n_h - 1)
        )
    
    ests_madmex_porcent <- c(p_hat = sum(p_porcent$p_aux_h), 
        se_p_hat = sqrt(sum(p_porcent$var_aux[p_porcent$N_h > 1])))
    

    
    ### paquete survey
    edo_svy <-  edo_bits_df %>% 
        mutate(
            id = stringr::str_c(oid, tile, sep = "-"),
            y_madmex = (predicted.x == Interpr1_p | predicted.x == Interpr2_p),
            y_M_madmex = (predicted.x == Interpr1_p | predicted.x == Interpr2_p) * area_r, 
            y_bits = (interpreta == Interpr1_p | interpreta == Interpr2_p),
            y_M_bits = (interpreta == Interpr1_p | interpreta == Interpr2_p) * area_r
        ) %>% 
        select(id, N_h, y_madmex, y_M_madmex, y_bits, y_M_bits, estrato, area_r)
    edo_design <- svydesign(id = ~ id, strata = ~ estrato, fpc = ~ N_h, 
        data = edo_svy)
    # Print for comparison
    print("% Área Madmex")
    svy_area_madmex <- svyratio(numerator = ~ y_M_madmex, denominator = ~ area_r, design = edo_design)
    print(svy_area_madmex)
    print("% Área Bits")
    svy_area_bits <- svyratio(numerator = ~ y_M_bits, denominator = ~ area_r, design = edo_design)
    print(svy_area_bits)
    
    print("% Madmex")
    svy_percent_madmex <- svymean(~ y_madmex, edo_design)
    print(svy_percent_madmex)
    print("% Bits")
    svy_percent_bits <- svymean(~ y_bits, edo_design)
    print(svy_percent_bits)

    return(list(est_madmex_area = est_madmex_area, 
        est_bits_area = est_bits_area, 
        ests_madmex_porcent = ests_madmex_porcent, 
        ests_bits_porcent = ests_bits_porcent))
}


### Chiapas
folder_datos_experto <- "datos_entrega/2017-11-06_chiapas/"
capa_datos_experto <- "ChisSegmVal_p"
estado <- "Chiapas"
file_bits_edo <- "datos_procesados/2018-01-17_bits_chiapas_df.RData"

ests_chiapas <- calcular_ests(estado, folder_datos_experto, capa_datos_experto, file_bits_edo)


### Oaxaca
# datos revisados por experto

folder_datos_experto <- "datos_entrega/2018-01-24_oaxaca/"
capa_datos_experto <- "OaxSegmVal_p"
estado <- "Oaxaca"
file_bits_edo <- "datos_procesados/2018-01-24_bits_oaxaca_df.RData"

ests_oaxaca <- calcular_ests(estado = estado, folder_datos_experto, 
    capa_datos_experto, file_bits_edo = file_bits_edo)



### Pendiente

# tablas de contingencia
# exactitudes por clase


diag <- edo_val_df %>% 
    group_by(predicted) %>% 
    summarise(area = sum((Interpr1_p == predicted) * area_r))


reng <- edo_val_df %>% 
    group_by(predicted) %>% 
    summarise(area = sum(y_h * area_r))


edo_svy <-  edo_bits_df %>% 
    mutate(
        id = stringr::str_c(oid, tile, sep = "-"),
        class_madmex = factor(predicted.x, levels = 1:32),
        class_bits = factor(interpreta, levels = 1:32),
        class_expert_1 = factor(Interpr1_p, levels = 1:32),
        class_expert_2 = factor(Interpr2_p, levels = 1:32),
        y_madmex = (predicted.x == Interpr1_p | predicted.x == Interpr2_p),
        y_M_madmex = (predicted.x == Interpr1_p | predicted.x == Interpr2_p) * area_r, 
        y_bits = (interpreta == Interpr1_p | interpreta == Interpr2_p),
        y_M_bits = (interpreta == Interpr1_p | interpreta == Interpr2_p) * area_r
    ) %>% 
    select(id, class_madmex, class_expert_1, class_expert_2, class_bits,
        N_h, y_madmex, 
        y_M_madmex, y_bits, y_M_bits, estrato, area_r
        )

edo_design <- svydesign(id = ~ id, strata = ~ estrato, fpc = ~ N_h, 
    data = edo_svy)

edo_design_w <- svydesign(id = ~ id, strata = ~ estrato, fpc = ~ N_h, 
    weights = ~ area_r,
    data = edo_svy)

madmex_expert_1 <- svytable(~ class_madmex + class_expert_1, design = edo_design_w)
svytable(~ class_madmex + class_expert_2, design = edo_design)

diag_madmex_expert_1 <- diag(madmex_expert_1)
user_accuracy <- round(diag_madmex_expert_1 / apply(madmex_expert_1, 1, sum) * 100, 2)
producer_accuracy <- round(diag_madmex_expert_1 / apply(madmex_expert_1, 2, sum) * 100, 2)


table(edo_svy$class_madmex, edo_svy$class_expert_1)

### paquete srvyr
library(srvyr)
edo_svy <-  edo_bits_df %>% 
    mutate(
        id = stringr::str_c(oid, tile, sep = "-"),
        class_madmex = factor(predicted.x, levels = 1:32),
        class_bits = factor(interpreta, levels = 1:32),
        class_expert_1 = factor(Interpr1_p, levels = 1:32),
        class_expert_2 = factor(Interpr2_p, levels = 1:32),
        y_madmex = (predicted.x == Interpr1_p),
        y_M_madmex = (predicted.x == Interpr1_p) * area_r, 
        y_bits = (interpreta == Interpr1_p),
        y_M_bits = (interpreta == Interpr1_p) * area_r
    ) %>% 
    select(id, class_madmex, class_expert_1, class_expert_2, class_bits,
        N_h, y_madmex, 
        y_M_madmex, y_bits, y_M_bits, estrato, area_r
    )

edo_design <- edo_svy %>% 
    as_survey_design(ids = id, strata = estrato, fpc = N_h)

edo_design %>% 
    summarise(
        est_area_madmex = survey_ratio(numerator = y_M_madmex, denominator = area_r), 
        est_area_bits = survey_ratio(numerator = y_M_bits, denominator = area_r)
        )

edo_design %>% 
    group_by(class_madmex) %>% 
    summarise(
        est_area_madmex = survey_ratio(numerator = y_M_madmex, denominator = area_r), 
    ) %>% 
    write.table(sep = ",", row.names = FALSE)

edo_design %>% 
    group_by(class_bits) %>% 
    summarise(
        est_area_bits = survey_ratio(numerator = y_M_bits, denominator = area_r)
    ) %>% 
    write.table(sep = ",", row.names = FALSE)


edo_design %>% 
    group_by(class_expert_1) %>% 
    summarise(
        est_area_madmex = survey_ratio(numerator = y_M_madmex, denominator = area_r), 
        est_area_bits = survey_ratio(numerator = y_M_bits, denominator = area_r)
    )

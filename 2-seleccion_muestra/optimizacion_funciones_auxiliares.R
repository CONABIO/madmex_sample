# Dado el problema de estimar la proporción de área correctamente clasificada
# por MAD-MEX, las funciones definidas en este script permiten obtener la
# distribución óptima de muestra por estrato.

# Las funciones anteriores asumen la existencia de un marco muestral, es decir,
# un data frame que contiene las siguientes columnas:
# id: id de polígono (int)
# clase: clase predicha para cada poligono (int)
# edo: estado al que pertenece cada polígono (chr)
# area: área de cada polígono (dbl)
# area_cat: categoría de área de cada polígono (chr)
# estrato: estrato al que pertenece cada polígono (chr)

# Las columnas principales son id, área y estrato.

#Referencias: "../design-v3.html".

library("plyr")
library("dplyr")
library("doMC")

# La siguiente función permite filtrar un marco muestral, al eliminar todos los
# polígonos pertenecientes a estratos de tamaño menor o igual a cierto valor
# "n_max_censados".
# La idea es que dichos estratos se censarán, por lo que sus correspondientes
# polígonos no se deben considerar en el problema de optimización.

# Parámetros:
# df: el data frame que representa al marco muestral.
# nombre_columna_estrato: nombre de la columna que tiene la información del estrato
# al que pertenece cada polígono.
# n_max_censo: tamaño máximo de los estratos que serán censandos.
# Cualquier estrato de tamaño menor o igual a "n_max_censo" será censado, por lo
# que se eliminará del marco muestral considerado en la optimización.

# La función regresa una lista con dos elementos:
# 1. El marco muestral obtenido al eliminar los polígonos pertenecientes a estratos
# que serán censados.
# 2. El número de polígonos en estratos que serán censados.

filtra_marco_muestral <- function(df, nombre_columna_estrato, n_max_censo){
  
  marco_muestral_filtrado <- df %>%
    group_by_(nombre_columna_estrato) %>%
    mutate(
      n = n()
    ) %>%
    filter_(paste0("n > ", n_max_censo)) %>%
    select(
      -n
    ) %>%
    ungroup()
  
  numero_poligonos_censados <- nrow(df) - nrow(marco_muestral_filtrado)
  
  resultado <- list(
    marco_muestral_filtrado = marco_muestral_filtrado,
    numero_poligonos_censados = numero_poligonos_censados
  )
  
  return(resultado)
}

################################################################################

# La siguiente función simula parámetros poblacionales desconocidos a partir de
# un marco muestral. La idea básica es que eligiendo p, la proporción de polígonos
# correctamente clasificados, se correrán repeticiones con distintos polígonos
# individuales correctamente clasificados y se calcularán los parámetros
# poblacionales de interés:
# 1. Sw^2 = la varianza poblacional de la variable wi = Miyi = el área de un
# polígono si está correctamente clasificado y 0 e.o.c.
# 2. El valor de B = la proporción poblacional de área correctamente clasificada
# 3. El valor de R = la correlación poblacional entre wi y Mi.

# Entradas de la función:
# df: el data frame que representa al marco muestral
# nombre_columna_area: nombre de la columna que tiene la información del área de
# cada polígono (Mi)
# p: proporción de polígonos a tomar como correctamente clasificados
# num_iteraciones: cantidad de veces que se repetirá el proceso de seleccionar
# polígonos correctamente clasificados y calcular los parámetros poblacionales.

# El resultado es un df que contiene las siguientes columnas:
# p, iteración, Sw2, B, R

# Nota: la  función "obtiene_constantes_funcion_objetivo()" requiere de
# esta para operar.

simula_parametros_poblacionales_desconocidos <- function(
  df, nombre_columna_area, p, num_iteraciones){
  
  resultado <- ldply(1:num_iteraciones, function(i, df, nombre_columna_area, p){
    
    # Obteniendo índice de polígonos clasificados correctamente
    indice_clasificados_correctamente <- sample(1:nrow(df), round(p*nrow(df)))
    # Creo que es más eficiente hacer esto que un ifelse
    df[indice_clasificados_correctamente, "yi"] <- TRUE
    
    df_trabajo <- df %>%
      mutate(
        yi = ifelse(is.na(yi), FALSE, TRUE)
      )
    
    # Expresión para calcular wi's en el mutate, por aquello de que
    # "nombre_columna_area" es una variable
    expresion_mutate_ <-list(
      paste0("yi * ", nombre_columna_area),
      nombre_columna_area
    )
    
    # Se estandariza el no,bre de la columna se área para trabajar con ella más
    # fácilmente
    names(expresion_mutate_) <- c("wi", "Mi")
    
    # Calculando df de resúmenes
    resumenes <- df_trabajo %>%
      mutate_(.dots = expresion_mutate_) %>%
      summarise(
        Sw2 = var(wi),
        B = sum(wi) / sum(Mi),
        R = cor(wi, Mi)
      )
    
    resultado <- data_frame(
      p = p,
      iteracion = i,
      Sw2 = resumenes$Sw2,
      B = resumenes$B,
      R = resumenes$R
    )
    return(resultado)
    
  }, df, nombre_columna_area, p, .parallel = TRUE)
  
  return(resultado)
}

################################################################################

# La siguiente función crea una tabla con los parámetros poblacionales, tanto
# conocidos como simulados, necesarios para plantear el problema de distribución
# óptima de la muestra por estrato.

# Entradas de la función:
# df: el data frame que representa al marco muestral
# nombre_columna_estrato: nombre de la columna que contiene la información del
# estrato al que pertenece cada polígono. Ésta es necesaria porque en la función
# objetivo del problema de optimización de interés, los parámetros se deben calcular
# por estrato
# nombre_columna_area: nombre de la columna que contiene la información del área
# de cada polígono.
# p: proporción de polígonos a tomar como correctamente clasificados
# num_iteraciones: cantidad de veces que se repetirá el proceso de seleccionar
# polígonos correctamente clasificados y calcular los parámetros poblacionales.

# La función regresa un data frame que contiene las siguientes columnas:
# -  "nombre_columna_estrato",
# - N: el tamaño de cada estrato.
# - M: el área total de los polígonos de cada estrato
# - SM: la desviación estándar de las áreas de los polígonos de cada estrato.
# - cota_superior_media_Sw: a la raíz cuadrada del promedio de las simulaciones
#   de Sw^2 para cada estrato (Sw2).
# - media_B: la media de las simulaciones de B para cada estrato.
# - media_R: la media de las simulaciones de R para cada estrato.

# Notas:
# 1. Esta función requiere de "simula_parametros_poblacionales_desconocidos()"
#    para operar

obtiene_constantes_funcion_objetivo <- function(
  df, nombre_columna_estrato, nombre_columna_area, p = 0.5, num_iteraciones = 100){
  
  # Obteniendo parámetros poblacionales conocidos por estrato:
  parametros_poblacionales_conocidos_estrato <- df %>%
    group_by_(nombre_columna_estrato) %>%
    summarise_(.dots = list(
      N = "n()",
      M = paste0("sum(", nombre_columna_area,")"),
      SM = paste0("sd(", nombre_columna_area,")")
    )) %>%
    ungroup()
  
  # Obteniendo parámetros poblacionales desconocidos, pero simulados por estrato.
  # Esto con ayuda de "simula_parametros_poblacionales_desconocidos()"
  simulacion_parametros_poblacionales_desconocidos_estrato <- ddply(df,
    nombre_columna_estrato, function(df_estrato){
      simula_parametros_poblacionales_desconocidos(df_estrato, nombre_columna_area, p, num_iteraciones)
    }, .parallel = TRUE)
  
  # Resumiendo los parámetros poblacionales simulados en cada iteración, por estrato,
  # y haciendo el join con los parámetros poblacionales conocidos, para obtener
  # la tabla con constantes para la función objetivo
  
  constantes_funcion_objetivo <-
    simulacion_parametros_poblacionales_desconocidos_estrato %>%
    group_by_(nombre_columna_estrato) %>%
    summarise(
      # Como estoy promediando varianzas, por la desigualdad de Jensen tengo una cota
      # superior para la media de Sw.
      cota_superior_media_Sw = sqrt(mean(Sw2, na.rm = TRUE)),
      media_B = mean(B, na.rm = TRUE),
      media_R = mean(R, na.rm = TRUE)
    ) %>%
    inner_join(parametros_poblacionales_conocidos_estrato, by = nombre_columna_estrato) %>%
    select_(.dots = list(
      nombre_columna_estrato,
      "N",
      "M",
      "SM",
      "cota_superior_media_Sw",
      "media_B",
      "media_R"
    ))
  
  return(constantes_funcion_objetivo)
}

################################################################################

# La siguiente función:
# 1. Plantea el problema de asignación óptima de muestra, utilizando los
# parámetros N, M, SM, cota_superior_media_Sw, media_B y media_R calculados por
# estrato. Este data frame se puede obtener al correr la función:
# "obtiene_constantes_funcion_objetivo()"
# 2. Lo optimizará, tanto numéricamente como de manera analítica (esto último
# heurísticamente).

# Entradas:
# df: data_frame con los parámetros "n, N, M, SM, Sw, B, R" por estrato (renglón)
# Las columnas en el df se deben llamar:
# - N
# - M
# - SM
# - Sw -> cota_superior_media_Sw
# - B -> media_B
# - R -> media_R
# n_total: tamanio de muestra máximo a distribuir entre todos los estratos (int).
# Este parámetro por lo general se calcula como: el número de polígonos total a
# muestrear, menos el número de polígonos en estratos censados (regresado por
# "filtra_marco_muestral()")
# n_min_estrato: tamanio mínimo de muestra por estrato (no censado): Cabe destacar
# que este parámetro es ignorado por el optimizador analítico, el cuál simplemente
# exige que ni > 0.
# epsilon: R utiliza un algoritmo de barrera para resolver el problema de
# optimización, por este motivo, se necesita plantear la restricción de igualdad
# suma(ni) = n_total como un conjunto de restricciones de desigualdad. La idea
# es elegir epsilon > 0 tal que n_total - 2*epsilon > 0
# num_iteraciones: número de iteraciones para el algoritmo de barrera
# La función regresa una lista con:
# - Un df con los óptimos numérico, analítico, y asignación proporcial por estrato
# - El objeto generado por el algoritmo de optimización numérica
# - Un lista que contiene los siguientes valores
#   - El valor de la función objetivo en el óptimo calculado analíticamente
#   - Una lista que, para cada estrato, marca con TRUE si el tamaño de mueestra
#   asignado para determinado estrato en el óptimo analítico es menor que el
#   número de objetos en el mismo, y FALSE e.o.c (en este caso se deberá o
#   corregir, o descartar el óptimo analítico)
encuentra_asignacion_optima <- function(
  df, n_total, n_min_estrato = 0, num_iteraciones=100, epsilon = 200){
  
  #### Optimización numérica del problema especificado con df y n_total
  
  # Definiendo función objetivo/gradiente y funciones generadora de wrappers:
  funcion_objetivo <- function(n, N, M, SM, Sw, B, R){
    valor <- sum((N/n - 1) * N * (Sw^2 - 2*B*R*Sw*SM + B^2*SM^2) / M^2)
    return(valor)
  }
  
  gradiente <- function(n, N, M, SM, Sw, B, R){
    valor <- (-N/n^2 ) * N * (Sw^2 - 2*B*R*Sw*SM + B^2*SM^2) / M^2
    return(valor)
  }
  
  # Definiendo función que define wrappers para la función objetivo / gradiente,
  # dependiendo del data frame de parámetros, es decir, define una función en
  # concreto a optimizar:
  define_wrappers <- function(df){
    N <- df$N
    M <- df$M
    SM <- df$SM
    Sw <- df$cota_superior_media_Sw
    B <- df$media_B
    R <- df$media_R
    
    wrapper_funcion_objetivo <- function(n){
      return(funcion_objetivo(n, N, M, SM, Sw, B, R))
    }
    
    wrapper_gradiente <- function(n){
      return(gradiente(n, N, M, SM, Sw, B, R))
    }
    
    resultado <- list(
      wrapper_funcion_objetivo = wrapper_funcion_objetivo,
      wrapper_gradiente = wrapper_gradiente
    )
    return(resultado)
  }
  
  # Encontrando número de estratos
  num_estratos <- nrow(df)
  # Definiendo wrappers para dicho problema:
  lista_wrappers <- define_wrappers(df)
  # Encontrando cotas superiores para los tamaños de muestra en cada estrato:
  cotas_superiores_n <- df$N
  
  # Creando la matriz de restricciones para constrOptim, ui
  matriz_restricciones <- matrix(nrow = 2 * num_estratos + 2, ncol = num_estratos)
  # Para acotar inferiormente la suma de las ni's
  matriz_restricciones[1,] <- rep(1, num_estratos)
  # Para acotar superiormente la suma de las ni's
  matriz_restricciones[2,] <- rep(-1, num_estratos)
  # Para acotar inferiormente (por 0) cada ni
  matriz_restricciones[3:(num_estratos + 2),] <- diag(num_estratos)
  # Para acotar superiormente (por Ni) cada ni
  matriz_restricciones[(num_estratos + 3):(2 * num_estratos + 2),] <- -diag(num_estratos)
  
  # Creando vector de cotas para las restricciones, ci:
  # Holgura para no tener problemas con valores iniciales, es necesaria pues es
  # un algoritmo de puntos interiores (barrera), pero sabemos que más muestra
  # siempre es mejor, entonces el óptimo siempre tendrá "n_total" unidades.
  n_holgura <- n_total - epsilon
  vector_cotas <- c(
    n_holgura-epsilon,
    -n_holgura-epsilon,
    rep(n_min_estrato, num_estratos), #Cota inferior de n por estrato
    -cotas_superiores_n)
  
  # Punto inicial, theta en constrOptim.
  # Asignación proporcional al área del estrato.
  p0 <- df %>%
    mutate(
      asignacion_proporcional = (.$M / sum(.$M)) * n_holgura
    ) %>%
    pull(asignacion_proporcional)
  #sum(p0)
  
  # Optimizando numérica del problema específico generado a partir de los
  # parámetros en df y n_total
  resultado_optimizacion_numerica <- constrOptim(
    p0,
    lista_wrappers$wrapper_funcion_objetivo,
    lista_wrappers$wrapper_gradiente,
    matriz_restricciones,
    vector_cotas,
    outer.iterations = num_iteraciones
  )
  
  #### Optimización analítica del problema especificado con df y n_total
  
  # Definiendo la función que calcula la optimización analítica.
  # df: data frame con los parámetros del problema citado en "../design-v3.html"
  # por estrato
  # n_total: número total de unidades de muestreo para todos los estratos
  # la función regresa una lista que conteien:
  # Un df con 2 columnas
  #   - "estrato" (obtenidad de "df")
  #   - La asignación óptima de muestra, calculada de manera analítica
  # El valor de la función objetivo en el óptimo analítico
  optimiza_analiticamente <- function(df, n_total){
    estrato <- df$estrato
    N <- df$N
    M <- df$M
    SM <- df$SM
    Sw <- df$cota_superior_media_Sw
    B <- df$media_B
    R <- df$media_R
    
    alpha <- N^2 * (Sw^2 - 2*B*R*Sw*SM + B^2*SM^2) / M^2
    optimo_analitico <- n_total * sqrt(alpha) / sum(sqrt(alpha))
    # Cambiando 0's, ya que desde el punto de vista analítico, no se toman en
    # cuenta, pero para evaluarlos en la función objetivo sí se necesitan.
    optimo_analitico[optimo_analitico == 0] <- 0.01
    
    # Acotando ni sea más grande que Ni (del estrato)
    optimo_analitico <- ifelse(optimo_analitico > N, N, optimo_analitico)
    
    f_optimo_analitico <- funcion_objetivo(optimo_analitico, N, M, SM, Sw, B, R)
    
    resultado <- list(
      df_optimo_analitico = data_frame(
        estrato = estrato,
        optimo_analitico = optimo_analitico
      ),
      f_optimo_analitico = f_optimo_analitico,
      n_estrato_mayor_N_estrato = optimo_analitico > N
    )
    return(resultado)
  }
  
  # Optimizando analíticamente
  resultado_optimizacion_analitica <- optimiza_analiticamente(df, n_total)
  
  # Generando el resultado de la función "encuentra_asignacion_optima":
  resultado <- list(
    df_optimos = resultado_optimizacion_analitica$df_optimo_analitico %>%
      mutate(
        optimo_numerico = resultado_optimizacion_numerica$par,
        asignacion_proporcional = p0
      ),
    metadatos_optimizacion_numerica = resultado_optimizacion_numerica,
    metadatos_optimizacion_analitica = list(
      f_optimo_analitico = resultado_optimizacion_analitica$f_optimo_analitico,
      n_estrato_mayor_N_estrato = 
        resultado_optimizacion_analitica$n_estrato_mayor_N_estrato
    )
  )
  
  if(n_min_estrato > 0){
    paste0("El óptimo obtenido de manera analítica ignoró n_min_estrato > 0, ",
      "se recomienda utilizar el óptimo obtenido numéricamente") %>%
      warning()
  }

  return(resultado)
}

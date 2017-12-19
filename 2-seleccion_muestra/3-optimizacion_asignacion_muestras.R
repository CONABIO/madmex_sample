# Credenciales del cluster:
# Trabajar en http://nodo5.conabio.gob.mx:8787/

# El siguiente script utiliza las funciones en "funciones_auxiliares.R" para
# resolver el problema de asignación óptima con distintos parámetros.

library("ggplot2")
library("doMC")
source("funciones_auxiliares.R")

registerDoMC(20) # Número de núcleos en el cluster.

# 1. Leyendo el marco muestral y obteniendo el marco muestral filtrado:
marco_muestral <- readRDS("/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-09_marco_muestral.Rdata")
info_marco_muestral_filtrado <- filtra_marco_muestral(
  marco_muestral,
  nombre_columna_estrato = "estrato",
  n_max_censo = 2)

marco_muestral_filtrado <- info_marco_muestral_filtrado[["marco_muestral_filtrado"]]
numero_poligonos_censados <- info_marco_muestral_filtrado[["numero_poligonos_censados"]]

# 2. Obteniendo las constantes para la función objetivo:
constantes_funcion_objetivo <- obtiene_constantes_funcion_objetivo(
  marco_muestral_filtrado,
  nombre_columna_estrato =  "estrato",
  nombre_columna_area = "area",
  p = 0.5,
  num_iteraciones = 100
)

# 3. Planteando el problema de optimización y resolviéndolo:
asignacion_optima <- encuentra_asignacion_optima(
  constantes_funcion_objetivo,
  n_total = 3000 - numero_poligonos_censados,
  n_min_estrato = 1,
  num_iteraciones = 100,
  epsilon = 200)

#saveRDS(asignacion_optima, "/LUSTRE/sacmod/validacion_madmex/datos_procesados/2017-12-09_asignacion_optima.Rdata")


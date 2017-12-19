# MAD-MEX Accuracy Assessment

We propose a sample designs to assess the accuracy of the 
MAD-Mex 2015 RapidEye (1:20,000) map (32 cover classes). 

### Madmex
[MAD-MEX](http://madmex.conabio.gob.mx) (Monitoring Activity Data for the 
Mexican REDD+ program) is a system to provide standardized annual wall-to-wall 
land cover information by automatic satellite image classification for the 
Mexican territory in a cost-beneficial manner. One of the aims of the system is 
to automatically produce a national land cover dataset in a standardized, 
consistent, transparent and transferable 
way, to ensure operative activity data monitoring. The integration of data, 
interfaces, processes, into one uniform, consistent and scalable hardware and 
software platform are the core components of MAD-MEX, but also, is the result 
of different governmental mexican organizations that follows international and national standardize guides. 

### Accuracy Assessment
In order to measure the performance of the Mad-Mex system we propose an 
accuracy assesment where experts will revise a sample of Mad-Mex cover classes 
and determine whether the class label is correct or not. 

#### Sample design
The sample design is focused in estimating the overall proportion of area 
correctly classified. 

To accomplish this with the most precision with fixed sample size we propose a 
one-stage stratified sampling: we independently take a Simple Random Sample 
from each stratum.

**Observation Units (spatial assessment unit):** Map polygons, the area within
each polygon has the same map classification, assigned by Madmex. The map is the 
result of an aggregation from madmex *homogeneous* segments (yielding 40m 
polygons per scene).

**Sampling frame:** Set of polygons.  

**Stata:** State (32) x Classes(32) x Area class(5) (not every class is reported on every state).

**Reference data**: RapidEye images 2015, this are the same images that were
input for the classification.

**Reference labeling protocol**: Experts will asign a single reference label to 
each polygon, if a given polygon is not homogeneous the will assign the 
prevalent class.

**Agreement:** For a given unit (polygon) if reference label and map label 
agree the map is correct for that unit.

## Scripts

### funciones_leer_shapes.R
Helper functions to read shapes, compute their area and return `data.frame`s with the information.

### crear_marco_muestral_slurm.R
Creates the sampling frame, consisting on a `data.frame` with the following variables: id, clase (madmex class), edo, 
area, area_cat (area categorized) and estrato.

To create the sampling frame we used *homogeneous* polygons, which are the output from madmex (post-processed to create polygons of at least 5000 m^2^), and divided so that each polygon belongs to only one State.

#### input 
/LUSTRE/MADMEX/tw/entrega2/Estado/tile - RapidEye tiles with classified polygons

#### output
datos_procesados/aaaa-mm-dd_pais_df.Rdata
datos_procesados/aaaa-mm-dd_marco_muestral.Rdata

### validar_marco_muestral.R
Compare the area of the polygons in each state with the area of the corresponding state.

#### input 
procesamiento/funciones_leer_shapes.R - helper functions to read shapes
datos_procesados/aaaa-mm-dd_pais_df.Rdata - sampling frame before tidying
datos_procesados/aaaa-mm-dd_marco_muestral.Rdata - sampling frame
/LUSTRE/MADMEX/eodata/footprints/shapes_estados_mexico_proyeccion_inegi_lcc - Mexico state shapes

#### output
NA

### seleccionar_muestra.R
Select sample, the sample size within each strata is numerically optimized.

#### input 
datos_procesados/aaaa-mm-dd_marco_muestral.Rdata - sampling frame
datos_procesados/aaaa-mm-dd_pais_df.Rdata - sampling frame before tidying
datos_procesados/aaaa-mm-dd_asignacion_optima_p_0.5_n_size.RData - list with sample size per strata

#### output
datos_procesados/aaaa-mm-dd_muestra.Rdata - data.frame with selected polygons
datos_procesados/aaaa-mm-dd_muestra_datos.Rdata - data.frame with selected polygons and tile info.

### crear_shapes_muestra_rslurm.R
Creates shapefiles with the polygons selected in the sample. It creates one file per state-tile.

#### input 
/LUSTRE/MADMEX/tw/entrega2/Estado/tile - RapidEye tiles with classified polygons
datos_procesados/aaaa-mm-dd_muestra_datos.Rdata - data.frame with selected polygons and tile info.

#### output
/LUSTRE/sacmod/validacion_madmex/muestra_shp_slurm/Estado shapes by state-tile with selected polygons

### unificar_shapes_muestra.R
Merges the shapefiles with the sample to create a  unique shapefile per state with the selected polygons.

#### input 
/LUSTRE/sacmod/validacion_madmex/muestra_shp_slurm/Estado shapes by state-tile with selected polygons

#### output
/LUSTRE/sacmod/validacion_madmex/muestra_shp_merged_chica/Estado

## Selection of sample
The scripts need to be run as follows

1. crear_marco_muestral_slurm.R -> aaaa-mm-dd_marco_muestral.Rdata
2. validar_marco_muestral.R 
3. optimizacion_asignacion_muestras -> aaaa-mm-dd_asignacion_optima_p_0.5_n_size.RData  
4. seleccionar_muestra.R -> aaaa-mm-dd_muestra_datos.Rdata 
5. crear_shapes_muestra_slurm.R -> muestra_shp_slurm
6. unificar_shapes_muestra.R -> muestra_shp_merged_chica/Estado





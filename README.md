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

### Notes on Data

* Input data from MADMEX team (Thilo & Steffen): */LUSTRE/MADMEX/tw/combine* organized in folders, each folder corresponds to a RapidEye tile.

* Input data */LUSTRE/MADMEX/tw/combine* was processed by MADMEX team (Erick) to create new polygons so that each polygon belongs to only one state, this is, polygons that belonged to more than one State were divided. This step was needed so that we could define the strata using state. Output data */LUSTRE/MADMEX/tw/entrega*

#### Observations
* Polygons overlap (*/LUSTRE/MADMEX/tw/entrega*) - This issue was taken care of, we got new files (*/LUSTRE/MADMEX/tw/entrega2*).

* Data was not homogeneous, the data from different files had different fields.

The data 

### BITS Data

* 2017/12/08 Data downloaded from FTP eoss origanized in folders according to state/tile. For some tiles the polygons did not match the polygons in the sampling frame.

* 2018/01/15 Data in LUSTRE/mapa_referencia_2015/conjunto one geodatabase (.gdb) per state, polygons appear to match with sampling frame. Attribute table includes: id, oid, predicted, cluster, predicted and interpreta, because it does not include the RE tile it is not possible to directly match information with sample.

* 2018/01/17 Data in LUSTRE/mapa_referencia_2015/final data in folders numbered 01 through 32 (one per state?) in addition we see folders named campeche, chiapas, colima, guerrero, jalisco, michoacan, oaxaca, quintanaroo and yucatan. Pending to explore why there are repeated folders for some states.



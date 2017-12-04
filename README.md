
# IRiR

README IS WORK IN PROGRESS

IRiR is a script for calculating Revenue Caps for Norwegian Electricity DSOs.

_All usage and calculation results are the users responsibility. NVE is not responsible for any result calculated by users outside NVE_


### Brief description of file and folder organization

* NVE_IRiR v1.0 (01.12.2017)
 This file sources all relevant subscripts and performs the revenue cap calculation "through and through".
 
* NVE_HIiR v1.0 (01.12.2017)
 This file can calculate estimates of Harmony Income (compensation for mergers). It depends on the correct usage of the NVE_merges-function   and  calls files from the share "R_Script\Harmony".
 
* Data
 Contains two sub-folders. BaseData and Bootstrap.

 BaseData contains several csv-files that are necessary for the calculation. These are different BaseData-sets, but also a set of IDs for each  DSO. BootStrap contains the latest version of the bootstrap corrected efficiences for use in stage 2, the files will by default be replaced   by  new estimates if code is run with BS.new = 1.

* Harmony
 Similar as the Data-folder, but used for the Harmony Income calculations. HIiR has more bootstraps, before and after relevant merger for analysis.

* R Script
 A folder containing all files needed in the IRiR-file, and also the subfolder Harmony which contains the scripts called by the HIiR-script.
 
 * 0_1_Config_Assumptions_Data.R : Imports dataset, defines important assumtions and parameters (interest rates, CPI-values) etc.
 
 * 0_2_Calculated_Input_Values.R : Calculates values from base data to data ready for DEA.
 
 * 0_3_Company_Selection.R : Distributes all companies in relevant groups to create sub-datasets of dat in later stages.
 
 * 1_0_DEA.R : Stage 1 - Performs the DEA-calculations. Described in report 71/2012 NOR  http://publikasjoner.nve.no/rapport/2012/rapport2012_71.pdf
 
 * 2_0_Stage2_GeoCorrection.R : Stage 2 - Corrects efficiencies from stage 1 for environmental conditions. Described in report 71/2012 and
   technicalites are described here  ENG: http://bit.ly/2sH5oLV .
   
 * 3_0_Stage3_Calibration.R : Cost norm calibration,  described in circular 1/2013 NOR http://webfileservice.nve.no/API/PublishedFiles/Download/201607005/1944365
 
 * 
 
 
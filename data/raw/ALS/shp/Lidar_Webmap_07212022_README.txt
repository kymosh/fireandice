Author: Astrid Sanna astrid87@uw.edu
Date: 7/21/2022

******FIRST THING FIRST: download the shapefile locally before editing it so that the original stays intact.******


This directory (\\172.25.182.82\pfc-frl\01_LiDAR_data\LiDAR_Webmap) contains the ArcPro package under the GIS folder. 
You'll need that package to updtae the LiDAR webmap layer in case of new processed acquisitions.
This package has a toolbox that transform the multiple shapefiles into feauture classes adding 2 new fields.
This step is necessary before mergeing new feature classes with the most recent map layer, which is found under Lidar_Webmap_layer. 
New feature classes and the most recent map layer MUST have the same fields in the attribute table or the merge will not happen. 
Use the R script in the R_scripts folder to create the new shapefile needed for updating the map layer.
You may need to edit the R script to make it work. If you do edit the script save it as a new file, DO NOT edit and save the original R script. 

##################################################################################################################################
AFTER MERGING ALL THE LAYERS, REMEBER TO FILL OUT THE FIELD COLLECTION_YEAR AND NEW_NAME IN THE NEW MERGED MAP LAYER. 
******* NEW_NAME SHOULD ALWAYS BE "NameOfTheAcquisition_year" FOR CONSISTENCY"************
THIS IS A FUNDAMENTAL STEP SINCE IT WILL ALLOW FUTURE GEOSPATIAL ANALYSIS USING THIS MAP AND OTHER LAYERS.
AND SAVE THE NEW UPDATED MAP UNDER LiDAR_Webmap_layer. 
##################################################################################################################################

Check GIS_workflow.docx under LiDAR_Webmap_stuff for more info on how to update the most recent map layer. 
AGOL map here https://uw.maps.arcgis.com/home/item.html?id=54877ffec97a490d8df2246b67912a2e

The last map update is 7/21/2022 and is available on AGOL in the UW Forest Resilience Lab group.



 


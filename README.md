# weather_data_France_SIM2
Calculating meteorological data at the municipality level in France, 2010-2025
This R project aims at calculating the temperature at the spatial level of municipalities (INSEE code) or BDI code1, taking the administrative division of 20232. It also creates heatwave indicators based on various definitions. 
The temperature data used is from the model SIM-2 from Météo-France (SAFRAN: analyse des paramètres météorologiques; ISBA: interaction sol-biosphère-atmosphère; Modcou :modèle hydrogéologique). This data is open access and contains daily meteorological information on a spatial grid of 8km². Data: https://www.data.gouv.fr/datasets/donnees-changement-climatique-sim-quotidienne.
This is done for the years 2010-2025 but more recent data can be downloaded and used as well, following the same process.
Two methods are performed:
1.	Mean estimates weighted by the surface of the intersect grid x municipality polygon
2.	Using the population centroid 
The meteorological data for each municipality is the one at the population centroid of that municipality.
 

More information is given in the readme.docx file

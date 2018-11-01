# VIRB_FIT_data_processing
R scripts to process ".csv" datasets obtained by parsing the .FIT files using the FitToCSV.bat (available from the FitSDK).

Garmin devices, such as action cameras and wearables, store the data from their integrated sensors (e.g. accelerometers, gyroscopes, gps) in ".fit" files. 
These can be parsed using the FitToCSV.bat program provided in the SDK to obtain a ".csv". 
However, sensor data in the .csv file are not immediately usable because of the format of the .csv (e.g. multiple values in a cell) and because sensor readings are provided as COUNT as opposed to real word units of measure (e.g. lat/lon degree, g, m/s, etc...).


The scripts in this repository aim at parsing the .csv file obtained from the FitToCSV.bat, using R, to:
- produce a dataset formatted for data analysis
- convert data from COUNT to the approriate unit of measure.

The main aim of these R scripts is to parse data obtained from the action camera Garmin VIRB Ultra 30, but it is possible that the can be used or easily adjusted for other devices.
Information needed to interpret the csv file and convert COUNTS is provided in the FitSDK domumentation (https://www.thisisant.com/developer/resources/downloads/)

Any contribution is welcome.

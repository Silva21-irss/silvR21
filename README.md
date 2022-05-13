# SilvR21 
![license](https://img.shields.io/badge/License-R--package-green) 

R package for accessing and processing climate data from ClimateNA with the Silva21 project.
The **silvR21** package provides functions to prepare Digital Elevation Models (DEM) as TIFF files into CSV files, access historical and future projected climate data, and process the files in a similar fashion to how it has been used thus far within Silva21. Annual and seasonal means/sums can be computed from monthly data along with personalized GCM ensemble generators.

## Key Features
#### Historical Climate Access
As an alternative to using the desktop application, `hist30YClimateNA`, `hist10YClimateNA`, and `histAnnualClimateNA` permit the user to access historical means for monthly, seasonal, and annual mean data over 30 years periods. These functions have the advantage of using DEM files located anywhere on your computer.
#### Future Climate Projections Access
The `projClimateNA20Y` tool enhances the access abilities with ClimateNA as it is easier to select GCM models, SSP scenarios, and date ranges by adjusting a few parameters. The DEM file used to access projected data must be located within the same directory as the ClimateNA application.
#### Converting DEM file to CSV ready for use
The guidelines for preparing a DEM file so that it can be correctly formatted for use with ClimateNA can be difficult to make properly. Errors are common and it is not always obvious how to correct them. The `demToCSV` tool is specifically designed to prepare any DEM TIFF file in the correct CSV format so that it can be inputted into either `historicalClimateNA` or `projClimateNA20Y`.
#### Generating GCM Ensembles
As an alternative to using the 13GCM ensembles that are available with ClimateNA, the `ensembleGenerator` tool prepares an ensemble from any list of GCMs provided that all data has been accessed.
#### Converting Monthly Data into Annual and Seasonal Means and Sums
Rather than downloading all monthly, seasonal, and annual data, `AnnualSeasonalMeans` uses all of the variables from monthly data and calculates the means (for temperature and relative humidity) and sums (for precipitation, degree days, frost-free days, evaporation, moisture deficit, solar radiation, and climate moisture index) on seasonal and annual factors.
#### Reprojecting ASCII Files
After using the ASCII DEM file format as input within the ClimateNA desktop application, the resulting outputs are often not projected properly, so they would not appear in the correct location once inputted into ArcGIS. `reprojectASCII` will automatically reproject these images.

## About
**silvR21** is developed openly at [UBC](https://www.ubc.ca/) through [Silva21](https://www.silva21.com/).
* Development of the **silvR21** package started in 2022.

## Install **silvR21** Package
* First, you will need to install the `devtools` package.
* Install the **silvR21** package through GitHub with `devtools::install_github("Silva21-irss/silvR21")`

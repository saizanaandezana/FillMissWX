# FillMissWX Modeling Interface [![DOI](https://sandbox.zenodo.org/badge/371678125.svg)](https://sandbox.zenodo.org/badge/latestdoi/371678125)

To reliably estimate missing weather data we developed FillMissWX, a tool in the EcoHydRology R package (Fuka et al., 2014), to automatically download daily weather data of Global Historical Climatology Network (GHCN) monitors and fill and interpolate missing data

## Links
See the following links for more information on  `R` and `RStudio` download and installation:

- An introduction to `R`: <https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf>
- `R` download: <https://www.r-project.org/>
- `RStudio` download: <https://www.rstudio.com/>

There is also a cloud-based `RStudio` sever at the following location:

- Cloud-based `RStudio` server: <https://rstudio.cloud/>

See the following links for more information on `GHCN` data:

- Global Historical Climatology Network daily (GHCNd): <https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily>
- Community Collaborative Rain, Hail, and Snow (CoCoRaHS): <https://www.cocorahs.org/>
- Cooperative Observer Program (COOP):<https://www.weather.gov/coop/Overview>
## Description
FillMissWX tool is a new modeling interface that has been developed and added to the  CRAN EcoHydRology package (Fuka et al., 2014). This tool enables users to : 

**(1)** Automatically download GHCN daily weather data including precipitation, maximum and minimum temperature from all monitors that are located within the user-defined distance of a location with the help of “rnoaa” R package library (Edmund et al., 2016)

**(2)** Fill missing data using neighboring monitors data based on three estimation methods including Inverse Distance Weighting (IDW) (Hubbard 1994), Inverse Distance and Elevation Weighting (IDEW) (Liston & Elder, 2006; Zhang et al., 2017), and closest station (Wallis et al. 1991)

**(3)** Automatically generate plots of weather variables including the sources and numbers of GHCN monitors used (CoCoRaHS, COOP, ECA&D, WMOID, NM/HC, RAWS, SNOTEL) and their distances from the target location.

The required **inputs** to run the FillMissWX function are latitude **(declat)** and longitude **(declon)** of the location of interest, the radius within which to search for monitors from the target location in kilometers **(StnRadius)**, the minimum number of monitors from which data need to be downloaded **(minstns)**, the earliest **(date_min)** and latest **(date_max)** date of interest, the elevation of the target location (km) in IDEW method **(targElev)**, the method to use to fill missing weather data including **“closest”**, **“IDW”**, and **“IDEW”**  (method), the weighting power in IDW and IDEW methods with the default value of 2 **(alfa, 1-6)**, and the print format “png” or “pdf” format **(printinto)**. 
The **outputs** of the FillMissWX function include a data frame containing filled precipitation **(P)** (mm), snow fall **(S)** (mm), snow depth **(SD)** (mm), maximum temperature **(MaxTemp)** and minimum temperature **(MinTemp)** (°C), the weighted-average elevation of monitors used for precipitation **(prcpElev)**, snow fall **(snowElev)**, snow depth **(snwdElev)**, maximum temperature **(tmaxElev)**, and minimum temperature **(tminElev)**.

## Quick start

### R packages that need to be installed:
•   rnoaa
•   EcoHydRology
•   ggplot2
•   moments
•   tidyverse
•   viridis

        if (!require("pacman")) install.packages("pacman")
        pacman::p_load(rnoaa,EcoHydRology,SWATmodel,ggplot2,moments,tidyverse,viridis)

#### Example
##### Using the latitude and longitude of USGS 04282650 (LITTLE OTTER CREEK AT FERRISBURG, VT)

         flowgage_id="04282650" 
         flowgage=get_usgs_gage(flowgage_id,begin_date = "2010-01-01",end_date = "2022-01-01")
         WXData=FillMissWX(declat = flowgage$declat,declon = flowgage$declon,StnRadius = 30,date_min="2010-01-01",date_max="2022-01-01",method = "IDW",minstns =10)

# License
Please see the LICENSE.md file for license information.

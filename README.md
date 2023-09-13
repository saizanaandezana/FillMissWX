# FillMissWX Modeling Interface

## Description
FillMissWX tool is a new modeling interface that has been developed and added to the  CRAN EcoHydRology package (Fuka et al., 2014). This tool enables users to : 

**(1)** Automatically download GHCN daily weather data including precipitation, maximum and minimum temperature from all monitors that are located within the user-defined distance of a location with the help of “rnoaa” R package library (Edmund et al., 2016)

**(2)** Fill missing data using neighboring monitors data based on three estimation methods including Inverse Distance Weighting (IDW) (Hubbard 1994), Inverse Distance and Elevation Weighting (IDEW) (Liston & Elder, 2006; Zhang et al., 2017), and closest station (Wallis et al. 1991)

**(3)** Automatically generate plots of weather variables including the sources and numbers of GHCN monitors used (CoCoRaHS, COOP, ECA&D, WMOID, NM/HC, RAWS, SNOTEL) and their distances from the target location.

The required **inputs** to run the FillMissWX function are latitude **(declat)** and longitude **(declon)** of the location of interest, the radius within which to search for monitors from the target location in kilometers **(StnRadius)**, the minimum number of monitors from which data need to be downloaded **(minstns)**, the earliest **(date_min)** and latest **(date_max)** date of interest, the elevation of the target location (km) in IDEW method **(targElev)**, the method to use to fill missing weather data including **“closest”**, **“IDW”**, and **“IDEW”**  (method), the weighting power in IDW and IDEW methods with the default value of 2 **(alfa, 1-6)**, and the print format “png” or “pdf” format **(printinto)**. 
The **outputs** of the FillMissWX function include a data frame containing filled precipitation **(P)** (mm), maximum temperature **(MaxTemp)** and minimum temperature **(MinTemp)** (°C), the weighted-average elevation of monitors used for precipitation **(prcpElev)**, maximum temperature **(tmaxElev)**, and minimum temperature **(tminElev)**.


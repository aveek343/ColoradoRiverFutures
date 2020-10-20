# LakePowellElevationTempModelInterogate.r
#
# Interigate the Lake Powell Elevation vs Release Temperature spreadsheet model of Yakulic.
# GCD_release_water_temp
# 
# A. Compare against daily min/max temperature data of Glen Canyon Dam release and USBR daily lake elevations 
#
# Uses the following data
# 2. LAKEPOWELL06-16-2020T16.32.29.csv - USBR daily data of reservoir level/storage/release (https://www.usbr.gov/rsvrWater/HistoricalApp.html)
# 3. PowellLevels.xlsx - Definitions of reservoir zones and storage levels (from CRSS/Rosenberg)
# 4. GCD_release_water_temp.csv - Hourly values of Powell release temperature. Provided by Bryce M.
#
# The basis data wrangling strategy is:
# 1. Load csv files
# 2. Join Primary Station and Lake Powell Daily data so we have the water level for each day a reading was taken
# 3. Substract depth from water level to get a level (elevation) for each reading.
#
# David E. Rosenberg
# October 15, 2020
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraies

if (!require(tidyverse)) { 
  install.packages("tidyverse", repos="http://cran.r-project.org") 
  library(tidyverse) 
}

if (!require(readxl)) { 
  install.packages("readxl", repos="http://cran.r-project.org") 
  library(readxl) 
}

  
if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) # 
}

if (!require(dplyr)) { 
  install.packages("dplyr",repos="http://cran.r-project.org") 
  library(dplyr) # 
}

if (!require(expss)) { 
  install.packages("expss",repos="http://cran.r-project.org") 
  library(expss) # 
}

if (!require(reshape)) { 
  install.packages("reshape", repos="http://cran.r-project.org") 
  library(reshape) 
}

if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}


if (!require(pracma)) { 
  install.packages("pracma", repos="http://cran.r-project.org") 
  library(pracma) 
}

if (!require(lubridate)) { 
  install.packages("lubridate", repos="http://cran.r-project.org") 
  library(lubridate) 
}

if (!require(directlabels)) { 
  install.packages("directlabels", repo="http://cran.r-project.org")
  library(directlabels) 
}


#if (!require(plyr)) { 
#  install.packages("plyr", repo="http://cran.r-project.org")
#  library(plyr) 
#}

if (!require(ggrepel)) { 
  devtools::install_github("slowkow/ggrepel")
  library(ggrepel) 
}

library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)


### 0. Definitions

dMetersToFeet = 3.28
sStation = 'LPCR0024' #Wahweap
sMonth = 'Jun' #Month

# New function interpNA to return NAs for values outside interpolation range (from https://stackoverflow.com/questions/47295879/using-interp1-in-r)
interpNA <- function(x, y, xi = x, ...) {
  yi <- rep(NA, length(xi));
  sel <- which(xi >= range(x)[1] & xi <= range(x)[2]);
  yi[sel] <- interp1(x = x, y = y, xi = xi[sel], ...);
  return(yi);
}


### 1. Read IN the data files

# Temperature profile data

sPowellTempProfileFile <- 'qryProfiles at Primary Stations.csv'

# Read in the historical Powell data
dfPowellTempProfiles <- read.csv(file=sPowellTempProfileFile, 
                               header=TRUE, 
                               
                               stringsAsFactors=FALSE,
                               sep=",")


# This historical reservoir level data comes from USBR website.

# File name to read in historical Powell Volume from CSV (download from USBR)
#    Water Operations: Historic Data, Upper Colorado River Division, U.S. Buruea of Reclamation
#    https://www.usbr.gov/rsvrWater/HistoricalApp.html

sPowellHistoricalFile <- 'LAKEPOWELL06-16-2020T16.32.29.csv'

# Read in the historical Powell data
dfPowellHistorical <- read.csv(file=sPowellHistoricalFile, 
                               header=TRUE, 
                               
                               stringsAsFactors=FALSE,
                               sep=",")

# Read in Lake Powell Release Temperature Data (provided by Bryce M.)
sPowellReleaseTempFile <- 'GCD_release_water_temp.csv'

# Read in the historical Powell data
dfPowellReleaseTemp <- read.csv(file=sPowellReleaseTempFile, 
                               header=TRUE, 
                               
                               stringsAsFactors=FALSE,
                               sep=",")
library(chron)
#Convert just the date
dfPowellReleaseTemp$DateClean <- as.Date(dfPowellReleaseTemp$DateTime, "%m/%d/%Y")
#Convert the time
#dfPowellReleaseTemp$DateTimeClean <- as.POSIXct(dfPowellReleaseTemp$DateTime, format="%m/%d/%Y %H:%M:%S")
dfPowellReleaseTemp$DateTimeClean <- mdy_hms(dfPowellReleaseTemp$DateTime)

###This reservoir data comes from CRSS. It was exported to Excel.

# Read pool level data in from Excel
sExcelFile <- 'PowellZones.xlsx'
sStation <- 'LPCR0024'   #Closest to Dam

# Read in the historical Mead data
dfPowellZones <- read_excel(sExcelFile)


# Read in the Elevation-Temperature model
sExcelFileModel <- 'TemperatureModel_GrandCanyonStorage.xlsx'

dfTempElevationModel <- read_excel(sExcelFileModel)

#Define the elevation temperature model by month
TempModel <- function(Month,Elevation) {
  #Month = 1, 2, ..., 12 for January, February, ..., December
  #Elevation in feet
  
  Temperature <- switch(Month,
      5.36+(3.815525648*exp(-(-0.004664035)*((Elevation/3.28084)-1127.76))), #January
      5.667857143+(2.64291514*exp(-(-0.002277994)*((Elevation/3.28084)-1127.76))),  #Feb
      7.343478261+(0.866777569*exp(-(0.009667425)*((Elevation/3.28084)-1127.76))),  #March
      6.759259259+(1.734071491*exp(-(0.007769259)*((Elevation/3.28084)-1127.76))),  #April
      7.112903226+(1.473399599*exp(-(0.018251341)*((Elevation/3.28084)-1127.76))),  #May
      8.095238095+(1.097430498*exp(-(0.031219207)*((Elevation/3.28084)-1127.76))),  #June
      8.115384615+(1.106900875*exp(-(0.044112483)*((Elevation/3.28084)-1127.76))),  #July
      7.910714286+(1.252536876*exp(-(0.044297389)*((Elevation/3.28084)-1127.76))),  #August
      7.788461538+(1.509123384*exp(-(0.040706994)*((Elevation/3.28084)-1127.76))),  #Sept
      7.876923077+(1.5738892*exp(-(0.035494644)*((Elevation/3.28084)-1127.76))),    #Oct
      7.594444444+(1.880906664*exp(-(0.025102979)*((Elevation/3.28084)-1127.76))),  #Nov
      7.587096774+(1.978022304*exp(-(0.011288015)*((Elevation/3.28084)-1127.76))))  #Dec
      
  return(Temperature)
}
  

#Check on TempModel function
for (i in seq(1,12,by=1)){
  print(paste0("Month ", i, ", Calc Temp: ", TempModel(i,as.numeric(dfTempElevationModel[4,i+1])), ", Actual: ", dfTempElevationModel[5,i+1], ", Difference: ",TempModel(i,as.numeric(dfTempElevationModel[4,i+1])) - as.numeric(dfTempElevationModel[5,i+1])))
}

#Calculate Temperatures by model over reservoir elevation range for each month

dfTempElevationModelCalc <- data.frame(Month = 0, Elevation = 0, Temperature = 0)
for(Mon in seq(1,12, by=1)) {
  for(Elev in seq(3300,3700, by=5)) {
    dfTempElevationModelCalc <- rbind(dfTempElevationModelCalc,
                                      data.frame(Month = Mon,Elevation = Elev,
                                                 Temperature = TempModel(Mon,Elev)))
   }
}

#Remove the First row of zeros
dfTempElevationModelCalc <- dfTempElevationModelCalc[2:nrow(dfTempElevationModelCalc),]


###### 2. Plot Release temperature data vs time

## Calculate daily min, max, average, range
dfPowellReleaseTempSum <- dfPowellReleaseTemp %>% group_by(DateClean) %>% summarize(minDay = min(WaterTemp_C),
                                                                                    maxDay = max(WaterTemp_C),
                                                                                    avgDay = mean(WaterTemp_C),
                                                                                    rangeDay = max(WaterTemp_C) - min(WaterTemp_C))


#Pull out Year, Month, Month as abbr, Day for plotting
dfPowellReleaseTempSum$Year <- year(dfPowellReleaseTempSum$DateClean)
dfPowellReleaseTempSum$Month <- month(dfPowellReleaseTempSum$DateClean)
dfPowellReleaseTempSum$MonthTxt <- format(dfPowellReleaseTempSum$DateClean, "%b")
dfPowellReleaseTempSum$Day <- day(dfPowellReleaseTempSum$DateClean)
dfPowellReleaseTempSum$WaterYear <- ifelse(dfPowellReleaseTempSum$Month >= 10,dfPowellReleaseTempSum$Year, dfPowellReleaseTempSum$Year - 1 )
dfPowellReleaseTempSum$DayOfYear <- yday(dfPowellReleaseTempSum$DateClean)

dfDaysPerYear <- dfPowellReleaseTempSum %>% group_by(Year) %>% summarize(numDays = n())
dfDaysPerMonthYear <- dfPowellReleaseTempSum %>% group_by(Year,Month) %>% summarize(numDays = n())

palBlues <- brewer.pal(9, "Blues")
palReds <- brewer.pal(9, "Reds")

palBlueFunc <- colorRampPalette(c(palBlues[3],palBlues[9]))


### 3. Join the Profile and Historical dataframes on the date

# Convert to date format
dfPowellHistorical$dDateTemp <- as.Date(dfPowellHistorical$Date, "%d-%b-%y")
dfPowellTempProfiles$dDate <- as.Date(dfPowellTempProfiles$Date, "%m/%d/%Y")

#Apparently R breaks the century at an odd place
#Coerce the years above 2050 (really 1950 to 1968) to be in prior century (substract 12*100 months)
dfPowellHistorical$Year <- as.numeric(format(dfPowellHistorical$dDateTemp,"%Y"))
dfPowellHistorical$dDate <- dfPowellHistorical$dDateTemp
dfPowellHistorical$dDate <- as.Date(ifelse((dfPowellHistorical$Year >= 2050),
                                           as.character(dfPowellHistorical$dDateTemp %m-% months(12*100)),as.character(dfPowellHistorical$dDateTemp)))
dfPowellHistorical$Year <- as.numeric(format(dfPowellHistorical$dDate,"%Y"))
dfPowellHistorical$Month <- (format(dfPowellHistorical$dDate,"%b"))

### 3. Left join the two dataframes so have an elevation/storage for each temperature profile value
dfPowellTempLevels <- left_join(dfPowellTempProfiles,dfPowellHistorical,by = c("dDate" = "dDate"))

### 4. Calculate an elevation for each measurement
dfPowellTempLevels$Elevation..feet. <- as.numeric(dfPowellTempLevels$Elevation..feet.)
dfPowellTempLevels$MeasLevel <- (dfPowellTempLevels$Elevation..feet.) - dMetersToFeet*dfPowellTempLevels$Depth
dfPowellTempLevels$MonNum <- as.numeric((format(dfPowellTempLevels$dDate,"%m")))

### 5. Filter on station and month

dfPowellTempLevelsPlot <- dfPowellTempLevels %>% filter(Station.ID == sStation)
# Tally Depths per day and Days per month
#dfPowellTempDays <- dcast(dfPowellTempLevelsPlot, Year ~ MonNum, value.var = "MonNum", na.rm = TRUE)

#dfPowellTempLevelsPerDay <- dfPowellTempLevelsPlot %>% group_by(Year,MonNum,dDate) %>% tally()
dfPowellTempLevelsPerDay <- dfPowellTempLevelsPlot %>% group_by(Year,MonNum,dDate) %>% dplyr::summarize(NumLevels = n(), MinTemp=min(T),MaxTemp=max(T))

dfPowellTempLevelsPerDay <- dfPowellTempLevelsPlot %>% group_by(Year,MonNum,dDate, Elevation..feet.) %>% dplyr::summarize(NumLevels = n(), MinTemp=min(T),MaxTemp=max(T), Level3525Temp = interp1(xi=3525, y=T, x=MeasLevel, method="linear" ))
dfPowellTempLevelsPerDay$Zone <- 3525


# Days per month
dfPowellTempDays <- dfPowellTempLevelsPerDay %>% group_by(Year,MonNum) %>% tally()

dfPowellTempDays <- dcast(dfPowellTempDays, Year ~ MonNum, value.var = "n", na.rm = FALSE)
dfPowellTempDays[is.na(dfPowellTempDays)] <- 0 
print("Number of measurements per month")
dfMonthSums <- colSums(dfPowellTempDays[,2:13],dim=1)

dfPlot <- dfPowellTempLevelsPlot

minTemp <- min(dfPlot$T)
maxTemp <- max(dfPlot$T)


#Subsett the columns
cZonesToShow <- c("Top of Dam", "Live Capacity", "Upper Eq. Tier (2019)", "Rated Power", "Upper Basin target", "Minimum Power (from Object)", "Can't release 7.5 maf/year", "Dead Pool (river outlets)")
dfPowellZones$level_feet <- dfPowellZones$`level (feet)`
dfPowellZones$Zone <- dfPowellZones$variable
dfPowellZonesShort <- as.data.frame(dfPowellZones %>% select(Zone, level_feet, stor_maf ) %>% filter (Zone %in% cZonesToShow) %>% arrange(-level_feet))

#Create the y-axis tick label from the level and variable
dfPowellZonesShort$rightlabel <- paste(round(dfPowellZonesShort$stor_maf,1),'-',dfPowellZonesShort$Zone)

dfPowellZonesShort$BeginTemp <- minTemp
dfPowellZonesShort$EndTemp <- maxTemp

dfPowellZonesShortMelt <- melt(dfPowellZonesShort[,c("Zone","level_feet", "BeginTemp","EndTemp")], id = c("Zone","level_feet"))

dfPowellZonesShortMelt <- dfPowellZonesShortMelt %>% arrange(-level_feet,Zone)


library(ggrepel)


### Plot Release temperature vs. lake surface elevation for full daily data set (very messy)

dfPowellReleaseElev <- left_join(dfPowellReleaseTempSum,dfPowellHistorical,by = c("DateClean" = "dDate"))
#Convert to numric
dfPowellReleaseElev$WaterSurface <- as.numeric(dfPowellReleaseElev$Elevation..feet.)
#Calculate Water Year
dfPowellReleaseElev$WaterYear <- ifelse(dfPowellReleaseElev$Month.x >= 10, dfPowellReleaseElev$Year.x,dfPowellReleaseElev$Year.x - 1)

#Classify each year as lake level increasing, decreasing, or steady within a given margin
nMargin <- 5 #feet
nTempMargin <- 1.5 # oC
#Pull the first and last weater level measurements for each year
dfPowellLevelChange <- rbind(dfPowellReleaseElev %>% filter(Month.x == 1,Day==1),
                             dfPowellReleaseElev %>% filter(Month.x == 12,Day==31))
dfPowellTempChange <- dcast(dfPowellLevelChange,Year.y~Month.y , mean, value.var = "avgDay")
dfPowellLevelChange <- dcast(dfPowellLevelChange,Year.y~Month.y , mean, value.var = c("WaterSurface"))#,"avgDay"))
#Classify water level changes for each year
dfPowellLevelChange$LevelYearType <- ifelse(dfPowellLevelChange$Dec - dfPowellLevelChange$Jan > nMargin,"Rise",ifelse(dfPowellLevelChange$Dec - dfPowellLevelChange$Jan < -nMargin,"Fall","Steady"))
dfPowellTempChange$TempYearType <- ifelse(dfPowellTempChange$Dec - dfPowellTempChange$Jan > nTempMargin,"Rise",ifelse(dfPowellTempChange$Dec - dfPowellTempChange$Jan < -nTempMargin,"Fall","Steady"))
#Join the two
dfPowellLevelChange <- left_join(dfPowellLevelChange,dfPowellTempChange,by=c("Year.y" = "Year.y"))
#Join the classification back to main data frame
dfPowellReleaseElev <- left_join(dfPowellReleaseElev,dfPowellLevelChange[,c(1,4,7)], by=c("Year.y" = "Year.y"))
#Reorder the factor levels
dfPowellReleaseElev <- transform(dfPowellReleaseElev, LevelYearType = factor(LevelYearType,c("Fall","Steady","Rise")))
dfPowellReleaseElev <- transform(dfPowellReleaseElev, TempYearType = factor(TempYearType,c("Fall","Steady","Rise")))

dfTempElevationModelCalc$Month.x <- dfTempElevationModelCalc$Month

#Remove highest elevation (top of dam)
dfPowellZonesMinusTop <- dfPowellZonesShort %>% filter(level_feet <= 3700)


# For the temperature profiles, reposition elevation so 0 foot measurement depth is at the penstock intakes
# 10 foot measurement depth is 10 feet above penstock intakes, etc.
# So assume the water surface is always depth feet above the penstock
dfPowellTempLevelsPlot$ElevAvbPenstock <- dfPowellZonesMinusTop[5,2] + dMetersToFeet*dfPowellTempLevelsPlot$Depth
#Add a Month.x field to allow faceting
dfPowellTempLevelsPlot$Month.x <- dfPowellTempLevelsPlot$MonNum

#Water Surface Elevation vs Release Temperature by Month
#with Monthly regression model overlaid

#NEED to add legend for regression fits and points above surface

ggplot(data=dfPowellReleaseElev %>% filter(Day %in% seq(1,31, by=1)) %>% arrange(DateClean)) +
  #geom_line(aes(x=Day,y=avgDay), color="black") +
  #Error bar on release data - color by water surface
  geom_errorbar(aes(y=WaterSurface, xmin= minDay, xmax=maxDay, color = Year.x), size=1) +
  geom_line(data = dfTempElevationModelCalc, aes(x=Temperature, y=Elevation), color = "Black", size=1.25) +
  
  scale_color_continuous(low=palBlues[2],high=palBlues[9], na.value="White", guide = "colorbar", aesthetics="color") +
  
  labs(y="Water Surface Elevation (feet)", x="Turbine Release Temperature (oC)", color="Year") +
  #labs(x="Temperature at Wahweap @ 3,490 ft (oC)", y="Release Temperature (oC)", color="") +
  
  facet_wrap(~Month.x) +
  scale_y_continuous(limits = c(3370,3700), breaks = seq(3250,3711, by=50),labels=seq(3250,3711, by=50),  sec.axis = sec_axis(~. +0, name = "Active Storage\n(million acre-feet)", breaks = dfPowellZonesMinusTop$level_feet, labels = dfPowellZonesMinusTop$rightlabel )) +
  xlim(7,30) +
  
  theme(text = element_text(size=18), legend.text=element_text(size=16),
        legend.key = element_blank())

ggsave("CompareReleaseElevationMonth.png", width=9, height = 6.5, units="in")


#Water Surface Elevation vs Release Temperature by Month
# with Monthly regression model overlaid and release temperature inferred from depth profiles
# if water surface is at specified elevation and release is from penstocks

ggplot(data=dfPowellReleaseElev %>% filter(Day %in% seq(1,31, by=1)) %>% arrange(DateClean)) +
  #geom_line(aes(x=Day,y=avgDay), color="black") +
  #Points represent transformed temperature profile reading. For a specific depth below the water surface,
  #we calculate the elevation that would put the depth at the turbine elevation
  geom_point(data = dfPowellTempLevelsPlot %>% filter(Depth*dMetersToFeet <= 3600 - dfPowellZonesShort[6,2]), aes(y = ElevAvbPenstock, x = T, shape ="Wahweap temperature\nat turbine elev."), color = "Red", size=0.75) +
    #Error bar on release data - color by water surface
  geom_errorbar(aes(y=WaterSurface, xmin= minDay, xmax=maxDay, color = Year.x), size=1) +
  geom_line(data = dfTempElevationModelCalc %>% filter(Elevation > dfPowellZonesShort[6,2] - 10), aes(x=Temperature, y=Elevation, linetype="Spreadsheet model"), color = "Black", size=1.25) +
  
  scale_color_continuous(low=palBlues[2],high=palBlues[9], na.value="White", guide = "colorbar", aesthetics="color") +
  scale_linetype_manual(values = c("solid")) +
  scale_shape_manual(values = c("circle")) +
  
  labs(y="Water Surface Elevation (feet)", x="Turbine Release Temperature (oC)", color="Year of obs.", linetype="", shape="") +
  #labs(x="Temperature at Wahweap @ 3,490 ft (oC)", y="Turbine Release Temperature (oC)", color="") +
  
  facet_wrap(~Month.x) +
  scale_y_continuous(limits = c(3370,3700), breaks = seq(3250,3711, by=50),labels=seq(3250,3711, by=50),  sec.axis = sec_axis(~. +0, name = "Active Storage\n(million acre-feet)", breaks = dfPowellZonesMinusTop$level_feet, labels = dfPowellZonesMinusTop$rightlabel )) +
  xlim(7,30) +
  
  theme(text = element_text(size=18), legend.text=element_text(size=16)) #,
        #legend.key = element_blank())

ggsave("CompareReleaseElevationMonth.png", width=9, height = 6.5, units="in")



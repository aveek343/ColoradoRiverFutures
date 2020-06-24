# LakePowellTempScenarios.r
#
# Creates scenarios of Lake Powell Water Temperature-Depth.
#  - Use the scenarios to see the variation in water temperatures at a specified depth (such as the Turbine intake) for different water surface levels
#  - Also the reverse: scenarios of water levels for different temperatures at depth.
#
# Uses the following data:
# 1. qryProfiles at Primary Stations.csv - USGS sond data of water temperature profiles going back to 1960s (Vernieu 2015, https://pubs.usgs.gov/ds/471/pdf/ds471.pdf)
#
#         Vernieu, W. S. (2015). "Historical Physical and Chemical Data for Water in Lake Powell and from Glen Canyon Dam Releases, Utah-Arizona, 1964 –2013." Data Series 471, Version 3.0. https://pubs.usgs.gov/ds/471/pdf/ds471.pdf.
#
# 2. LAKEPOWELL06-16-2020T16.32.29.csv - USBR daily data of reservoir level/storage/release (https://www.usbr.gov/rsvrWater/HistoricalApp.html)
# 3. PowellLevels.xlsx - Definitions of reservoir zones and storage levels (from CRSS/Rosenberg)
#
# The basis data wrangling strategy is:
# 1. Load csv files
# 2. Join Primary Station and Lake Powell Daily data so we have the water level for each day a reading was taken
# 3. Substract depth from water level to get a level (elevation) for each reading.
#
# David E. Rosenberg
# June 19, 2020
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


if (!require(plyr)) { 
  install.packages("plyr", repo="http://cran.r-project.org")
  library(plyr) 
}

if (!require(ggrepel)) { 
  devtools::install_github("slowkow/ggrepel")
  library(ggrepel) 
}

### 0. Definitions

dMetersToFeet = 3.28
sStation = 'LPCR0024' #Wahweap
sMonth = 'Jun' #Month


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


###This reservoir data comes from CRSS. It was exported to Excel.

# Read pool level data in from Excel
sExcelFile <- 'PowellZones.xlsx'
sStation <- 'LPCR0024'   #Closest to Dam

# Read in the historical Mead data
dfPowellZones <- read_excel(sExcelFile)

### 2. Join the Profile and Historical dataframes on the date

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

### Filter on station and month
dfPowellTempLevelsPlot <- dfPowellTempLevels %>% filter(Station.ID == sStation, Month == sMonth)

paste0("Station: ", sStation, ", Month: ", sMonth)
paste0("Number of observations = ", dfMonthSums[dfPowellTempLevelsPlot$MonNum[1]])

# Assign each starting lake elevation for a profile to a class to plot as the same color
cLakeElevationClasses <- c(3710,3655,3570,3525) 

dfPowellTempLevelsPlot$ElevationClass <- cLakeElevationsClasses[1]
for (lev in cLakeElevationClasses) {
  dfPowellTempLevelsPlot$ElevationClass <- ifelse(dfPowellTempLevelsPlot$Elevation..feet. <= lev,lev,dfPowellTempLevelsPlot$ElevationClass)
  
}

dfPowellTempLevelsPlot$ElevationClass <- as.factor(dfPowellTempLevelsPlot$ElevationClass)

#Plot up the depth readings

#dfPlot <- dfPowellTempLevelsPlot %>% filter(dDate==as.Date("1995-06-06"))
dfPlot <- dfPowellTempLevelsPlot

#Prepare the zone data to add as horizontal lines on the plot
#Grab the min/max temperatures
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

palBlues <- brewer.pal(9, "Blues")

# Plot water temperature vs elevation. Color as continuous
ggplot() +
  #Temperature profiles
  geom_line(data=dfPlot, aes(x = T,y = MeasLevel, color = Elevation..feet., group = dDate), size=1.5) +
  #Powell zones
  geom_line(data=dfPowellZonesShortMelt, aes(x = value, y = level_feet, group = Zone), size=1, color="purple", linetype = "longdash") +
  
  scale_x_continuous(trans= "reverse") + 
  scale_y_continuous(limits = c(3300,3715), breaks = seq(3250,3711, by=50),labels=seq(3250,3711, by=50),  sec.axis = sec_axis(~. +0, name = "Active Storage\n(million acre-feet)", breaks = dfPowellZonesShort$level_feet, labels = dfPowellZonesShort$rightlabel)) +
  
  #Continuous color scale by elevation
  scale_color_continuous(low=palBlues[3],high=palBlues[9], na.value="White", guide = "colorbar", aesthetics="color") +
  #Color breaked by zone
  #scale_color_manual(breaks = c(3710,3650,3670,3525),values=c(palBlues[9],palBlues[7],palBlues[5],palBlues[3])) +
  
 
  theme_bw() +
  #coord_fixed() +
  labs(x="Temperature (oC)", y="Elevation (feet)", color="Start Elevation\n(feet)") +
  theme(text = element_text(size=20), legend.text=element_text(size=16))
  
  
ggsave("PowellTempProfile.png", width=9, height = 6.5, units="in")



# Plot water temperature vs elevation. Color as class
ggplot() +
  #Temperature profiles
  geom_line(data=dfPlot, aes(x = T,y = MeasLevel, color = ElevationClass, group = dDate), size=1.5) +
  #Powell zones
  geom_line(data=dfPowellZonesShortMelt, aes(x = value, y = level_feet, group = Zone), size=1, color="purple", linetype = "longdash") +
  
  #Interpolated values
  #geom_point(data=dfPowellTempLevelsPerDay, aes(x=Level3525Temp,y=Zone)) + 
  
  scale_x_continuous(trans= "reverse") + 
  scale_y_continuous(limits = c(3300,3715), breaks = seq(3250,3711, by=50),labels=seq(3250,3711, by=50),  sec.axis = sec_axis(~. +0, name = "Active Storage\n(million acre-feet)", breaks = dfPowellZonesShort$level_feet, labels = dfPowellZonesShort$rightlabel)) +
  
  #Continuous color scale by elevation
  #scale_color_continuous(low=palBlues[3],high=palBlues[9], na.value="White", guide = "colorbar", aesthetics="color") +
  #Color breaked by zone
  scale_color_manual(breaks = cLakeElevationClasses,values=c(palBlues[9],palBlues[7],palBlues[5],palBlues[3])) +
  
  
  theme_bw() +
  #coord_fixed() +
  labs(x="Temperature (oC)", y="Elevation (feet)", color="Start Elevation\n(feet)") +
  theme(text = element_text(size=20), legend.text=element_text(size=16))




ggsave("PowellTempProfile.png", width=9, height = 6.5, units="in")


ZoneTemps <- function(dfData, cZoneElevations, cGroups) {
  #Returns a data frame of the interpolated min and max temperatures at each ZoneElevation with the
  #Specified grouping cGroups
  
  ZoneTemps <- data.frame(Zone, )
}








 

#Add the label for the 1:1 line
#direct.label(p,"angled.boxes")



##### #3. Plot MEAD Pool Levels/Volumes including Flood storage as a function of upstream storage
# Select the pools
cMeadVarNames <- c("Dead Pool", "SNWA Intake #1", "ISG trigger", "DCP trigger", "Live Capacity")
dfMeadPoolsMonthly <- dfMeadPoolsPlot %>% filter(name %in% cMeadVarNames) %>% arrange(-level)
dfMeadPoolsMonthly$EndMonth <- 12
dfMeadPoolsMonthlyMelt <- melt(dfMeadPoolsMonthly[,c("Reservoir","name","level","stor_maf","month","EndMonth")],id.vars = c("Reservoir","name","level","stor_maf"))
dfMeadPoolsMonthlyMelt$Month <- dfMeadPoolsMonthlyMelt$value
dfMeadPoolsMonthlyMelt <- dfMeadPoolsMonthlyMelt[,-c(5,6)]
dfPlotData <- dfMeadPoolsMonthlyMelt %>% arrange(-level,Month)

# Calculate new flood pools as a function of empty upstream storage
MaxMeadFloodPool <- max(dfReservedFlood$Mead_flood_stor)

cFillContours <- c(1,2,3)
cFillNames <- c("one","two","three","four")

nRowsPools <- nrow(dfReservedFlood)

dfFloodPools <- as.data.frame(dfReservedFlood[,c("month_num")])
dfFloodPools$'0' <- dfReservedFlood$Mead_flood_stor
dfFloodPools$'1' <- 0
dfFloodPools$'2' <- 0
dfFloodPools$'3' <- 0

for (contour in cFillContours) {
 #Credit the empty upstream storage
 dfFloodPools[,contour+2] <- dfReservedFlood$Mead_flood_stor + cFillContours[contour]
 #Maintain a minimum flood pool
  for (iRow in  (1:nrow(dfFloodPools))) {
      dfFloodPools[iRow,contour+2] <- min(dfFloodPools[iRow,contour+2], MaxMeadFloodPool)
  }
}

#Melt the contour data
dfFloodPoolsMelt <- melt(dfFloodPools,id.vars = c("month_num"))

#Adjust the right level ticks
dfMeadPoolsPlot2 <- dfMeadPoolsPlot
dfMeadPoolsPlot2[9,8] <- "1192 - Max. Req. Flood Pool"
dfMeadPoolsPlot2[10,8] <- "1218 - Min. Req. Flood Pool"

#Create the month ticks
cMonths <- 1:12
cMonthsLabels <- month.abb[cMonths]


ggplot() +
  geom_line(data=dfPlotData,aes(x=Month,y=stor_maf, color = name), size=2) +
  scale_color_manual(values = palBlues[10:-1:2]) +
  #geom_area(data=dfPlotData,aes(x=month,y=stor_maf, fill = variable), position='stack') +
  #scale_fill_manual(values = palBlues[7:-1:2]) +
  geom_line(data=dfFloodPoolsMelt,aes(x=month_num, y=value, group = variable), size=1) +
  #Label the flood pool contours of empty upstream storage
  geom_label(data=dfFloodPoolsMelt %>% filter(month_num %in% c(3,11)), aes( x = month_num, y = value, label = variable, fontface="bold"), size=5, angle = 0) + 
  geom_label(aes( x = 7, y = 23, label = "[MAF of empty upstream storage]", fontface="bold"), label.size = 0, size=5, angle = 0) + 
  
  #Label the levels within the equalization tiers
  #geom_text(data=dfReleases, aes( x = MidPowell, y = MidMead, label = Release), angle = 0, size = 6) + 
  
  #Create secondary axes for Lake Levels
  # Can't figure out why dfMeadTiers$Volume does not work!!!! Hard coding ...
  #ylim(0,dfPlotData[1,c("stor_maf")]+1) + 
  #scale_y_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = LevelsAsTicks$stor_maf, labels = round(LevelsAsTicks$level,1))) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25),  sec.axis = sec_axis(~. +0, name = "Elevation (feet)", breaks = dfMeadPoolsPlot2$stor_maf, labels = dfMeadPoolsPlot2$label)) +
  
  #    scale_y_continuous(breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]),labels=c(0,5.98,9.6,12.2,dfMaxStor[2,2]),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]), labels = c(895,1025,1075,1105,1218.8))) +
  #scale_x_discrete(breaks=cMonths, labels= cMonthsLabels) +
  scale_x_continuous(breaks=cMonths, labels= cMonthsLabels) +

  
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Active Storage (MAF)", fill = "Pool") +
  theme(text = element_text(size=20), legend.title=element_blank(), 
        legend.text=element_text(size=18), legend.position = "none",
        axis.text.x = element_text(size=18))

ggsave("MeadPools.png", width=9, height = 6.5, units="in")


##### #4. Plot POWELL Pool Levels including Flood storage as a function of upstream storage
# Prepare data for Lower and Mid equalization tiers
dfPowellTiersAsPools <- dfPowellTiers[,c("Tier","PowellLowerVol")]
colnames(dfPowellTiersAsPools) <- c("variable","PowellLowerVol")
dfPowellTiersAsPools$stor_maf <- 0;
dfPowellTiersAsPools <- mutate(dfPowellTiersAsPools, stor_maf = lead(PowellLowerVol))
dfPowellTiersAsPools$Reservoir <- 'Powell'
dfPowellTiersAsPools$value <- dfPowellTiersAsPools$stor_maf*1000000
dfPowellTiersAsPools <- dfPowellTiersAsPools[1:2,]
dfPowellTiersAsPools$PowellLowerVol <- NULL

#Work on the equalization levels
dfPowellEqLevelsAsPools <- dfPowellEqLevelsFilt
colnames(dfPowellEqLevelsAsPools) <- c("Year","variable","stor_maf","MeadBeg","MeadEnd")
dfPowellEqLevelsAsPools$value <- 1000000*dfPowellEqLevelsAsPools$stor_maf
dfPowellEqLevelsAsPools$Reservoir <- "Powell"
#drop the three uneeded columns
dfPowellEqLevelsAsPools <- subset(dfPowellEqLevelsAsPools,select = -c(Year,MeadBeg,MeadEnd))

dfPowellTiersAsPools <- rbind(dfPowellTiersAsPools,dfPowellEqLevelsAsPools)

dfPowellTiersAsPools$level <- interp1(xi = dfPowellTiersAsPools$value,x=dfPowellElevStor$`Live Storage (ac-ft)`,y=dfPowellElevStor$`Elevation (ft)`, method="linear")
dfPowellTiersAsPools$month <- 1

dfPowellAllPools <- rbind(dfPowellVals,dfPowellTiersAsPools) %>% filter(month == 1)

cPowellRows <- c(2,4,7,9,12,13,14,15,16)
cPowellVarNames <- dfPowellAllPools$variable[cPowellRows]
dfPowellPoolsPlot <- dfPowellAllPools %>% filter(variable %in% cPowellVarNames) %>% arrange(level)
dfPowellPoolsPlot$name <- as.character(dfPowellPoolsPlot$variable)
#Save the data to csv
write.csv(dfPowellAllPools,"dfPowellAllPools.csv")

#Rename a few of the variable labels
dfPowellPoolsPlot[1,c("name")] <- "Dead Pool"
dfPowellPoolsPlot[2,c("name")] <- "Minimum Power (CRSS)"
dfPowellPoolsPlot[8,c("name")] <- "Flood Pool (Nov 1 to Jan 31)"
cPowellVarNames <- dfPowellPoolsPlot$name
#Create the y-axis tick label from the level and variable
dfPowellPoolsPlot$label <- paste(round(dfPowellPoolsPlot$level,0),'-',dfPowellPoolsPlot$name)


dfPowellPoolsMonthly <- dfPowellPoolsPlot %>% filter(name %in% cPowellVarNames) %>% arrange(-level)
dfPowellPoolsMonthly$EndMonth <- 12
dfPowellPoolsMonthlyMelt <- melt(dfPowellPoolsMonthly[,c("Reservoir","name","level","stor_maf","month","EndMonth")],id.vars = c("Reservoir","name","level","stor_maf"))
dfPowellPoolsMonthlyMelt$Month <- dfPowellPoolsMonthlyMelt$value
dfPowellPoolsMonthlyMelt <- dfPowellPoolsMonthlyMelt[,-c(5,6)]
dfPlotData <- dfPowellPoolsMonthlyMelt %>% arrange(-level,Month)
#Remove the Flood pool entry
dfPlotData2 <- dfPlotData %>% filter(name != dfPowellPoolsPlot[8,c("name")]) %>% arrange(-level)


ggplot() +
  geom_line(data=dfPlotData2,aes(x=Month,y=stor_maf, color = name), size=2) +
  scale_color_manual(values = palBlues[9:-1:2], breaks=cPowellVarNames) +
  #geom_area(data=dfPlotData,aes(x=month,y=stor_maf, fill = variable), position='stack') +
  #scale_fill_manual(values = palBlues[7:-1:2]) +
  geom_line(data=dfReservedFlood,aes(x=month_num, y=Powell_flood_stor, group = 1), size=2) +
 
  #Label the levels within the equalization tiers
  #geom_text(data=dfReleases, aes( x = MidPowell, y = MidMead, label = Release), angle = 0, size = 6) + 
  
  #Create secondary axes for Lake Levels
  # Can't figure out why dfMeadTiers$Volume does not work!!!! Hard coding ...
  #ylim(0,dfPlotData[1,c("stor_maf")]+1) + 
  #scale_y_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = LevelsAsTicks$stor_maf, labels = round(LevelsAsTicks$level,1))) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25),  sec.axis = sec_axis(~. +0, name = "Elevation (feet)", breaks = dfPowellPoolsPlot$stor_maf, labels = dfPowellPoolsPlot$label)) +
  
  #    scale_y_continuous(breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]),labels=c(0,5.98,9.6,12.2,dfMaxStor[2,2]),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]), labels = c(895,1025,1075,1105,1218.8))) +
  #scale_x_discrete(breaks=cMonths, labels= cMonthsLabels) +
  scale_x_continuous(breaks=cMonths, labels= cMonthsLabels) +
  
  
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Active Storage (MAF)", fill = "Pool") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18), legend.position = "none")

ggsave("PowellPools.png", width=9, height = 6.5, units="in")


### DER January 31, 2020 - Plot Reservoir Storage over time
### Mead, Powell, All onn same plot including total system storage

##### #5. Plot historical MEAD Pool Levels/Volumes over time
# Select the pools

dStartDate <- min(dfMeadHist$BeginOfMon)
dEndDate <- max(dfMeadHist$BeginOfMon)

cMeadVarNames <- c("Dead Pool", "SNWA Intake #1", "ISG trigger", "DCP trigger", "Live Capacity")
dfMeadPoolsMonthly <- dfMeadPoolsPlot %>% filter(name %in% cMeadVarNames) %>% arrange(-level)
dfMeadPoolsMonthly$month <- dStartDate
dfMeadPoolsMonthly$EndMonth <- dEndDate
dfMeadPoolsMonthlyMelt <- melt(dfMeadPoolsMonthly[,c("Reservoir","name","level","stor_maf","month","EndMonth")],id.vars = c("Reservoir","name","level","stor_maf"))
dfMeadPoolsMonthlyMelt$Month <- dfMeadPoolsMonthlyMelt$value
dfMeadPoolsMonthlyMelt <- dfMeadPoolsMonthlyMelt[,-c(5,6)]
dfPlotData <- dfMeadPoolsMonthlyMelt %>% arrange(-level,Month)

# Calculate new flood pools as a function of empty upstream storage
MaxMeadFloodPool <- max(dfReservedFlood$Mead_flood_stor)

cFillContours <- c(1,2,3)
cFillNames <- c("one","two","three","four")

nRowsPools <- nrow(dfReservedFlood)

dfFloodPools <- as.data.frame(dfReservedFlood[,c("month_num")])
dfFloodPools$'0' <- dfReservedFlood$Mead_flood_stor
dfFloodPools$'1' <- 0
dfFloodPools$'2' <- 0
dfFloodPools$'3' <- 0

for (contour in cFillContours) {
  #Credit the empty upstream storage
  dfFloodPools[,contour+2] <- dfReservedFlood$Mead_flood_stor + cFillContours[contour]
  #Maintain a minimum flood pool
  for (iRow in  (1:nrow(dfFloodPools))) {
    dfFloodPools[iRow,contour+2] <- min(dfFloodPools[iRow,contour+2], MaxMeadFloodPool)
  }
}

#Melt the contour data
dfFloodPoolsMelt <- melt(dfFloodPools,id.vars = c("month_num"))

#Adjust the right level ticks
dfMeadPoolsPlot2 <- dfMeadPoolsPlot
dfMeadPoolsPlot2[9,8] <- "1192 - Max. Req. Flood Pool"
dfMeadPoolsPlot2[10,8] <- "1218 - Min. Req. Flood Pool"

#Create the month ticks
cMonths <- 1:12
cMonthsLabels <- month.abb[cMonths]


ggplot() +
  
  #Zone levels
  geom_line(data=dfPlotData,aes(x=Month,y=stor_maf, color = name), size=1.25) +
  #Mead storage
  geom_line(data=dfMeadHist,aes(x=BeginOfMon,y=Stor/1e6), size=2) +
  
  scale_color_manual(values = palBlues[10:-1:2]) +
  #geom_area(data=dfPlotData,aes(x=month,y=stor_maf, fill = variable), position='stack') +
  #scale_fill_manual(values = palBlues[7:-1:2]) +
  #Flood pools as a function of upstream storage
  #geom_line(data=dfFloodPoolsMelt,aes(x=month_num, y=value, group = variable), size=1) +
  #Label the flood pool contours of empty upstream storage
  #geom_label(data=dfFloodPoolsMelt %>% filter(month_num %in% c(3,11)), aes( x = month_num, y = value, label = variable, fontface="bold"), size=5, angle = 0) + 
  #geom_label(aes( x = 7, y = 23, label = "[MAF of empty upstream storage]", fontface="bold"), label.size = 0, size=5, angle = 0) + 
  
  scale_y_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25),  sec.axis = sec_axis(~. +0, name = "Elevation (feet)", breaks = dfMeadPoolsPlot2$stor_maf, labels = dfMeadPoolsPlot2$label)) +
  
  #    scale_y_continuous(breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]),labels=c(0,5.98,9.6,12.2,dfMaxStor[2,2]),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]), labels = c(895,1025,1075,1105,1218.8))) +
  #scale_x_discrete(breaks=cMonths, labels= cMonthsLabels) +
  #scale_x_continuous(breaks=seq(1960,2020,by=10)) +
  
  
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Mead Active Storage (MAF)", fill = "Pool") +
  theme(text = element_text(size=20), legend.title=element_blank(), 
        legend.text=element_text(size=18), legend.position = "none",
        axis.text.x = element_text(size=18))

ggsave("MeadTimeSeries.png", width=9, height = 6.5, units="in")


##### #6. Plot POWELL storage over time
# Prepare data for Lower and Mid equalization tiers


dfPowellTiersAsPools <- dfPowellTiers[,c("Tier","PowellLowerVol")]
colnames(dfPowellTiersAsPools) <- c("variable","PowellLowerVol")
dfPowellTiersAsPools$stor_maf <- 0;
dfPowellTiersAsPools <- mutate(dfPowellTiersAsPools, stor_maf = lead(PowellLowerVol))
dfPowellTiersAsPools$Reservoir <- 'Powell'
dfPowellTiersAsPools$value <- dfPowellTiersAsPools$stor_maf*1000000
dfPowellTiersAsPools <- dfPowellTiersAsPools[1:2,]
dfPowellTiersAsPools$PowellLowerVol <- NULL

#Work on the equalization levels
dfPowellEqLevelsAsPools <- dfPowellEqLevelsFilt
colnames(dfPowellEqLevelsAsPools) <- c("Year","variable","stor_maf","MeadBeg","MeadEnd")
dfPowellEqLevelsAsPools$value <- 1000000*dfPowellEqLevelsAsPools$stor_maf
dfPowellEqLevelsAsPools$Reservoir <- "Powell"
#drop the three uneeded columns
dfPowellEqLevelsAsPools <- subset(dfPowellEqLevelsAsPools,select = -c(Year,MeadBeg,MeadEnd))

dfPowellTiersAsPools <- rbind(dfPowellTiersAsPools,dfPowellEqLevelsAsPools)

dfPowellTiersAsPools$level <- interp1(xi = dfPowellTiersAsPools$value,x=dfPowellElevStor$`Live Storage (ac-ft)`,y=dfPowellElevStor$`Elevation (ft)`, method="linear")
dfPowellTiersAsPools$month <- 1

dfPowellAllPools <- rbind(dfPowellVals,dfPowellTiersAsPools) %>% filter(month == 1)

cPowellRows <- c(2,4,7,9,12,13,14,15,16)
cPowellVarNames <- dfPowellAllPools$variable[cPowellRows]
dfPowellPoolsPlot <- dfPowellAllPools %>% filter(variable %in% cPowellVarNames) %>% arrange(level)
dfPowellPoolsPlot$name <- as.character(dfPowellPoolsPlot$variable)
#Rename a few of the variable labels
dfPowellPoolsPlot[1,c("name")] <- "Dead Pool"
dfPowellPoolsPlot[2,c("name")] <- "Minimum Power (CRSS)"
dfPowellPoolsPlot[8,c("name")] <- "Flood Pool (Nov 1 to Jan 31)"
cPowellVarNames <- dfPowellPoolsPlot$name
#Create the y-axis tick label from the level and variable
dfPowellPoolsPlot$label <- paste(round(dfPowellPoolsPlot$level,0),'-',dfPowellPoolsPlot$name)


dfPowellPoolsMonthly <- dfPowellPoolsPlot %>% filter(name %in% cPowellVarNames) %>% arrange(-level)
dfPowellPoolsMonthly$EndMonth <- 12
dfPowellPoolsMonthlyMelt <- melt(dfPowellPoolsMonthly[,c("Reservoir","name","level","stor_maf","month","EndMonth")],id.vars = c("Reservoir","name","level","stor_maf"))
dfPowellPoolsMonthlyMelt$Month <- dfPowellPoolsMonthlyMelt$value
dfPowellPoolsMonthlyMelt <- dfPowellPoolsMonthlyMelt[,-c(5,6)]
dfPlotData2 <- dfPowellPoolsMonthlyMelt %>% arrange(-level,Month)
#Remove the Flood pool entry
dfPlotData2 <- dfPlotData2 %>% filter(name != dfPowellPoolsPlot[8,c("name")]) %>% arrange(-level)
#Add Full Dates
dStartDate <- min(dfJointStorage$DateAsValue)
dfPlotData2$Dates <- rep(c(dStartDate,dEndDate),times=nrow(dfPlotData2)/2)

ggplot() +
  #Powell Zones
  geom_line(data=dfPlotData2,aes(x=Dates,y=stor_maf, color = name), size=1.25) +
  #Powell storage
  geom_line(data=dfJointStorage,aes(x=DateAsValue,y=PowellStorage), size=2) +
  scale_color_manual(values = palBlues[9:-1:2], breaks=cPowellVarNames) +
  #geom_area(data=dfPlotData,aes(x=month,y=stor_maf, fill = variable), position='stack') +
  scale_y_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25),  sec.axis = sec_axis(~. +0, name = "Elevation (feet)", breaks = dfPowellPoolsPlot$stor_maf, labels = dfPowellPoolsPlot$label)) +
  
  #    scale_y_continuous(breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]),labels=c(0,5.98,9.6,12.2,dfMaxStor[2,2]),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]), labels = c(895,1025,1075,1105,1218.8))) +
  #scale_x_discrete(breaks=cMonths, labels= cMonthsLabels) +
  #scale_x_continuous(breaks=seq(1960,2020,by=10), labels= seq(1960,2020,by=10)) +
  
  
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Powell Active Storage (MAF)", fill = "Pool") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18), legend.position = "none")

ggsave("PowellStorageTime.png", width=9, height = 6.5, units="in")

#### POWELL AND MEAD STORAGE OVER TIME

ggplot() +
  #Powell storage
  geom_line(data=dfJointStorage,aes(x=DateAsValue,y=PowellStorage, color="Powell"), size=2) +
  #Mead Storage
  geom_line(data=dfJointStorage,aes(x=DateAsValue,y=MeadStorage, color="Mead"), size=2) +
  #Combined Storage
  geom_line(data=dfJointStorage,aes(x=DateAsValue,y=MeadStorage+PowellStorage, color="Combined"), size=2) +
  scale_color_manual(values = c("purple","red","blue"), breaks=c("Combined", "Powell", "Mead")) +
  #geom_area(data=dfPlotData,aes(x=month,y=stor_maf, fill = variable), position='stack') +
  scale_y_continuous(breaks = seq(0,50,by=10),labels=seq(0,50,by=10)) +
  
  #    scale_y_continuous(breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]),labels=c(0,5.98,9.6,12.2,dfMaxStor[2,2]),  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = c(0,5.98,9.6,12.2,dfMaxStor[2,2]), labels = c(895,1025,1075,1105,1218.8))) +
  #scale_x_discrete(breaks=cMonths, labels= cMonthsLabels) +
  #scale_x_continuous(breaks=seq(1960,2020,by=10), labels= seq(1960,2020,by=10)) +
  
  
  #scale_fill_manual(breaks=c(1:6),values = palBlues[2:7]) + #,labels = variable) + 
  theme_bw() +
  #coord_fixed() +
  labs(x="", y="Active Storage (MAF)", color = "Reservoir") +
  theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18))
  #theme(text = element_text(size=20), legend.text=element_text(size=16)




# TimeToDeadPool.r
#
# Calculate the time to reach the reservoir dead pool based (or some other criteria) from an initial storage and steady inflow. Plot
# as Time To Dead Pool (y axis) vs Reservoir storage (x-axis). Show for different release policies 
# (release as a function of storage and inflow) and reservoirs.

# Examples for a simple test case, Lake Mead, and Lake Powell.
#
# The overall governing equation is:
#   Storage_t+1 = Storage_t + Inflow - Release - Evaporation loss_t.
# We simply count the numer of iterations until Storage_ t+1 goes to zero or becomes really large.
#
# This is a deterministic version of analysis by Barnett, T. P., and Pierce, D. W. (2008). "When will Lake Mead go dry?" Water Resources Research, 44(3). https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2007WR006704.
# The deterministic analysis makes it easier to identify new release policies that are functions of storage AND inflow
# to balance supply and demand over the long term.
#
# Data is drawn from CRSS, analysis of DCP, and other sources as docummented in source Excel files (see below)
# Please report bugs/feedback to:
#
# David E. Rosenberg
# April 15, 2019
# Updated August 2, 2019
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

if (!require(plyr)) { 
  install.packages("plyr", repo="http://cran.r-project.org")
  library(plyr) 
}

# New functoin MeadInflowToLeeFerrylNatural that takes a Mead Inflow value and estimates a Lee Ferry Natural Flow value
# Simplistic relationship between natural flow at Lee Ferry and Mead Flow. I think this is something like:
  
#  [Lee Ferry Natural Flow]  =  [Mead Inflow]  - [0.3 to 0.8 MAF Grand Canyon Tributary inflow] + [0.6 MAF Powell Evaporation] + [4.5 MAF upper basin consumptive use] (numbers all very rough).

MeadInflowToLeeFerryNatural <- function(MeadInflow,GrandCanyonTrib,PowellEvap,UpperBasinConsumUse) {
  
  LeeFerryNatural <- MeadInflow - GrandCanyonTrib + PowellEvap + UpperBasinConsumUse;
  return(LeeFerryNatural)  ;
}



# New function interp2 to return NAs for values outside interpolation range (from https://stackoverflow.com/questions/47295879/using-interp1-in-r)
interp2 <- function(x, y, xi = x, ...) {
  yi <- rep(NA, length(xi));
  sel <- which(xi >= range(x)[1] & xi <= range(x)[2]);
  yi[sel] <- interp1(x = x, y = y, xi = xi[sel], ...);
  return(yi);
}

# New function which calculates the number of time periods to reach the reservoir's terminal state (low storage such as dead pool, high storage such top of dam)
# User provides
#   Sinit: an initial storage volume
#   inflow: steady constant inflow in each and every time step
#   delivery schedule (release as a function of...) defined by deliveryVolume and deliveryResStorage
#   sMethodRelease: the interpolation method used by interp1 for reservoir releases
#   eRate: evaporation rate in depth per year
#   reservoir bathymetry of ResArea and ResVolume
#   SminTarget: low storage target (when reached, simulation stops)
#   SmaxTarget: upper storage target (when reached, simulation stops)
#   MaxIts: maximum number of iterations before stopping
#   startYear: first year of simulation to help organize time series results
# Accounting is done using the storage balance equation sCurr_t+1 = Scurr_t + inflow - release - evaporation(sCurr_t) is done on an annual basis. 

# OUTPUTS
#   dfTimeResults - data frame of time series results including inflow and storage
#   periods - Number of periods to reach terminal state
#   finalstate - takes the value of either "Upper", "Lower", "Middle" to indicate where the final reservoir storage state is

#   
# For sMethodRelease options, see method in https://www.rdocumentation.org/packages/pracma/versions/1.9.9/topics/interp1
TimeToReservoirTarget <- function(Sinit, inflow, deliveryVolume, deliveryResStorage, sMethodRelease, eRate, 
                                  ResArea, ResVolume, sMinTarget, sMaxTarget, MaxIts, startYear) {
  #Start with zero years
  currT <- 1
  Scurr <- Sinit #Set current storage volume
  #Create empty data frame of results
  cReleases <- rep(NA,MaxIts)
  dfTimeResults <- data.frame(matrix(NA,nrow=MaxIts,ncol=5))
  names(dfTimeResults) <- c("Inflow","Year","index","Storage","Release")
  
  Smax <- min(max(ResVolume),max(deliveryResStorage),sMaxTarget) # Calculate maximum volume at which the simulation will stop. from the Bathymetry and Delivery curves and user provided SmaxTarget
  Smin <- max(min(ResVolume),min(deliveryResStorage),sMinTarget) # Calculate minimum volume at which the simulation will stop. from the Bathymetry and Delivery curves and user provided SminTarget
  
  
   while ((Scurr > Smin) && (currT <= MaxIts) && (Scurr <= Smax)){  #keep looping until storage drops to minimum threshold, storage increases to maximum threshold, or we hit the maximum number of interations
    #Record the current storage
    dfTimeResults$Storage[currT] <- Scurr
     
    #Calculate mass balance components in current time step at Scurr
    release <- interp2(x=deliveryResStorage, y=deliveryVolume,xi = Scurr, method = sMethodRelease) # release is step function defined in the data
    evap <- eRate*interp2(x=ResVolume, y=ResArea,xi = Scurr) # Evaporation is a linear interpolation off the reservoir bathymetry curve
    #Reservoir storage balance equation. New storage = Current Storage + Inflow - release - evaporation
    Scurr <- Scurr + inflow - release - evap
  
    cReleases[currT] <- release #Log the current release
    
    currT <- currT + 1 # Advance the time step
    
   }
  
  if (currT < MaxIts) {
    #Log the "next" storage
    #dfTimeResults$Storage[currT] <- Scurr
  }
  
  #Determine the ending storage state
  if (Scurr >= Smax) {
    sStatus <- "Top"
  } else if
   (Scurr <= Smin) {
    sStatus <- "Bottom"
  } else { sStatus <- "Middle"
  }
  
  #Further post-processing of results to turn into a time-series data from
  #Convert list to column
  dfTimeResults$Inflow <- rep(inflow, nrow(dfTimeResults))
  #Add calendar years
  dfTimeResults$Year <- seq(startYear,startYear+nrow(dfTimeResults)-1)
  #Add year index
  dfTimeResults$index <- seq(1,1+nrow(dfTimeResults)-1)
  #Convert storage as list to number
  dfTimeResults$Storage <- as.numeric(dfTimeResults$Storage)
  #Log the releases
  dfTimeResults$Release <- as.numeric(cReleases)
    
  
  ReturnList <- list("volume" = Scurr, "periods" = currT - 1, "status" = sStatus, "dfTimeResults" = dfTimeResults)
  
  return(ReturnList)
  #return(currT)
}


####  Small example to test the TimeToDeadPool function ######
#                                                            #
##############################################################
tStartVol <- 10
tMaxVol <- 15
tInflow <- 2
dfDeliverySchedule <- data.frame(release = c(2,2,5,5), stor = c(0,2,11,tMaxVol))
dfBath <- data.frame(volume = c(0,3,10,tMaxVol), area = c(1,3,5,6))
tErate <- 0.5

interp2(x=dfDeliverySchedule$stor, y=dfDeliverySchedule$release, xi = tStartVol, method = "constant") 
interp2(x=dfBath$volume, y=dfBath$area,xi = tStartVol)

#debug(TimeToReservoirTarget)
lTestReturn <- TimeToReservoirTarget(Sinit = tStartVol, inflow = tInflow, deliveryVolume = dfDeliverySchedule$release, 
                  deliveryResStorage = dfDeliverySchedule$stor, eRate = tErate, ResArea = dfBath$area, 
                  ResVolume = dfBath$volume, MaxIts = 50, sMethodRelease = "constant", sMinTarget = 3, sMaxTarget = 15, startYear = 2000)

#############################################################
#      Load Data for LAKE MEAD and LAKE POWELL              #
#############################################################

# Lower Basin Delivery Target for CA, AZ, NV, MX, and losses (maf per year)
vLowerBasinDeliveryTarget <- 9.6e6

###This reservoir data comes from CRSS. It was exported to Excel.

# Read elevation-storage data in from Excel
sExcelFile <- 'MeadDroughtContingencyPlan.xlsx'
dfMeadElevStor <- read_excel(sExcelFile, sheet = "Mead-Elevation-Area",  range = "A4:D676")
dfPowellElevStor <- read_excel(sExcelFile, sheet = 'Powell-Elevation-Area',  range = "A4:D689")

#Evaporation rates from CRSS
#EvapRates <- read_excel(sExcelFile, sheet = 'Data',  range = "P3:S15")
# Evaporation Rates from Schmidt et al (2016) Fill Mead First, p. 29, Table 2 - https://qcnr.usu.edu/wats/colorado_river_studies/files/documents/Fill_Mead_First_Analysis.pdf
dfEvapRates <- data.frame(Reservoir = c("Mead","Mead","Powell"),"Rate ft per year" = c(5.98,6.0, 5.73), Source = c("CRSS","FEIS-2008","Reclamation"), MinRate = c(NA,5.5,4.9), MaxRate = c(NA,6.4, 6.5))

# Define maximum storages
dfMaxStor <- data.frame(Reservoir = c("Powell","Mead"),Volume = c(24.32,25.95))


# Read in Reservoir Pools Volumes / Zones from Excel
dfPoolVols <- read_excel(sExcelFile, sheet = "Pools",  range = "D31:O43")
# Read in Reserved Flood Storage
dfReservedFlood <- read_excel(sExcelFile, sheet = "Pools",  range = "C46:E58")
#Convert dates to months
dfReservedFlood$month_num <- month(as.POSIXlt(dfReservedFlood$Month, format="%Y-%m-%Y"))

# Read in Paria, Little Colorado, and Virgin River Flows from CRSS DMI to convert Inflow to Mead to Natural Flow at Lee Ferry
sExcelFileGrandCanyonFlow <- 'HistoricalNaturalFlow.xlsx'
dfGCFlows <- read_excel(sExcelFileGrandCanyonFlow, sheet = 'Total Natural Flow',  range = "V1:Y1324")
dfGCDates <- read_excel(sExcelFileGrandCanyonFlow, sheet = 'Total Natural Flow',  range = "A1:A1324")

#Merge and combine into one Data frame
dfGCFlows$Date <- dfGCDates$`Natural Flow And Salt Calc model Object.Slot`
dfGCFlows$Total <- dfGCFlows$`CoRivPowellToVirgin:PariaGains.LocalInflow` + dfGCFlows$`CoRivPowellToVirgin:LittleCoR.LocalInflow` + 
                          dfGCFlows$VirginRiver.Inflow

#Convert to Water Year and sum by water year
dfGCFlows$Year <- year(dfGCFlows$Date)
dfGCFlows$Month <- month(as.Date(dfGCFlows$Date,"%Y-%m-%d"))
dfGCFlows$WaterYear <- ifelse(dfGCFlows$Month >= 10,dfGCFlows$Year,dfGCFlows$Year - 1)
dfGCFlowsByYear <- aggregate(dfGCFlows$Total, by=list(Category=dfGCFlows$WaterYear), FUN=sum)

#Plot as a box-and whiskers
ggplot(dfGCFlowsByYear, aes(y=x)) +
     geom_boxplot()

#Calculate the median value
vMedGCFlow <- median(dfGCFlowsByYear$x)

# Read in the ISG and DCP cutbacks from Excel
dfCutbacksElev <- read_excel(sExcelFile, sheet = "Data",  range = "H21:H33") #Elevations
dfCutbacksVols <- read_excel(sExcelFile, sheet = "Data",  range = "O21:U33") #ISG and DCP for states + MX
dfCutbacksVolsFed <- read_excel(sExcelFile, sheet = "Data",  range = "Y21:Y33") # Federal cutback
#Merge into one data frame
dfCutbacks <- dfCutbacksElev
dfCutbacks$RowNum <- 0
dfCutbacksVols$RowNum <- 0
dfCutbacksVolsFed$RowNum <- 0
for (CurrRow in 1:nrow(dfCutbacks)) {
  dfCutbacks[CurrRow,"RowNum"] <- CurrRow
  dfCutbacksVols[CurrRow,"RowNum"] <- CurrRow
  dfCutbacksVolsFed[CurrRow,"RowNum"] <- CurrRow
}

dfCutbacks <- full_join(dfCutbacks,dfCutbacksVols)
dfCutbacks <- full_join(dfCutbacks,dfCutbacksVolsFed)

# Convert NAs to Zeros
dfCutbacks <- replace(dfCutbacks,is.na(dfCutbacks),0)

# Calculate Mead Volume from Elevation (interpolate from storage-elevation curve)
dfCutbacks$MeadActiveVolume <- interp1(xi = dfCutbacks$`Mead Elevation (ft)`,x=dfMeadElevStor$`Elevation (ft)` , y=dfMeadElevStor$`Live Storage (ac-ft)`, method="linear")

#Calculate Total Reductions for ISG (use Federal and Mexico dating to 2012 )
dfCutbacks <- dfCutbacks %>% mutate(Total2007ISG = `Mexico Reduction (Minute 323) [2017]`+ 
                                      `2007-AZ Reduction (ac-ft)` + `2007-NV Reduction (ac-ft)` + `2007-CA Reduction (ac-ft)` +
                                     `DCP Federal Government (ac-ft)`)
#Remove federal amount at 1090 ft since IGS only starts at 1075 ft
dfCutbacks$Total2007ISG[dfCutbacks$`Mead Elevation (ft)` == 1090] <- 0

#Calculate Total Reudctions for DCP
dfCutbacks <- dfCutbacks %>% mutate(TotalDCP = `Mexico Reduction (Minute 323) [2017]`+ 
                                      `DCP-AZ Reduction (ac-ft)` + `DCP-NV Reduction (ac-ft)` + `DCP-CA Reduction (ac-ft)` +
                                      `DCP Federal Government (ac-ft)`)

#Calculate Delivers as Target - Reduction
dfCutbacks$DeliveryDCP <- vLowerBasinDeliveryTarget - dfCutbacks$TotalDCP
dfCutbacks$DeliveryISG <- vLowerBasinDeliveryTarget - dfCutbacks$Total2007ISG
dfCutbacks$DeliveryNorm <- vLowerBasinDeliveryTarget 

# Identify important Mead Levels to put as context on x-axis above the plot 
#Calculate Levels from volumes (interpolate from storage-elevation curve)
#Mead
dfMeadVals <- melt(subset(dfPoolVols,Reservoir == "Mead"),id.vars = c("Reservoir"))
dfMeadVals$level <- interp1(xi = dfMeadVals$value,x=dfMeadElevStor$`Live Storage (ac-ft)`,y=dfMeadElevStor$`Elevation (ft)`, method="linear")

#Powell
dfPowellVals <- melt(subset(dfPoolVols,Reservoir == "Powell"),id.vars = c("Reservoir"))
dfPowellVals$level <- interp1(xi = dfPowellVals$value,x=dfPowellElevStor$`Live Storage (ac-ft)`,y=dfPowellElevStor$`Elevation (ft)`, method="linear")

dfPowellVals <- melt(dfPowellVals,id.vars = c("Reservoir","variable","value","level"))
dfMeadVals <- melt(dfMeadVals,id.vars = c("Reservoir","variable","value","level"))

# Convert to MAF storage
dfMeadVals$stor_maf <- dfMeadVals$value / 1000000
dfPowellVals$stor_maf <- dfPowellVals$value / 1000000

#Calculate the volume of flood storage space reserved
dfReservedFlood$Mead_flood_stor <- dfMeadVals[2,c("stor_maf")] - dfReservedFlood$Mead
dfReservedFlood$Powell_flood_stor <- dfPowellVals[2,c("stor_maf")] - dfReservedFlood$Powell
#Calculate levels for the reserved flood volumes
dfReservedFlood$Mead_level <- interp1(xi = dfReservedFlood$Mead_flood_stor*1000000,x=dfMeadElevStor$`Live Storage (ac-ft)`,y=dfMeadElevStor$`Elevation (ft)`, method="linear")
dfReservedFlood$Powell_level <- interp1(xi = dfReservedFlood$Powell_flood_stor*1000000,x=dfPowellElevStor$`Live Storage (ac-ft)`,y=dfPowellElevStor$`Elevation (ft)`, method="linear")

# Include additional levels not in the CRSS pool data
#Specify Powell Equalization levels by Year (data values from Interim Guidelines)
dfPowellEqLevels <- data.frame(Year = c(2008:2026), Elevation = c(3636,3639,3642,3643,3645,3646,3648,3649,3651,3652,3654,3655,3657,3659,3660,3663,3663,3664,3666))
dfPowellEqLevels$Volume <- vlookup(dfPowellEqLevels$Elevation,dfPowellElevStor,result_column=2,lookup_column = 1)/1000000
#Need to convert these Powell volumes into equivalent Mead levels for the next step
dfPowellEqLevels$EqMeadLev <- interp2(xi = dfPowellEqLevels$Volume*1000000,x=dfMeadElevStor$`Live Storage (ac-ft)`,y=dfMeadElevStor$`Elevation (ft)`, method="linear")


dfMeadValsAdd <- data.frame(Reservoir = "Mead",
                            variable = c("Mead Flood Pool","Powell Eq. Level (2019)","DCP trigger","ISG trigger","SNWA Intake #1","Mead Eq. Tier","SNWA Intake #2","Mead Power","SNWA Intake #3"),
                            level = c(max(dfReservedFlood$Mead_level),dfPowellEqLevels$EqMeadLev[12],1090,1075,1050,1025,1000,955,860))
nRowMead <- nrow(dfMeadValsAdd)
dfMeadValsAdd$value <- 0
#Interpolate live storage volume
dfMeadValsAdd$value[1:(nRowMead-1)] <- interp1(xi = dfMeadValsAdd$level[1:(nRowMead-1)],x=dfMeadElevStor$`Elevation (ft)`,y=dfMeadElevStor$`Live Storage (ac-ft)`, method="linear")
#Add SNWA third straw which is below dead pool
dfMeadValsAdd$value[nRowMead] <- -dfMeadVals[10,3]
dfMeadValsAdd$stor_maf <- dfMeadValsAdd$value / 1000000

#Combine the original mead levels from CRSS with the levels added above
dfMeadAllPools <- rbind(dfMeadVals,dfMeadValsAdd)
#dfMeadAllPools <- dfMeadAllPools[order(dfMeadAllPools$month, dfMeadAllPools$level),]

#Pull out the desired rows
#dfMeadPoolsPlot <- dfMeadAllPools[c(3,6,7,9:13,16),]
cMeadVarNames <- c("Inactive Capacity", "Mead Power", "SNWA Intake #2", "Mead Eq. Tier", "SNWA Intake #1", "DCP trigger", "Powell Eq. Level (2019)",
                  "Mead Flood Pool", "Live Capacity")
dfMeadPoolsPlot <- dfMeadAllPools %>% filter(variable %in% cMeadVarNames) %>% arrange(level)
dfMeadPoolsPlot$name <- as.character(dfMeadPoolsPlot$variable)
#Rename a few of the variable labels
dfMeadPoolsPlot[1,c("name")] <- "Dead Pool"
#dfMeadPoolsPlot[6,c("name")] <- "Flood Pool (1-Aug)"
#Create the y-axis tick label from the level and variable
#dfMeadPoolsPlot$label <- paste(round(dfMeadPoolsPlot$level,0),'\n',dfMeadPoolsPlot$name)
#Use label/labelComb when it's a secondary x axis
dfMeadPoolsPlot$label <- paste(str_replace_all(dfMeadPoolsPlot$name," ","\n"),'\n', round(dfMeadPoolsPlot$level,0))
dfMeadPoolsPlot$labelComb <- str_replace_all(dfMeadPoolsPlot$name," ","\n")
dfMeadPoolsPlot$labelComb[1] <- paste0(dfMeadPoolsPlot$labelComb[1],"s")
##Use labelSecY when it's a secondary y axis
dfMeadPoolsPlot$labelSecY <- paste(round(dfMeadPoolsPlot$level,0), " - ", dfMeadPoolsPlot$name)


#Assume deliveries hold constant when we go to even lower reservoir levels than defined in the DCP or ISG
#Copy the last row
dfCutbacks <- rbind(dfCutbacks, dfCutbacks %>% slice(rep(n(), each = 1)))
#Change the elevation and storage

dfCutbacks[nrow(dfCutbacks),c("Mead Elevation (ft)")] <- dfMeadAllPools %>% filter(Reservoir %in% c("Mead"), variable %in% c("Inactive Capacity")) %>%
                select(level)
dfCutbacks[nrow(dfCutbacks),"MeadActiveVolume"] <- dfMeadAllPools %>% filter(Reservoir %in% c("Mead"), variable %in% c("Inactive Capacity")) %>%
              select(stor_maf)*1000000

sReservoir <- "Mead"

#Identify the reservoir maximum active storage
tMaxVol <- as.numeric(round(dfMaxStor %>% filter(Reservoir %in% c(sReservoir)) %>% select(Volume)-0.5,0))
# CRSS value
eRateToUse <- dfEvapRates %>% filter(Reservoir %in% c(sReservoir), Source %in% c("CRSS")) %>% select(Rate.ft.per.year)
# 5-year running average from Moreo (2015)
eRateToUse <- 6.2 #I suggest that it is better to use the available 5-yr average for the latest Moreo data for Mead (6.2 ft/yr 2010-2015) 

yMax = 10
yMin = 0
dfOneToOne <- data.frame(MeadVol = c(yMin,yMax), Delivery = c(yMin,yMax))

### Plot #1. DCP and ISG Deliveries versus Mead active storage
ggplot() +
  #DCP and ISG step functions
  geom_step(data=dfCutbacks[1:12,],aes(x=MeadActiveVolume/1000000,y=DeliveryISG/1000000, color = "ISG", linetype="ISG"), size=2, direction="vh") +
  geom_step(data=dfCutbacks[1:12,],aes(x=MeadActiveVolume/1000000,y=DeliveryDCP/1000000, color = "DCP", linetype="DCP"), size=2, direction="vh") +
  geom_line(data=dfOneToOne,aes(x=MeadVol,y=Delivery, color="1:1",linetype="1:1"), size=1) +
  
  scale_color_manual(name="Guide1",values = c("1:1"="Black","ISG"="Blue", "DCP"="Red"),breaks=c("ISG","DCP","1:1"), labels= c("Interim Shortage Guidelines (2008)","Drought Contingency Plan (2019)","1:1" )) +
  scale_linetype_manual(name="Guide1",values=c("1:1"="dashed","ISG"="longdash","DCP"="solid"), breaks=c("ISG", "DCP","1:1"), labels= c("Interim Shortage Guidelines (2008)","Drought Contingency Plan (2019)","1:1" )) +
  
  scale_x_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25), limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
                     sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$label)) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1)) +
  ylim(yMin,yMax) +
  theme_bw() +
  
  labs(x="Mead Active Storage (MAF)", y="Delivery (MAF per year)") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18), legend.position = c(0.7,0.5))

### Plot #2. DCP and ISG Deliveries versus Mead active storage with Mead protection level

#Protect to bottom of DCP cutbacks
lProtectLevel <- 1025
sProtectlabel <- "1:1 Line-Protect 1,025"
#Convert to acre-feet
vProtectLevel <- interp1(xi = lProtectLevel,x=dfMeadElevStor$`Elevation (ft)` , y=dfMeadElevStor$`Live Storage (ac-ft)`, method="linear")/1e6
# Construct a 1:1 line representing the Protection level. This line starts at (vProtectLevel,0)
dfProtectLine <- data.frame(MeadVol=c(vProtectLevel,vProtectLevel+yMax),Delivery=c(0,yMax))

ggplot() +
  #DCP and ISG step functions
  geom_step(data=dfCutbacks[1:12,],aes(x=MeadActiveVolume/1000000,y=DeliveryISG/1000000, color = "ISG", linetype="ISG"), size=2, direction="vh") +
  geom_step(data=dfCutbacks[1:12,],aes(x=MeadActiveVolume/1000000,y=DeliveryDCP/1000000, color = "DCP", linetype="DCP"), size=2, direction="vh") +
  geom_line(data=dfOneToOne,aes(x=MeadVol,y=Delivery, color="1:1 Line to Dead Pool",linetype="1:1 Line to Dead Pool"), size=1) +
  geom_line(data=dfProtectLine,aes(x=MeadVol,y=Delivery, color="1:1 Line-Protect 1,025",linetype="1:1 Line-Protect 1,025"), size=1) +
  
   
  scale_color_manual(name="Guide1",values = c("1:1 Line to Dead Pool"="Black","1:1 Line-Protect 1,025"="Grey","ISG"="Blue", "DCP"="Red"),breaks=c("ISG","DCP","1:1 Line to Dead Pool", "1:1 Line-Protect 1,025"), labels= c("Interim Shortage Guidelines (2008)","Drought Contingency Plan (2019)","1:1 Line to Dead Pool", "1:1 Line-Protect 1,025" )) +
  scale_linetype_manual(name="Guide1",values=c("1:1 Line to Dead Pool"="dashed","1:1 Line-Protect 1,025"="dashed","ISG"="longdash","DCP"="solid"), breaks=c("ISG", "DCP","1:1 Line to Dead Pool", "1:1 Line-Protect 1,025"), labels= c("Interim Shortage Guidelines (2008)","Drought Contingency Plan (2019)","1:1 Line to Dead Pool", "1:1 Line-Protect 1,025" )) +
  
  scale_x_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25), limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
                     sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$label)) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1)) +
  ylim(yMin,yMax) +
  theme_bw() +
  
  labs(x="Mead Active Storage (MAF)", y="Delivery (MAF per year)") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18), legend.position = c(0.7,0.5))


### Plot 3 - DCP Delivery vs Available Water for varying inflows. Available water is Mead Active Storage + Inflow.

# Create the data frame with deliveries as a function of mead active storage and inflow
cInflows <- c(5,6,7,8,8.5,9) #Million acre-feet per year
dfDeliveries <- dfCutbacks[, c("MeadActiveVolume", "DeliveryDCP", "DeliveryISG")]/1e6
dfDeliveries$Inflow <- 0
dfDeliveries[nrow(dfDeliveries),c("DeliveryDCP","DeliveryISG")] <- NA
dfDeliveriesInflows <- dfDeliveries
for (iFlow in cInflows){
  dfDeliveries$Inflow <- iFlow
  dfDeliveriesInflows <- rbind(dfDeliveriesInflows,dfDeliveries)
}

#Calculate available water
dfDeliveriesInflows$AvailableWater <- dfDeliveriesInflows$MeadActiveVolume + dfDeliveriesInflows$Inflow
dfDeliveriesInflows$Inflow=as.factor(dfDeliveriesInflows$Inflow)

#Specify te order for traces on the plot
cBreakOrder <- c("1:1 Line to Dead Pool","1:1 Line-Protect 1,025",cInflows)
cColorVals <- c(pBlues[2],"Grey","Black",pBlues[3:9])
cLineVals <- c("solid","longdash","dashed",rep("solid",times=length(cInflows)))

## Make the plot

ggplot() + 
  geom_step(data=dfDeliveriesInflows,aes(x=AvailableWater,y=DeliveryDCP, color=Inflow, linetype=Inflow), size=2, direction="vh") +
  geom_line(data=dfOneToOne,aes(x=MeadVol,y=Delivery, color="1:1 Line to Dead Pool", linetype="1:1 Line to Dead Pool"), size=1.5) +
  geom_line(data=dfProtectLine,aes(x=MeadVol,y=Delivery, color="1:1 Line-Protect 1,025", linetype="1:1 Line-Protect 1,025"), size=1.5) +

  scale_color_manual(name="Guide1", values = cColorVals, breaks=cBreakOrder) +
  scale_linetype_manual(name="Guide1",values = cLineVals, breaks=cBreakOrder) +

  scale_x_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25), limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
                     sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$label)) +
  
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1)) +
  ylim(yMin,yMax) +
  theme_bw() +
  
  labs(x="Available Water (Mead Active Storage + Inflow) (MAF)", y="Delivery (MAF per year)") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18), legend.position = c(0.7,0.5))



###############################################################################################
# RUN MEAD SIMULATIONS STARTING AT CURRENT APRIL 2019 storage WITH DIFFERNT cONSTANT INFLOWS ####
#
# Make a plot of reservoir storage (y-axis) over time. Show two zones above and below Mead 1025 ft
###############################################################################################
#Create the master dataframe of results
dfInflowSimulations <- data.frame(Storage=0, Year=0, index=0, Inflow=0, Release=0)
#Mead Initial Storage on April 9, 2019
sMeadApril2019 <- interp1(xi = 1089.74,y=dfMeadElevStor$`Live Storage (ac-ft)`,x=dfMeadElevStor$`Elevation (ft)`, method="linear")
sMeadDCPBottom <- interp1(xi = 1000,y=dfMeadElevStor$`Live Storage (ac-ft)`,x=dfMeadElevStor$`Elevation (ft)`, method="linear")
#Define start year
startYear <- 2019
#Define the maximum number of iterations. Use an even number so the inflow labels plot nicely
maxIts <- 24

#Loop over stead natural inflow values (stress tests)
for (tInflow in seq(5,12, by=1)*1e6){
  
    #tInflow <- 6e6
    #debug(TimeToReservoirTarget)
  
    # With lower basin delivery losses
    tRes <- TimeToReservoirTarget(Sinit = sMeadApril2019, inflow = tInflow, deliveryVolume = dfCutbacks$DeliveryDCP, 
                                deliveryResStorage = dfCutbacks$MeadActiveVolume, eRate = eRateToUse,  ResArea = dfMeadElevStor$`Area (acres)`, 
                                ResVolume = dfMeadElevStor$`Live Storage (ac-ft)`, MaxIts = maxIts, sMethodRelease = "constant", 
                                sMinTarget = sMeadDCPBottom, sMaxTarget = tMaxVol*1e6, startYear = startYear )
  
   # Without lower basin delivery losses
    #tRes <- TimeToReservoirTarget(Sinit = sMeadApril2019, inflow = tInflow, deliveryVolume = dfCutbacks$DeliveryDCP, 
    #           deliveryResStorage = dfCutbacks$MeadActiveVolume, eRate = eRateToUse,  ResArea = dfMeadElevStor$`Area (acres)`, 
    #           ResVolume = dfMeadElevStor$`Live Storage (ac-ft)`, MaxIts = maxIts, sMethodRelease = "constant", 
    #              sMinTarget = 0, sMaxTarget = tMaxVol*1e6, startYear = startYear )

    #Append results to dataframe   
    dfInflowSimulations <- rbind(dfInflowSimulations, tRes$dfTimeResults)
    
}

#Remove the first dummy row of zeros
dfInflowSimulations <- dfInflowSimulations[2:nrow(dfInflowSimulations),]

# Plot up storage over time for different inflow traces.
dfTimeResults <- dfInflowSimulations    
# Calculate Steady Natural Lees Ferry Flow from Mead Inflow
# Lee Ferry Natural Flow = Mead Inflow - Grand Canyon Trib Flows + Upper Basin Demands + Powell Evaporation
ePowellRate <- dfEvapRates %>% filter(Reservoir %in% c("Powell"), Source %in% c("Reclamation")) %>% select(Rate.ft.per.year)
ePowellArea <- interp1(xi = 9e6,x=dfPowellElevStor$`Live Storage (ac-ft)` , y=dfPowellElevStor$`Area (acres)`, method="linear")

vMeadInflowToLeeNaturalCorrection <- -300000 + 5e6 + ePowellRate*ePowellArea
dfTimeResults$LeeFerryNaturalFlow <- dfTimeResults$Inflow + as.numeric(vMeadInflowToLeeNaturalCorrection )


# Select even rows for plotting flow labels
dfTimeResultsEven <- dfTimeResults[seq(3,nrow(dfTimeResults),by=3),]

## Define a polygons that identify the follow:
# 1. Level below Mead 1025 where deliveries are no longer defined by Drought Contingency Plan
# 2. Levels between Mead 1090 and 1025 where deliveries are defined by Drought COntingency Plan
# Define the polygons showing each tier to add to the plot. A polygon is defined by four points in the plot space. Lower-left, Lower-right, upper-right, upper left
# Polygon name
ids <- factor(c("Mead Releases Undefined\nStates Renegotiate","Drought Contingency Plan\nReleases"))
# Polygon corners (see above for defs)
dfPositions <- data.frame(id = rep(ids, each = 4),
                          Year = c(startYear,startYear+maxIts,startYear+maxIts,startYear,startYear,startYear+maxIts,startYear+maxIts,startYear),
                          MeadVol = c(0,0,dfMeadValsAdd$value[6],dfMeadValsAdd$value[6],dfMeadValsAdd$value[6],dfMeadValsAdd$value[6],dfMeadValsAdd$value[3],dfMeadValsAdd$value[3]))
#Number of polygons
nPts <- nrow(dfPositions)/4

#Polygon labels
dfPolyLabel <- data.frame(id = ids,
                         Label = c("Mead Releases Undefined\nStates Renegotiate", "Drought Contingency Plan\nReleases"),
                         DumVal = c(1:nPts))


#Calculate midpoints for each polygon. This is the average of the cooridinates for
# the polygon

dfPolyLabel$MidYear <- 0
dfPolyLabel$MidMead <- 0
dfPolyLabel$MidInflow <- mean(c(5,12))

for (point in 1:nPts) {
  #dfPolyLabel[point,c("MidYear")] = mean(dfPositions[(4*(point-1)+1):(4*point),c("Year")])
  #Weighted average for Year to push things to the right of the plot
  dfPolyLabel[point,c("MidYear")] =  0.35*min(dfPositions[(4*(point-1)+1):(4*point),c("Year")]) + 0.65*max(dfPositions[(4*(point-1)+1):(4*point),c("Year")])
  if (point==1) {
    dfPolyLabel[point,c("MidMead")] = mean(dfPositions[(4*(point-1)+1):(4*point),c("MeadVol")])
  } else {
    dfPolyLabel[point,c("MidMead")] = 0.35*dfPositions[(4*(point-1)+1),c("MeadVol")] + 0.65*dfPositions[(4*point),c("MeadVol")]
  }
  
}


# Currently we need to manually merge the two together
dfPolyAll <- merge(dfPolyLabel, dfPositions, by = c("id"))

#Add a variable for the annual inflow max and mins
dfPolyAll$Inflow <- c(5,12,12,5,5,12,12,5)
dfPolyAll$MidInflow <- mean(5,12)


#vertical line to show when the interim guidelines expire
tInterGuideExpire <- 2026
dfIntGuidelinesExpire <- data.frame(Year = c(tInterGuideExpire,tInterGuideExpire), MeadVol <- c(0,tMaxVol))

#Colors for the polygons
palReds <- brewer.pal(9, "Reds") #For plotting DCP tiers

#Now do the plot: Storage versus time with different Steady Mead inflow traces. Different DCP zones. And a vertical line showing the end of the Interim Guidelines
ggplot() +
  #Polygon zones
  geom_polygon(data = dfPolyAll, aes(x = Year, y = MeadVol/1e6, group = id, fill = as.factor(dfPolyAll$DumVal)), show.legend = F) +
  #Inflow traces
  geom_line(data=dfTimeResults,aes(x=Year,y=Storage/1e6, group = Inflow/1e6, color = (Inflow/1e6)), size=2) +
  
  #Interim guidelines expire
  geom_line(data=dfIntGuidelinesExpire,aes(x=Year,y=MeadVol, linetype="IntGuide"), size=3,show.legend = F) +
  scale_linetype_manual(name="Guide1", values = c("IntGuide"="longdash"), breaks=c("IntGuide"), labels= c("Interim Guidelines Expire")) +
  geom_text(aes(x=tInterGuideExpire, y=25, label="Interim Guidelines\nExpire"), angle = 0, size = 7, hjust="middle") +
  geom_label(aes(x=2037, y=20, label="Steady Inflow (MAF/year)\n(Stress Test)", fontface="bold"), angle = 0, size = 7) +
  
  
   
  #Label the constant inflow contours
  geom_label(data=dfTimeResultsEven , aes( x = Year, y = Storage/1e6, label = Inflow/1e6, fontface="bold"), size=5, angle = 0) + 
  #Label the polygons
  geom_label(data=dfPolyLabel, aes(x = MidYear, y = MidMead/1e6, label = Label, fontface="bold"), size=6, angle = 0) + 
  
  #Y-axis: Active storage on left, Elevation with labels on right 
  scale_y_continuous(breaks = seq(0,tMaxVol,by=5), labels = seq(0,tMaxVol,by=5), limits = c(0, tMaxVol), 
                     sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$labelSecY)) +
  #limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
  #scale_y_continuous(breaks = seq(0,50,by=10), labels = seq(0,50,by=10), limits = c(0, 50)) +

  #Color scale for polygons - increasing red as go to lower levels
  scale_fill_manual(breaks = c(2,1),values = c(palReds[5],palReds[4]),labels = dfPolyLabel$Label ) + 
  
    
  theme_bw() +
  
  labs(x="Year", y="Mead Active Storage (MAF)", color =  "Natural Inflow\n(MAF/year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        legend.position = "none")


#Calculate the final Mead Elevation
dfTimeResults$Elevation <- interp2(xi = dfTimeResults$Storage,y=dfMeadElevStor$`Elevation (ft)` , x=dfMeadElevStor$`Live Storage (ac-ft)`, method="linear")

#Get the blue color bar
pBlues <- brewer.pal(9,"Blues")

#PLOT 2: Storage versus time with different Steady Mead inflow traces. Different DCP zones. And a vertical line showing the end of the Interim Guidelines. Line Labels Show the reservoir release
ggplot() +
  #Polygon zones
  geom_polygon(data = dfPolyAll, aes(x = Year, y = MeadVol/1e6, group = id, fill = as.factor(dfPolyAll$DumVal)), show.legend = F) +
  #Inflow traces
  geom_line(data=dfTimeResults,aes(x=Year,y=Storage/1e6, group = Inflow/1e6, color = (Inflow/1e6), size= (Inflow/1e6))) +
  
  #Interim guidelines expire
  geom_line(data=dfIntGuidelinesExpire,aes(x=Year,y=MeadVol, linetype="IntGuide"), size=3,show.legend = F) +
  scale_linetype_manual(name="Guide1", values = c("IntGuide"="longdash"), breaks=c("IntGuide"), labels= c("Interim Guidelines Expire")) +
  geom_text(aes(x=tInterGuideExpire, y=25, label="Interim Guidelines\nExpire"), angle = 0, size = 7, hjust="middle") +
  geom_label(aes(x=2037, y=20, label="Release\n(MAF/year)", fontface="bold"), angle = 0, size = 7) +
  
  
  #Label the lines with release
  geom_label(data=dfTimeResultsEven , aes( x = Year, y = Storage/1e6, label = round(Release/1e6,1), fontface="bold"), size=5, angle = 0) + 
  #Label the polygons
  geom_label(data=dfPolyLabel, aes(x = MidYear, y = MidMead/1e6, label = Label, fontface="bold"), size=6, angle = 0) + 
  
  #Y-axis: Active storage on left, Elevation with labels on right 
  scale_y_continuous(breaks = seq(0,tMaxVol,by=5), labels = seq(0,tMaxVol,by=5), limits = c(0, tMaxVol), 
                     sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$labelSecY)) +
  #limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
  #scale_y_continuous(breaks = seq(0,50,by=10), labels = seq(0,50,by=10), limits = c(0, 50)) +
  
  #Color scale for polygons - increasing red as go to lower levels
  scale_fill_manual(breaks = c(2,1),values = c(palReds[5],palReds[4]),labels = dfPolyLabel$Label ) + 
  scale_color_continuous(low=pBlues[2],high=pBlues[9]) +
  guides(color = guide_legend("Steady Inflow\n(MAF/year)"), size = guide_legend("Steady Inflow\n(MAF/year)")) +
  
  theme_bw() +
  
  labs(x="Year", y="Mead Active Storage (MAF)") + #, color =  "Steady Inflow\n(MAF/year)", size = "Steady Inflow\n(MAF/year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), legend.text=element_text(size=18)) #,
        #legend.position = "none")





#Another Plot of Estimated Lee Ferry Natural Flow: storage versus time with different Lee Ferry Natural inflow traces. Different DCP zones. And a vertical line showing the end of the Interim Guidelines
ggplot() +
  #Polygon zones
  geom_polygon(data = dfPolyAll, aes(x = Year, y = MeadVol/1e6, group = id, fill = as.factor(dfPolyAll$DumVal)), show.legend = F) +
  #Inflow traces
  geom_line(data=dfTimeResults,aes(x=Year,y=Storage/1e6, group = LeeFerryNaturalFlow/1e6, color = (LeeFerryNaturalFlow/1e6)), size=2) +
  
  #Interim guidelines expire
  geom_line(data=dfIntGuidelinesExpire,aes(x=Year,y=MeadVol, linetype="IntGuide"), size=3,show.legend = F) +
  scale_linetype_manual(name="Guide1", values = c("IntGuide"="longdash"), breaks=c("IntGuide"), labels= c("Interim Guidelines Expire")) +
  geom_text(aes(x=tInterGuideExpire, y=25, label="Interim Guidelines\nExpire"), angle = 0, size = 7, hjust="middle") +
  geom_label(aes(x=2037, y=18, label="Lee Ferry Natural Flow (MAF/year)\n= [Mead Inflow] - [GC Tribs] + [Powell Evap] + [UB Consump. Use]\n= [Mead Inflow] - 0.3 + 0.46 + 5", fontface="bold"), angle = 0, size = 7) +
  
  
  
  #Label the constant inflow contours
  geom_label(data=dfTimeResultsEven , aes( x = Year, y = Storage/1e6, label = round(LeeFerryNaturalFlow/1e6,0), fontface="bold"), size=5, angle = 0) + 
  #Label the polygons
  geom_label(data=dfPolyLabel, aes(x = MidYear, y = MidMead/1e6, label = Label, fontface="bold"), size=6, angle = 0) + 
  
  #Y-axis: Active storage on left, Elevation with labels on right 
  scale_y_continuous(breaks = seq(0,tMaxVol,by=5), labels = seq(0,tMaxVol,by=5), limits = c(0, tMaxVol), 
                     sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$labelSecY)) +
  #limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
  #scale_y_continuous(breaks = seq(0,50,by=10), labels = seq(0,50,by=10), limits = c(0, 50)) +
  
  #Color scale for polygons - increasing red as go to lower levels
  scale_fill_manual(breaks = c(2,1),values = c(palReds[5],palReds[4]),labels = dfPolyLabel$Label ) + 
  
  
  theme_bw() +
  
  labs(x="Year", y="Mead Active Storage (MAF)", color =  "Natural Inflow\n(MAF/year)") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        legend.position = "none")






###############################################################################################
# CALCULATE Final state as a function of Steady Inflow and Initial Reservoir storage ####
#
#  Final state is either years to lower Target (e.g., Mead 1025 feet), years to Fill, or steady state volume
#
# Make a plot of inflow (x-axis), initial reservoir storage (y-axis), and time to bads
###############################################################################################
#Create the master dataframe of results
dfInflowStorageSimulations <- data.frame(InitStorage=0, Inflow=0, FinalStorage=0, Status="dummy", Year=0, index=0, Release=0)
maxIts <- 100

#Loop over initial storage
for (tInitStorage in seq(6,tMaxVol,by=2)*1e6) {

  #Loop over stead natural inflow values (stress tests)
  for (tInflow in seq(5,12, by=1)*1e6){
    
    #tInflow <- 6e6
    #debug(TimeToReservoirTarget)
    tRes <- TimeToReservoirTarget(Sinit = tInitStorage, inflow = tInflow, deliveryVolume = dfCutbacks$DeliveryDCP, # deliveryVolume = dfCutbacks$DeliveryDCP, 
                                  deliveryResStorage = dfCutbacks$MeadActiveVolume, eRate = eRateToUse,  ResArea = dfMeadElevStor$`Area (acres)`, 
                                  ResVolume = dfMeadElevStor$`Live Storage (ac-ft)`, MaxIts = maxIts, sMethodRelease = "constant", 
                                  sMinTarget = dfMeadValsAdd$value[6], sMaxTarget = tMaxVol*1e6, startYear = startYear )
    
    #Append results to dataframe   
    dfTempRecord <- data.frame(InitStorage=tInitStorage, Inflow=tInflow, FinalStorage=tRes$volume, Status=tRes$status, Year=startYear+tRes$periods, index=tRes$periods, Release=tRes$dfTimeResults$Release)
    dfInflowStorageSimulations <- rbind(dfInflowStorageSimulations, dfTempRecord)
    
  }
}
#Remove the first dummy row of zeros
dfInflowStorageSimulations <- dfInflowStorageSimulations[2:nrow(dfInflowStorageSimulations),]

# Plot up storage over time for different inflow traces.
dfTimeInflowStorageResults <- dfInflowStorageSimulations    
# Select even rows for plotting flow labels
dfTimeInflowStorageResultsEven <- dfTimeInflowStorageResults[seq(2,nrow(dfTimeResults),by=2),]

xLabelPos <- 0.7*max(dfTimeInflowStorageResults$Inflow/1e6) # in inflow units
#Flow scale
xFlowScale <- seq(min(dfTimeInflowStorageResults$Inflow),max(dfTimeInflowStorageResults$Inflow),1e6)/1e6

#Calculate label as either years to bottom target, years to full, or steady-state storage in maf
dfTimeInflowStorageResults$Label <- ifelse(dfTimeInflowStorageResults$Status == "Middle",
                                           paste(round(dfTimeInflowStorageResults$FinalStorage/1e6, digits=1),'maf'),
                                           paste(dfTimeInflowStorageResults$index,"yr"))




#Now do the plot: X-axis is inflow, y-axis is initial storage, z-labels are time to catastrophy.
ggplot() +
  #Polygon zones
  geom_polygon(data = dfPolyAll, aes(x = Inflow, y = MeadVol/1e6, group = id, fill = as.factor(dfPolyAll$DumVal)), show.legend = F) +
  
#  geom_polygon(data = dfPolyAll, aes(x = Year, y = MeadVol/1e6, group = id, fill = as.factor(dfPolyAll$DumVal)), show.legend = F) +
  #Inflow traces
  #geom_line(data=dfTimeInflowStorageResults,aes(x=Year,y=Storage/1e6, group = Inflow/1e6, color = (Inflow/1e6)), size=2) +
  
#  geom_text(aes(x=xLabelPos, y=23, label="Time to Mead 1025\n(Years)"), angle = 0, size = 7, hjust="middle") +

  #Plot labels that show the the numbre of years to
   
  #geom_label(data=dfTimeInflowStorageResults , aes( x = Inflow/1e6, y = InitStorage/1e6, label = round(FinalStorage/1e6,1), color = Status , size = index ,fontface="bold"),  angle = 0) + 
  geom_label(data=dfTimeInflowStorageResults , aes( x = Inflow/1e6, y = InitStorage/1e6, label = Label, color = Status , size = 5 ,fontface="bold"),  angle = 0) + 
  #Label the polygons
  geom_label(data=dfPolyLabel[1,], aes(x = MidInflow, y = MidMead/1e6, label = Label, fontface="bold"), size=6, angle = 0) + 
  
    #Label the polygons
#  geom_label(data=dfPolyLabel, aes(x = xLabelPos, y = MidMead/1e6, label = Label, fontface="bold"), size=6, angle = 0) + 
  
  #Y-axis: Active storage on left, Elevation with labels on right 
  scale_y_continuous(breaks = seq(0,tMaxVol,by=5), labels = seq(0,tMaxVol,by=5), limits = c(0, tMaxVol), 
                     sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$labelSecY)) +
  scale_x_continuous(breaks = xFlowScale, labels = xFlowScale) +
  #limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
  #scale_y_continuous(breaks = seq(0,50,by=10), labels = seq(0,50,by=10), limits = c(0, 50)) +
  
  #Color scale for polygons - increasing red as go to lower levels
  scale_fill_manual(breaks = c(2,1),values = c(palReds[5],palReds[4]),labels = dfPolyLabel$Label ) + 
  #scale_fill_manual(guide="Guide2", breaks = c("Top","Middle","Bottom"),values = c("Blue","Green","Red"),labels = c("Fill (years)","Steady volume (maf)","To 1,025 (years)" )) + 
  scale_color_manual(breaks = c("Top","Middle","Bottom"), values=c("red","green","blue"), labels=c("To Fill (years)","Steady volume (maf)","To 1,025 (years)")) +
  
  
  theme_bw() +
  
  scale_size(guide="none") +
  
  labs(x="Steady Inflow (MAF/year)", y="Mead Active Storage (MAF)", color =  "End State") +
  #theme(text = element_text(size=20), legend.title=element_blank(), legend.text=element_text(size=18),
  #      legend.position = c(0.8,0.7))
  theme(text = element_text(size=20), legend.text=element_text(size=18), 
        panel.border = element_rect(colour = "black", fill=NA),
        legend.box.background = element_rect(colour = "black"),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.position = c(1.125,0.85)) #,
        #legend.position = "none")





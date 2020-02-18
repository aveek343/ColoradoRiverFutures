# ICS-Plots.r
#
# Make stacked bar graph of state Intentionally Created Surplus holdings by year
#
# Data is USBR Water Accounting Reports: https://www.usbr.gov/lc/region/g4000/wtracct.html in source Excel file
# Please report bugs/feedback to:
#
# David E. Rosenberg
# February 11, 2020
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraies

if (!require(tidyverse)) { 
  install.packages("tidyverse", repos="https://cran.cnr.berkeley.edu/", verbose = TRUE) 
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

if (!require(ggplot)) { 
  install.packages("ggplot", repo="http://cran.r-project.org")
  library(ggplot) 
}

if (!require(stringr)) { 
  install.packages("stringr", repo="http://cran.r-project.org")
  library(stringr) 
}



# Load Data

sExcelFile <- 'IntentionallyCreatedSurplus-Summary.xlsx'
dfICSBalance <- read_excel(sExcelFile, sheet = "Sheet1",  range = "B6:F10")

cColNames <- colnames(dfICSBalance)

#Melt the data so state columns become a variable
dfICSBalanceMelt <- melt(data = dfICSBalance,id.vars = "Year", measure.vars = cColNames[1:3])

#Plot #1. Stacked bar chart of account balancy by state by year

ggplot() +
  
  geom_bar(data=dfICSBalanceMelt, aes(fill=variable,y=value/1e6,x=Year),position="stack", stat="identity") +
  
  scale_fill_manual(name="Guide1",values = c("Blue", "Red", "Purple"),breaks=cColNames[1:3]) +
  
  #scale_x_continuous(breaks = c(0,5,10,15,20,25),labels=c(0,5,10,15, 20,25), limits = c(0,as.numeric(dfMaxStor %>% filter(Reservoir %in% c("Mead")) %>% select(Volume))),
   #                  sec.axis = sec_axis(~. +0, name = "Mead Level (feet)", breaks = dfMeadPoolsPlot$stor_maf, labels = dfMeadPoolsPlot$label)) +
 
  guides(fill = guide_legend(keywidth = 1, keyheight = 1)) +
  
  theme_bw() +
  
  labs(x="", y="Intentionally Created Surplus\nAccount Balance\n(MAF)") +
  theme(text = element_text(size=20),  legend.title = element_blank(), legend.text=element_text(size=18))



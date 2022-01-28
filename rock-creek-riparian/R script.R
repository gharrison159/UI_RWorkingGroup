##Test script for RRCR Riparian monitoring dataset##
setwd("~/Rangeland Center/Research/Git Depot/UI_RWorkingGroup/rock-creek-riparian")
KR_RRCR_2021_Riparian_Bioblitzdata_Final <- read.csv("~/Rangeland Center/Research/R Script/KR_RRCR_2021_Riparian_Bioblitzdata_Final.csv", header=TRUE)
View(KR_RRCR_2021_Riparian_Bioblitzdata_Final)

blitz.data <- read.csv("~/Rangeland Center/Research/R Script/KR_RRCR_2021_Riparian_Bioblitzdata_Final.csv", header = TRUE)
head(blitz.data) #Check the column titles
str(blitz.data) #Look at all of the column headers

###################################################
#####PERCENT FOLIAR COVER BY WETLAND INDICATOR#####
###################################################
#Calculated as: [(# of points with at least one hit of wetland indicator A)/(# of points)]*100

#####Site Scale#####

#####New data set without extra columns
b.dat <- data.frame(blitz.data)
library(dplyr) #Install and call the dplyr package
col_remove <- c("Date", "GPS.E..Start.", "GPS.N..Start.", "GPS.E..End.", "GPS.N..End.", "Point.Spacing..Paces.",
                "Top", "Second", "Third", "Fourth", "Fifth", "Sixth", "Surface", "Height", "Top.1", "Second.1",
                "Third.1", "Fourth.1", "Fifth.1", "Channel.Width..m.", "FAC.comp", "FACW.comp", "FACU.comp",
                "OBL.comp", "UPL.comp", "NONE.comp","GRASS.OR.GRASSLIKE.cvr", "GRASS.OR.GRASSLIKE.comp",
                "FORB.cvr", "FORB.comp", "WOODY.cvr", "WOODY.comp", "LITTER.cvr", "LITTER.comp") #Identify unneeded columns for removal
b.dat <- b.dat %>%
  select(- one_of(col_remove)) #Remove identified columns
str(b.dat)
###################################################################################################
###################################################################################################
##### University of Idaho - Rinker Rock Creek Ranch - Bioblitz Data Summary Statistics - 2021 #####
#####                                 Riparian Monitoring Blitz                               #####
###################################################################################################
###################################################################################################

#####Before continuing:
#Wetland indicator status has been designated using the Wetland Indicator Status rating determined
#by the U.S. Army Corps of Engineers, the Fish and Wildlife Service, the Environmental Protection Agency,
#and the Natural Resources Conservation Service using taxonomic information from the Fish and Wildlfie
#Service. This information is directed by the Corps of Engineers.

#As of 10/22/2021, a current link to the website is: https://plants.usda.gov/home/wetlandSearch

#Indicator categories:
#OBL / Obligate Wetland / Hydrophyte / Almost always occur in wetlands
#FACW / Facultative Wetland / Hydrophyte / Usually occur in wetlands, but may occur in non-wetlands
#FAC / Facultative / Hydrophyte / Ocur in wetlands and non-wetlands
#FACU / Facultative Upland / Nonhydrophte / Usually occur in non-wetlands, but may occur in wetlands
#UPL / Obligate Upland / Nonhydrophyte / Almost never occur in wetlands
setwd("~/Rangeland Center/Research/Git Depot/UI_RWorkingGroup/rock-creek-riparian")
KR_RRCR_2021_Riparian_Bioblitzdata_Final <- read.csv("~/Rangeland Center/Research/Git Depot/UI_RWorkingGroup/rock-creek-riparian/KR_RRCR_2021_Riparian_Bioblitzdata_Final.csv", header=TRUE)
View(KR_RRCR_2021_Riparian_Bioblitzdata_Final)

blitz.data <- read.csv("~/Rangeland Center/Research/R Script/KR_RRCR_2021_Riparian_Bioblitzdata_Final.csv", header = TRUE)
head(blitz.data) #Check the column titles
str(blitz.data) #Look at all of the column headers

###################################################
#####PERCENT Composition BY WETLAND INDICATOR#####
###################################################
#Calculated as: [(# of points with at least one hit of wetland indicator A)/(# of hits)]*100

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

####Group data and sum hits for each for each wetland indicator at each site. Included total hits
b.dat$Site <- as.factor(b.dat$Site)
levels(b.dat$Site)
agg.b.dat <- aggregate(cbind(FAC.cvr, FACW.cvr, FACU.cvr, OBL.cvr, UPL.cvr, NONE.cvr, TOTAL) ~ Site, data = b.dat, FUN = sum)
agg.b.dat ##provides sum of the points for each site for each wetland indicator

####Rename columns 
colnames(agg.b.dat) <- c("Site", "FAC", "FACW", "FACU", "OBL", "UPL", "NONE", "TOTAL")
agg.b.dat

####Convert from wide format to long format for plotting
library(tidyr) #install and call the tidyr package
b.dat.long <- gather(agg.b.dat, Wet.Ind, Cover, FAC:NONE, factor_key = TRUE)
b.dat.long

#Calculate composition from LPI data at transect scale 
#[(# of points with at least one hit of Wetland Indicator A)/(# of hits)]*100
#NOTE: Denominator is determined by the total number of pin drops per site (chance of being intercepted)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "East Fork", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "West Fork One", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "West Fork Two", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Smith Creek Four", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Smith Creek Five", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Long Gulch One", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Long Gulch Three", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Hattie Gulch One", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Little Rock Creek Five", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Little Rock Creek Four", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Guy Canyon Eight", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)
b.dat.long$Cover <- ifelse(b.dat.long$Site == "Guy Canyon Ten", (b.dat.long$Cover/b.dat.long$TOTAL) * 100, b.dat.long$Cover)

####Summary of Entire Dataset
#####Full data set summary statistics
aggregate(cbind(Cover)~Wet.Ind, data = b.dat.long, FUN = sum)
##percent composition by wetland indicator

#####Use long format data to plot visual of mean Cover of each wetland indicator
library(ggplot2) #install and call ggplot2 package
library(scales) #install and call scales package

#Specify factors and numerics for dataset
b.dat.long$Site <- as.factor(b.dat.long$Site)
b.dat.long$Wet.Ind <- as.factor(b.dat.long$Wet.Ind)
b.dat.long$Cover <- as.numeric(b.dat.long$Cover)

#Specify levels and orders for plotting
levels(b.dat.long$Wet.Ind)
level_order <- c("NONE", "UPL", "FACU", "FAC", "FACW", "OBL")
level_ord <- c("East Fork", "Guy Canyon Eight", "Guy Canyon Ten", "Hattie Gulch One",
               "Little Rock Creek Four", "Little Rock Creek Five",
               "Long Gulch One", "Long Gulch Three", "Smith Creek Four", "Smith Creek Five",
               "West Fork One", "West Fork Two")
b.dat.long$Wet.Ind <- factor(b.dat.long$Wet.Ind, levels = level_order)
levels(b.dat.long$Wet.Ind)

#Plot percent composition of wetland indicator by site 
p <- ggplot(b.dat.long, aes(x = factor(Site, level = level_ord), y = Cover, fill = Wet.Ind)) + geom_bar(stat = "identity", color = "black")
p <- p + theme(axis.text.x = element_text(angle = 60, hjust = 1))
p <- p + labs(title = "RRCR - Riparian Sites 2021 - Wetland Indicator Cover", 
              x = "RRCR Riparian Site", y = "Composition (%)")
p <- p + labs(fill = "Wetland Indicator Status")
p <- p + theme(axis.text.x = element_text(size = 12),
               axis.text.y = element_text(size = 12),
               axis.title.x = element_text(size = 14, face = "bold"),
               axis.title.y = element_text(size = 14, face = "bold"))
p 

#####Remove all of the unneeded data
remove(p, b.dat.long, agg.b.dat)


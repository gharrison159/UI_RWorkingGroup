---
title: "Hill Pasture Richness analysis (2020 + 2021 data)"
author: "Georgia Harrison"
date:"Jan 25, 2021"
output: html_document
---
  
#################################
#library all the necessary packages
library(dplyr)
library(vegan)
library(agricolae) #for LSD
library(ggplot2)
library(RColorBrewer) # for graph color palettes

#################################
#file import:
sp_list <-read.csv("Hill_splist_meta.csv", header = T)
#species list with plant habit 
#habits are AF, PF, PG, AG, and  S


hill19 <- read.csv("hill_2019_midpoint.csv", header = T)
hill20 <- read.csv("hill_2020_midpoint.csv", header = T)
hill21 <- read.csv("hill_2021_midpoint.csv", header = T)
#midpoint for each species and plot from 2019, 2020, and 2021

plotmeta <- read.csv("hill_plot_metadata.csv", header = T)
#treatment information for all the plots by the unique identifier


########################
#create the categories we will use for analyis in plotmeta
chem_drop <- 
  paste(plotmeta$Chem_code, plotmeta$Droplet_cat, sep = "_")
#fill in "_NA" as control plots
chem_drop_clean <- 
  replace(chem_drop, chem_drop == '_NA', 'C')

plotmeta$chem_drop <- chem_drop_clean

#################################
#Subset the cover data to a species by plot matrix
#no plot lebels or bare ground measurements 
# accomplish this by slicing the data at the first and last plant column

head(hill19,2)#first view the column headings
# record first and last plant code column and slice by that
all.abun.19 <- 
  hill19[,which(colnames(hill19)=="Aforb"):which(colnames(hill19)=="CHVI8")]
#the 'which colnames' thing outputs an integer which is the column # for that species

head(hill20,2)
all.abun.20 <- 
  hill20[,which(colnames(hill20)=="ACMI2"):which(colnames(hill20)=="ZIPA2")]

head(hill21,2)
all.abun.21 <- 
  hill21[,which(colnames(hill21)=="ACMI2"):which(colnames(hill21)=="ZIPA2")]


#bare ground
bground.19 <-hill19[,which(colnames(hill20)=="BGROUND")]
bground.20 <-hill20[,which(colnames(hill19)=="Bground")]
bground.21 <-hill21[,which(colnames(hill21)=="BGROUND")]



#################################
#okay now we need to subset the plant functional group of interest from the species matrix
#in this case we are only interested in annual grass cover but you could do the same thing with other functional groups
#if there were a bunch of plant functional groups, we could do this in a fancier way but with AG, 
#there are only two species: BRJA and BRTE

#2019
ag.abun.19 <- 
  hill19[,which(colnames(hill19)=="BRJA"):which(colnames(hill19)=="BRTE")]

#same thing for 2020 data
ag.abun.20 <- 
  hill20[,which(colnames(hill20)=="BRJA"):which(colnames(hill20)=="BRTE")]

ag.abun.21 <- 
  hill21[,which(colnames(hill21)=="BRTE")] #NO BRJA in 2021





#################################
#recombine all the years of data into one dataset for analysis and plotting
#combine by a unique but consistent plot labeling 
#was getting a weird error with just using one column UNIQUE_PLOT_ID, so used two and it worked
#summarize average annual grass cover for each year within each treatment combo


#pull the plot ID from the original cover data
ag19_sum <- hill19[,c("UNIQUE_PLOT_ID",'Trt')]
#input the AG cover data from that year as a new column
ag19_sum$ag_cover_19  <- rowSums(ag.abun.19)


ag20_sum <- hill20[,c("UNIQUE_PLOT_ID",'Trt')]
ag20_sum$ag_cover_20  <- rowSums(ag.abun.20)

ag21_sum <- hill21[,c("UNIQUE_PLOT_ID",'Trt')]
ag21_sum$ag_cover_21  <- ag.abun.21



#combine 2019, 2020, and 2021 cover
combine_ag_cover <- 
  right_join(ag19_sum, ag21_sum, by="UNIQUE_PLOT_ID") %>%
  right_join(., ag20_sum,by="UNIQUE_PLOT_ID")
#need to use a right join becuase there are more plots in later years than inital


#clean up to only the columns we are interested in 
head(combine_ag_cover,3)
combine_ag_cover <- select(combine_ag_cover, UNIQUE_PLOT_ID,
                           ag_cover_19, ag_cover_20, ag_cover_21)




#################################
# add in treatment and plot metadata to AG cover dataset
head(plotmeta)
#decide which metadata columns we want
meta_cols = c('UNIQUE_PLOT_ID', 'chem_drop')

combine_ag_cover <-
  inner_join(combine_ag_cover, select(plotmeta, all_of(meta_cols)), 
             by = "UNIQUE_PLOT_ID")



head(combine_ag_cover)

#########################
#analysis!
##########################


##########################
###calculate Percent control of annual grasses
#formula: (1-(%weed in treatment / % weed in untreated check))*100
#first we need to know the average % weed cover in control plots
combine_ag_cover$chem_drop <- factor(combine_ag_cover$chem_drop)


combine_ag_cover %>%
  group_by(chem_drop)%>%
  summarize(mean=mean(ag_cover_20))
#average cover of AG in control plots is 41.2

combine_ag_cover %>%
  group_by(chem_drop)%>%
  summarize(mean=mean(ag_cover_21))
#average cover of AG in control plots is 24.1


#create a column of P control 20
combine_ag_cover$Pcontrol20 <- 
  (1-(combine_ag_cover$ag_cover_20)/ 41.2)*100

#create a column of P control 20
combine_ag_cover$Pcontrol21 <- 
  (1-(combine_ag_cover$ag_cover_21)/24.1)*100


#############
#create new summary table with average ag cover and percent control for each treatment group and year

exportable <- 
  combine_ag_cover %>%
  group_by(chem_drop)%>%
  summarize(cover20=mean(ag_cover_20),
            cover21=mean(ag_cover_21),
            cover19=mean(ag_cover_19),
            control20 = mean(Pcontrol20),
            control21 = mean(Pcontrol21)
            )


##########################
#anovas and LSD
#########
#percent control of annual grasses
#2020 percent control
t <- aov(Pcontrol20~chem_drop, data=combine_ag_cover)
summary(t)
out2020 <- LSD.test(t, "chem_drop")
out2020$groups
pcontrol_20_export <-out2020$groups
pcontrol_20_export <- 
  tibble::rownames_to_column(pcontrol_20_export, "chem_drop")#turn the row labels into a column


#2021 percent control
t <- aov(Pcontrol21~chem_drop, data=combine_ag_cover)
summary(t)
out2021 <- LSD.test(t, "chem_drop")
out2021$groups
pcontrol_21_export <-out2021$groups
pcontrol_21_export <- 
  tibble::rownames_to_column(pcontrol_21_export, "chem_drop")#turn the row labels into a column




##########################
#export files 
write.csv(exportable, "ag_cover_control_export.csv", row.names = FALSE)
write.csv(pcontrol_20_export, "LSDpcontrol_20_export.csv", row.names = FALSE)
write.csv(pcontrol_21_export, "LSDpcontrol_21_export.csv", row.names = FALSE)





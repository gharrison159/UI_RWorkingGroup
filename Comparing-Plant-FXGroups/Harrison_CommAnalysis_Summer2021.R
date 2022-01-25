---
title: "Harrison plant community analysis (2021 summer data)"
author: "Georgia Harrison"
date:"Oct 28, 2021"
output: html_document
---
########
#big goals:
#1. % weed control comparison between same plant community types 
#2. compare treated and untreated  richness and diversity between plant community types 
#3. Compare level of invasives between treated & untreated
#######



#################################
#load in packages

library(dplyr)
library(vegan)
library(agricolae) #for LSD
library(ggplot2)
library(RColorBrewer) # for graph color palettes
library(tidyr)

#set working directory
setwd("~/Research/Summer2021")



#################################
### import files
#files:
plant_traits <-read.csv("PlantTraits.csv", header = T)
#species list with plant habit 
#code is USDA plant species code
#habits are PGR, AGR, AFO, PFO, and PSH
#FXGroup is combination of life form, natiity, longevity, size and regen strategy


#Count data in wide format
foliar_count <- read.csv("FoliarCover_LPICounts.csv", header = T)
#PlotID and transect by species matrix
#within matrix: count of hits for each transect


plotmeta <- read.csv("Plot_meta.csv", header = T)
#treatment information for all the plots



#################################
#add in plot metadata for each column 
head(plotmeta,2)
head(foliar_count, 1)
try1 = merge(foliar_count, plotmeta, by.x = "PLOTID", by.y="PlotID")
head(try1,1)

#reorder columns so that metadata is first, species are second 
which(colnames(try1)=="Num") #89
which(colnames(try1)=="old_point") #93
foliar_count <- try1[, c(1:2, 89:93, 3:88)]



#####################
#create a species only matrix for ALL SPECIES!
head(foliar_count, 1)
meta_cols = c("PLOTID", "TRANSECT", "Num", "Trt", "SH_GR", "pasture", "old_point") #these are the plot metadata columns we would like to retain through the calculations

which(colnames(foliar_count)=="ACMI2") #8
which(colnames(foliar_count)=="ZIPA2") #93
total.abundance.matrix <- foliar_count[, 8:93]
#total.abundance.matrix <- lapply(total.abundance.matrix, as.numeric)


i_wide <- foliar_count[,meta_cols]
i_wide$AllRichness <- rowSums(total.abundance.matrix>0) #sp richness is a count of how many species are present in a plot
i_wide$AllShannon <- diversity(total.abundance.matrix) # shannon is default
i_wide$AllSimpson <- diversity(total.abundance.matrix, index = "simpson") #simpson diversity index if you wanted that
i_wide$AllCount <- rowSums(total.abundance.matrix) #this is summing all of the cover values for each row, remember that each row is a plot

#################################
#lets add in species richness and diversity by functional groups too! 

#filter out the plant list by functional group traits
#PERENNIAL FORBS
codes_PF <- plant_traits %>%
  filter(FXGroup == "FO.NAT.P"|
          FXGroup == "FO.Unk.P" )
PF_sp_list <- codes_PF$code # a string containing al the PF species 
PF.abundance.matrix <- select(foliar_count, any_of(PF_sp_list))

i_wide$PFRichness <- rowSums(PF.abundance.matrix>0) #sp richness is a count of how many species are present in a plot
i_wide$PFShannon <- diversity(PF.abundance.matrix) # shannon is default
i_wide$PFSimpson <- diversity(PF.abundance.matrix, index = "simpson") #simpson diversity index if you wanted that
i_wide$PFCount <- rowSums(PF.abundance.matrix) #this is summing all of the cover values for each row, remember that each row is a plot

#######
# SHRUBS
codes_SH <- plant_traits %>%
  filter(FXGroup == "SH.NAT.P.SE"|
           FXGroup == "SH.NAT.P.SP" )
SH_sp_list <- codes_SH$code # a string containing al the PF species 
SH.abundance.matrix <- select(foliar_count, any_of(SH_sp_list))

i_wide$SHRichness <- rowSums(SH.abundance.matrix>0) #sp richness is a count of how many species are present in a plot
i_wide$SHShannon <- diversity(SH.abundance.matrix) # shannon is default
i_wide$SHSimpson <- diversity(SH.abundance.matrix, index = "simpson") #simpson diversity index if you wanted that
i_wide$SHCount <- rowSums(SH.abundance.matrix) #this is summing all of the cover values for each row, remember that each row is a plot

######
#ANNUAL FORBS
codes_AF <- plant_traits %>%
  filter(FXGroup == "FO.NAT.A"|
           FXGroup == "FO.INV.A" )
AF_sp_list <- codes_AF$code # a string containing al the PF species 
AF.abundance.matrix <- select(foliar_count, any_of(AF_sp_list))

i_wide$AFRichness <- rowSums(AF.abundance.matrix>0) #sp richness is a count of how many species are present in a plot
i_wide$AFShannon <- diversity(AF.abundance.matrix) # shannon is default
i_wide$AFSimpson <- diversity(AF.abundance.matrix, index = "simpson") #simpson diversity index if you wanted that
i_wide$AFCount <- rowSums(AF.abundance.matrix) #this is summing all of the cover values for each row, remember that each row is a plot

########
#PERENNIAL GRASSES
codes_PG <- plant_traits %>%
  filter(FXGroup == "GR.NAT.P.LG."|
           FXGroup == "GR.INV.P.SM." |
           FXGroup == "GR.NAT.P.SM.")
PG_sp_list <- codes_PG$code # a string containing al the PF species 
PG.abundance.matrix <- select(foliar_count, any_of(PG_sp_list))

i_wide$PGRichness <- rowSums(PG.abundance.matrix>0) #sp richness is a count of how many species are present in a plot
i_wide$PGShannon <- diversity(PG.abundance.matrix) # shannon is default
i_wide$PGSimpson <- diversity(PG.abundance.matrix, index = "simpson") #simpson diversity index if you wanted that
i_wide$PGCount <- rowSums(PG.abundance.matrix) #this is summing all of the cover values for each row, remember that each row is a plot



#########
#ANNUAL GRASSES
codes_AG <- plant_traits %>%
  filter(FXGroup == "GR.INV.A")
AG_sp_list <- codes_AG$code # a string containing al the PF species 
AG.abundance.matrix <- select(foliar_count, any_of(AG_sp_list))

i_wide$AGRichness <- rowSums(AG.abundance.matrix>0) #sp richness is a count of how many species are present in a plot
i_wide$AGShannon <- diversity(AG.abundance.matrix) # shannon is default
i_wide$AGSimpson <- diversity(AG.abundance.matrix, index = "simpson") #simpson diversity index if you wanted that
i_wide$AGCount <- rowSums(AG.abundance.matrix) #this is summing all of the cover values for each row, remember that each row is a plot


head(AG.abundance.matrix,1)
ag_cover <- foliar_count[,meta_cols]
ag_cover$BRJA= (AG.abundance.matrix$BRJA/60)*100
ag_cover$BRTE= (AG.abundance.matrix$BRTE/60)*100
ag_cover$total_ag_cover = (ag_cover$BRJA + ag_cover$BRTE)

head(ag_cover)


###calculate Percent control of annual grasses
#formula: (1-(%weed in treatment / % weed in untreated check))*100
#first we need to know the average % weed cover in control plots
ag_cover %>%
  group_by(Trt)%>%
  summarize(mean=mean(total_ag_cover))
#average cover of AG in control plots is 10.2, is 4.41 in treatment plots

#create a column of P control
ag_cover$Pcontrol <- 
  (1-(ag_cover$total_ag_cover)/10.2)*100





#########
#percent control of annual grasses
t <- aov(Pcontrol~Trt, data=trt_only_rich)
summary(t)
out1 <- LSD.test(t, "Trt")
out1


p <- ggplot(ag_cover, aes(x=SH_GR, y=Pcontrol))+
  geom_boxplot(aes(fill = Trt))+
  scale_fill_brewer(palette="RdBu")+
  xlab("Community type") + ylab("% control of AG") +
  labs(fill = "Treatment or control")+
  theme_classic()
plot(p)


t <- aov(total_ag_cover~SH_GR*Trt, data=ag_cover)
summary(t)
TukeyHSD(t)
#out1 <- LSD.test(t, "Trt")
#out1

p1 <- ggplot(ag_cover, aes(x=SH_GR, y=total_ag_cover))+
  geom_boxplot(aes(fill = Trt))+
  scale_fill_brewer(palette="RdBu", 
                    labels = c("Control", "Treatment"))+
  xlab("Community type") + 
  ylab("Annual grass cover (%)") +
  labs(fill = "Treatment or control")+
  theme_classic()
p1 <- p1 +
  theme(legend.position = "top")

plot(p1)


#######
#figures for species richness

theme_set(
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.text = element_text(size=11, colour = "black"),  
        axis.line = element_line(colour = "black"), axis.title = element_text(size=16, colour = "black"),
        legend.title = element_text(size = 16, colour = "black"), legend.text = element_text(size=16, colour= "black")))

#total species richness
all <- ggplot(i_wide, aes(x=SH_GR, y=AllRichness, fill=factor(Trt)))
all <- all + geom_boxplot()+theme_classic()
all <- all + labs(y="Species Richness", x="Plant Community", title="Total species richness")
all <- all + scale_fill_brewer(palette="Dark2", name="Treatment", labels = c("Control", "Treatment"))
plot(all)

#total diversity


all <- ggplot(i_wide, aes(x=SH_GR, y=AllShannon, fill=factor(Trt)))
all <- all + geom_boxplot()+theme_classic()
all <- all + labs(y="Species Diversity", x="Plant Community", title="Total diversity")
all <- all + scale_fill_brewer(palette="Dark2", name="Treatment", labels = c("Control", "Treatment"))
all <- all + ylim(1,2.5)
plot(all)





####### 
# stacked bar graphs 


#need to go from wide to long format
head(i_wide, 2)

columns_rich = c("PGRichness", "AFRichness", "AGRichness", "SHRichness", "PFRichness")
rich_longer = pivot_longer(i_wide, columns_rich, 
                         names_to = "FXGroup", 
                         values_to = "Richness")

p2 <- ggplot(rich_longer, aes(x=SH_GR, y=Richness, fill=FXGroup))
p2 <- p2 + theme_bw()+ 
  geom_bar(position="stack", stat="identity")
p2 <- p2 + facet_grid(~Trt) +
  scale_fill_brewer(palette="Dark2")
plot(p2)






## all of this is with transect data - we need to average across the plot
i_wide_plot <- i_wide %>% 
  group_by(PLOTID) %>%
  summarise(mean(AllRichness))













#################################
# #convert from count to cover 
# #first need to go from wide to long 
# head(foliar_count)
# count_long = pivot_longer(foliar_count, ACMI2:ZIPA2,
#                           names_to = "species", values_to = "count")
# head(count_long) #it worked
# 
# 
# ##transform count data to percent cover
# #count_long$cover = (count_long$count / 50)
# #count_long <- filter(count_long, rowSums(is.na(count_long)) != ncol(count_long))  #remove NAs
# 
# 
# ##go back to wide
# count_long$Unique_ID <- seq.int(nrow(count_long)) #creates a unique identifier for each row
# head(count_long)
# 
# head(foliar_count,1)

# all_wide <- pivot_wider(count_long, id_cols = meta_cols, 
#                               names_from = species,
#                               values_from = count)
# 
# 
# head(PF.abundance.matrix, 1)
# 
# PF.abundance.matrix <- foliar_count[, foliar_count(PF_sp_list)]
# 
# 
# PF.abundance.matrix <- 
#   foliar_count %>%
#   select()
# 
# 
# ?select()
# 
# 
# 
# ###################
# #add in plant traits column for each species 
# 
# head(plant_traits)
# # add in the FXGroup
# 
# try = merge(count_long, plant_traits, by.x="species", by.y="code", copy = TRUE)
# #need to set copy = true so that you can fill in multiple species repeated observations
# head(try, 1) 
# #it worked but we need to trim to the columns we actually care about
# which(colnames(try)=="FXGroup") #24
# keep_cols = c(1:9, 17:24)
# count_long =try[,keep_cols] 
# #this is the updated long datasets, now with FXGroup info for each species!
# head(count_long)
# 
# 
# 
# #######
# #now we want to subset the data by functional group 
# count_long$Unique_ID <- seq.int(nrow(count_long)) #creates a unique identifier for each row
# head(count_long)
# 
# #here are all the functional groups:
# unique(count_long$FXGroup)
# 
# #for now lets just do the normal functional groups 
# SH <- count_long[count_long$FXGroup == "SH.NAT.P.SE" |
#                  count_long$FXGroup == "SH.NAT.P.SP",]
# AF <- count_long[count_long$FXGroup == "FO.NAT.A" |
#                  count_long$FXGroup == "FO.INV.A",]
# PF <- count_long[count_long$FXGroup == "FO.NAT.P" |
#                  count_long$FXGroup == "FO.Unk.P",]
# PG <- count_long[count_long$FXGroup == "GR.NAT.P.LG." |
#                    count_long$FXGroup == "GR.INV.P.SM."|
#                    count_long$FXGroup == "GR.NAT.P.SM.",]
# AG <- count_long[count_long$FXGroup == "GR.INV.A",]
# dead <- count_long[count_long$FXGroup == c(""),]
# 
# 
# #remove NAs
# SH <- filter(SH, rowSums(is.na(SH)) != ncol(SH)) 
# AF <- filter(AF, rowSums(is.na(AF)) != ncol(AF)) 
# PF <- filter(PF, rowSums(is.na(PF)) != ncol(PF)) 
# PG <- filter(PG, rowSums(is.na(PG)) != ncol(PG)) 
# AG <- filter(AG, rowSums(is.na(AG)) != ncol(AG)) 
# #dead <- filter(dead, rowSums(is.na(dead)) != ncol(dead)) 
# 
# 
# meta_cols1 = c("PLOTID", "TRANSECT", "Num", "Trt", "SH_GR", "pasture", "Unique_ID") #these are the plot metadata columns we would like to retain through the calculations
# 
# meta_cols2 = c("PLOTID", "TRANSECT", "Num", "Trt", "SH_GR", "pasture") #these are the plot metadata columns we would like to retain through the calculations
# 
# ##pivot all to wide format
# SH_wide <- pivot_wider(SH, id_cols = meta_cols2, 
#                        names_from = species, values_from = count)
# AF_wide <- pivot_wider(AF, id_cols = meta_cols, 
#                        names_from = species, values_from = count)
# PF_wide <- pivot_wider(PF, id_cols = meta_cols, 
#                        names_from = species, values_from = count)
# PG_wide <- pivot_wider(PG, id_cols = meta_cols, 
#                        names_from = species, values_from = count)
# AG_wide <- pivot_wider(AG, id_cols = meta_cols, 
#                        names_from = species, values_from = count)
# 
#
# 
# #create a species only matrix 
# head(SH_wide, 1)
# which(colnames(SH_wide)=="ARAR8") #8
# which(colnames(SH_wide)=="TECA2") #13
# shrub.abundance.matrix <- SH_wide[, 8:13]
# 
# 
# #calculate richness, diversity, and total cover 
# i_wide <- SH_wide[,meta_cols2]
# i_wide$ShrubRichness <- rowSums(shrub.abundance.matrix>0) #sp richness is a count of how many species are present in a plot
# i_wide$ShrubShannon <- diversity(shrub.abundance.matrix) # shannon is default
# i_wide$ShrubSimpson <- diversity(shrub.abundance.matrix, index = "simpson") #simpson diversity index if you wanted that
# i_wide$ShrubCover <- rowSums(shrub.abundance.matrix) #this is summing all of the cover values for each row, remember that each row is a plot
# 
# 

# 
# #Create matrix of only species data - no plot labels or bare ground measures
# #abundance matrix that is just all the plants
# #all plants from each year
# which(colnames(foliar_count)=="ACMI2") #3
# which(colnames(foliar_count)=="ZIPA2") #88
# 
# abun.2021 <- foliar_count[,3:88]
# 
# #get the N out
# which(colnames(foliar_count)=="N") #75
# bare.ground <- foliar_count[,75]
# 
# 
# #get the litter out
# which(colnames(foliar_count)=="L") #53
# which(colnames(foliar_count)=="WL") #87
# litter <- foliar_count[,53] + foliar_count[,87] #does not work
# 
# #get the dead stuff out too 
# which(colnames(foliar_count)=="DF") #32
# which(colnames(foliar_count)=="DG") #33
# which(colnames(foliar_count)=="DS") #34
# 
# dead <- foliar_count[,32:34]
# 
# plants.only <- foliar_count[, 3:31, 35:52, 54:74, 76:86, 87:88]
# plants.only






















#rangeland analsysis platform data 
RAP <-read.csv("RAP_cover.csv", header = T)
head(RAP)


ag <- ggplot(RAP, aes(x=year, y=AFGC))
ag <- ag + geom_point()
ag <- ag + geom_smooth(color = 'red', alpha = .5)
pg <- pg + theme_classic()
ag <- ag + labs(x="Year", y="Annual forb & grass cover")
ag <- ag + facet_wrap(~Plot_type) +
  scale_fill_brewer(palette="Dark2")
plot(ag)

pg <- ggplot(RAP, aes(x=year, y=PFGC))
pg <- pg + geom_point()
pg <- pg + geom_smooth(color = 'red', alpha = .5)
pg <- pg + theme_classic()
pg <- pg + labs(x="Year", y="Perennial forb & grass cover")
pg <- pg + facet_wrap(~Plot_type) +
  scale_fill_brewer(palette="Dark2")
plot(pg)

sh <- ggplot(RAP, aes(x=year, y=SHR))
sh <- sh + geom_point()
sh <- sh + geom_smooth(color = 'red', alpha = .5)
sh <- sh + theme_classic()
sh <- sh + labs(x="Year", y="Shrub cover")
sh <- sh + facet_wrap(~Plot_type) +
  scale_fill_brewer(palette="Dark2")
plot(sh)



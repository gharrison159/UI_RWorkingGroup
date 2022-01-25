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
library(tidyverse)


#set working directory
#setwd("~/Research/Summer2021")
#setwd("C:/Users/harr4718/OneDrive - University of Idaho/Summer2021")
setwd("C:/Users/georg/OneDrive - University of Idaho/Summer2021")

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


#get special info for each ag species
head(AG.abundance.matrix,1)
ag_cover <- foliar_count[,meta_cols]
ag_cover$BRJA= (AG.abundance.matrix$BRJA/60)*100
ag_cover$BRTE= (AG.abundance.matrix$BRTE/60)*100
ag_cover$total_ag_cover = (ag_cover$BRJA + ag_cover$BRTE)

head(ag_cover)


###calculate Percent control of annual grasses
#formula: (1-(%weed in treatment / % weed in untreated check))*100
#first we need to know the average % weed cover in control plots

#average for all control plots
ag_cover %>%
  group_by(Trt)%>%
  summarize(mean=mean(total_ag_cover))
#average cover of AG in control plots is 10.2, is 4.41 in treatment plots

#average of control plots by plant community type
ag_cover %>%
  group_by(Trt, SH_GR)%>%
  summarize(mean=mean(total_ag_cover))

#create a column of P control
ag_cover$Pcontrol <- 
  (1-(ag_cover$total_ag_cover)/10.2)*100

write.csv(ag_cover, "ag_cover_export.csv")

### create sub datasets for treatment only and control only 

head(i_wide)
trt_only_rich <-
  i_wide %>% 
  filter(Trt == "T")
head(trt_only_rich)

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

##cover of annual grasses
t <- aov(total_ag_cover~SH_GR*Trt, data=ag_cover)
summary(t)
TukeyHSD(t)
#out1 <- LSD.test(t, "Trt")
#out1



theme_set(
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text = element_text(size=16, colour = "black"),  
        axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=24, colour = "black"),
        legend.title = element_text(size = 24, colour = "black"), 
        legend.text = element_text(size=16, colour= "black")))


p1 <- ggplot(ag_cover, aes(x=SH_GR, y=total_ag_cover))+
  geom_boxplot(aes(fill = Trt))+
  scale_fill_brewer(palette="Dark2", 
                    labels = c("Control", "Treatment"))+
  xlab("Community type") + 
  ylab("Annual grass cover (%)") +
  labs(fill = " ")+
  scale_x_discrete(labels=c("HSHG" = "High Shrub, \n High Perennial", 
                            "HSLG" = "High Shrub, \n Low Perennial",
                            "LSHG" = "Low Shrub,\n High Perennial",
                            "LSLG" = "Low Shrub,\n Low Perennial"))
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
aov_rich = aov(AllRichness~SH_GR, data  = i_wide)
TukeyHSD(aov_rich)
summary(aov_rich)

#total diversity
all <- ggplot(i_wide, aes(x=SH_GR, y=AllShannon, fill=factor(Trt)))
all <- all + geom_boxplot()+theme_classic()
all <- all + labs(y="Species Diversity", x="Plant Community", title="Total diversity")
all <- all + scale_fill_brewer(palette="Dark2", name="Treatment", labels = c("Control", "Treatment"))
all <- all + ylim(1,2.5)
plot(all)
aov_div = aov(AllShannon~Trt, data  = i_wide)
summary(aov_div)

#total cover
all <- ggplot(i_wide, aes(x=SH_GR, y=AllCount, fill=factor(Trt)))
all <- all + geom_boxplot()+theme_classic()
all <- all + labs(y="Species Diversity", x="Plant Community", title="Total diversity")
all <- all + scale_fill_brewer(palette="Dark2", 
                               name="Treatment", 
                               labels = c("Control", "Treatment"))
plot(all)

aov_div = aov(AllCount~SH_GR*Trt, data  = i_wide)
summary(aov_div)
TukeyHSD(aov_div)


####### 
# stacked bar graphs 

####richness
#need to go from wide to long format
head(i_wide, 2)

#need to summarize average richness by plant community type
#not including annual grasses
columns_rich = c("PGRichness", "AFRichness", "SHRichness", "PFRichness")
rich_longer = pivot_longer(i_wide, columns_rich, 
                           names_to = "FXGroup", 
                           values_to = "Richness")
rich_longer = select(rich_longer, c('PLOTID', 'SH_GR', 'Trt',
                                    'FXGroup', 'Richness'))

head(rich_longer, 2)

rich_long_byplot <- 
  rich_longer %>%
  group_by(SH_GR, FXGroup, Trt) %>%
  summarize(mean_plotRich=mean(Richness),
            sd_plotRich=sd(Richness))

rich_long_byplot

labels_pfc = c("Annual forbs", "Perennial forbs", "Perennial grasses", "Shrubs")

#create labels for facet
plot.labs <- c("High Shrub, \n High Perennial", 
               "High Shrub, \n Low Perennial",
               "Low Shrub, \n High Perennial", 
               "Low Shrub, \n Low Perennial")
names(plot.labs) <- c("HSHG", "HSLG", "LSHG", "LSLG")
library(grid)

theme_set(
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text = element_text(size=16, colour = "black"),  
        axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=24, colour = "black"),
        legend.title = element_text(size = 16, colour = "black"), 
        legend.text = element_text(size=16, colour= "black"),
        strip.text.x = element_text(size = 14, colour = "black"),
        strip.background = element_rect(fill = "white"),
        legend.position = "none"
        ))

p2 <- ggplot(rich_long_byplot, aes(x=Trt, y=mean_plotRich, fill=FXGroup))
p2 <- p2 + geom_bar(position="stack", stat="identity", colour = 'black')
p2 <- p2 + scale_fill_brewer(palette="Dark2", name = "Plant Functional Group",
                             labels = labels_pfc)
p2 <- p2 + facet_grid(~SH_GR, 
                      labeller = labeller(SH_GR = plot.labs))
p2 <- p2 + xlab("Treatment Condition") + 
  ylab("Species Richnes") + 
  scale_x_discrete(labels=c("C" = "Untrt", 
                          "T" = "Trt")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 15)) #gets rid of space at bottom
#p2 <- theme(legend.position = "none")
plot(p2)


# #create labels for the functional groups
# p2 <- ggplot(rich_long_byplot, aes(x=SH_GR, y=mean_plotRich, fill=FXGroup))
# p2 <- p2 + theme_bw()+ 
#   geom_bar(position="stack", stat="identity")
# p2 <- p2 +  scale_fill_brewer(palette="Paired", labels = labels_pfc )
# p2 <- p2+ ylim(0, 15)
# p2 <- p2 + facet_grid(~Trt)
# p2 <- p2 + xlab("Community type") + 
#   ylab("Species Richnes") + labs(fill = "Plant Functional Group")
# plot(p2)
# 




#cover

#need to go from wide to long format
head(i_wide, 2)

#need to summarize average richness by plant community type
columns_count = c("PGCount", "AFCount", "AGCount", "SHCount", "PFCount")
count_longer = pivot_longer(i_wide, columns_count, 
                           names_to = "FXGroup", 
                           values_to = "Count")
count_long_byplot <- 
  count_longer %>%
  group_by(SH_GR, FXGroup, Trt) %>%
  summarize(mean_plotCount=mean(Count)) %>%
  mutate(mean_plotCover = (mean_plotCount/180)*100)

head(count_long_byplot)
labels_pfc = c("Annual forbs", "Annual grasses", "Perennial forbs", "Perennial grasses", "Shrubs")


#create labels for facet
plot.labs <- c("High Shrub, \n High Perennial", 
               "High Shrub, \n Low Perennial",
               "Low Shrub, \n High Perennial", 
               "Low Shrub, \n Low Perennial")
names(plot.labs) <- c("HSHG", "HSLG", "LSHG", "LSLG")
library(grid)

theme_set(
  theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_blank(), 
          axis.text = element_text(size=16, colour = "black"),  
          axis.line = element_line(colour = "black"), 
          axis.title = element_text(size=24, colour = "black"),
          legend.title = element_text(size = 16, colour = "black"), 
          legend.text = element_text(size=16, colour= "black"),
          strip.text.x = element_text(size = 14, colour = "black"),
          strip.background = element_rect(fill = "white"),
          legend.position = "none"
    ))


p2 <- ggplot(count_long_byplot, aes(x=Trt, y=mean_plotCover, fill=FXGroup))
p2 <- p2 + geom_bar(position="stack", stat="identity", colour = 'black')
p2 <- p2 + scale_fill_brewer(palette="Paired", name = "Plant Functional Group",
                             labels = labels_pfc)
p2 <- p2 + facet_grid(~SH_GR, 
                      labeller = labeller(SH_GR = plot.labs))
p2 <- p2 + xlab("Treatment Condition") + 
  ylab("Cover (%)") + 
  scale_x_discrete(labels=c("C" = "Untrt", 
                            "T" = "Trt")) 
p2 <- p2 + scale_y_continuous(expand = c(0, 0), limits = c(0, 90)) #gets rid of space at bottom
p2 <- p2
plot(p2)

# 
# #create labels for the functional groups
# p2 <- ggplot(count_long_byplot, aes(x=SH_GR, y=mean_plotCover, fill=FXGroup))
# p2 <- p2 + theme_bw()+
#   geom_bar(position="stack", stat="identity")
# p2 <- p2 +  scale_fill_brewer(palette="Paired", labels = labels_pfc )
# p2 <- p2 + facet_grid(~Trt)
# p2 <- p2 + xlab("Community type") +
#   ylab("Species Richnes") + labs(fill = "Plant Functional Group")
# plot(p2)
# 













##########################
##### create shrub and invaded index

#shrub index 
i_wide$SI = (i_wide$SHCount)/(i_wide$AllCount)

######
#invasive herbs
unique(plant_traits$FXGroup)
       
codes_INV <- plant_traits %>%
  filter(FXGroup == "GR.INV.P.SM."|
           FXGroup == "FO.INV.A" |
           FXGroup == "GR.INV.A")
INV_sp_list <- codes_INV$code # a string containing al the PF species 
INV.abundance.matrix <- select(foliar_count, any_of(INV_sp_list))

i_wide$InvRichness <- rowSums(INV.abundance.matrix>0) #sp richness is a count of how many species are present in a plot
#i_wide$AFShannon <- diversity(AF.abundance.matrix) # shannon is default
#i_wide$AFSimpson <- diversity(AF.abundance.matrix, index = "simpson") #simpson diversity index if you wanted that
i_wide$InvCount <- rowSums(INV.abundance.matrix) #this is summing all of the cover values for each row, remember that each row is a plot

i_wide$Herb.TOT <- i_wide$AllCount - i_wide$SHCount #total herbaceous
#invasive index
i_wide$II <- (i_wide$InvCount)/(i_wide$Herb.TOT) #invadedness index ("I index")


head(i_wide,2)


theme_set(
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.text = element_text(size=16, colour = "black"),  
        axis.line = element_line(colour = "black"), 
        axis.title = element_text(size=24, colour = "black"),
        legend.title = element_text(size = 14, colour = "black"), 
        legend.text = element_text(size=16, colour= "black")))

#create labels for facet
labels_pc <- c("HSHP", "HSLP", "LSHP", "LSLP")

#plot points on shrub and invasive axis 
p2 <- ggplot(i_wide, aes(x=II, y=SI, color=SH_GR))
p2 <- p2 + geom_point(size = 3)
p2 <- p2 + scale_fill_brewer(palette="Dark2", 
                             labels = labels_pc )
p2 <- p2 + ylab("Shrub Index") +
  xlab("Invasive Index") + labs(color = "Plant Community")
p2 <- p2 + theme(legend.position = "top",
                 legend.key=element_blank())
plot(p2)
#add 95% CI ellipse 
p3 <- p2 + stat_ellipse(size=1, level = 0.95)
plot(p3)



######## export PFC cover by plot to compare to remotely sensed measure of cover

head(i_wide)
#bring in percent cover data 
PCover = read.csv("FoliarCover_PCover.csv", header = T)
head(PCover)

#seperate by plant functional groups 
meta_cols = c("PLOTID", "TRANSECT")
cover_wide <- PCover[,meta_cols]











############# NMDS
LPICOUNT = read.csv("FoliarCover_LPICounts.csv", header = T)
head(LPICOUNT)

#################################
#add in plot metadata for each column 
head(plotmeta,2)
head(LPICOUNT, 1)
try1 = merge(LPICOUNT, plotmeta, by.x = "PLOTID", by.y="PlotID")
head(try1,1)

#reorder columns so that metadata is first, species are second 
which(colnames(try1)=="Num") #89
which(colnames(try1)=="old_point") #93
count_with_meta <- try1[, c(1:2, 89:93, 3:88)]

#community data only
head(count_with_meta,2)



which(colnames(count_with_meta)=="ACMI2") #8
which(colnames(count_with_meta)=="ZIPA2") #93
com <- count_with_meta[, 8:93] #community data - all species
m_com = as.matrix(com) #convert com to a matrix


env <- count_with_meta[,c("SH_GR", "Trt")] 
env$SI <- i_wide$SI
env$II <- i_wide$II



#nmds code - same from above 
set.seed(123)
en2nmds = metaMDS(m_com, k=3, distance = "bray")
stressplot(en2nmds)
en2nmds

en2 = envfit(en2nmds, env, permutations = 999, na.rm=TRUE)
en2

e2.data.scores = as.data.frame(scores(en2nmds))
e2.data.scores$PlantComm = count_with_meta$SH_GR
e2.data.scores$Treatment = count_with_meta$Trt
en_coord_cont = as.data.frame(scores(en2, "vectors")) * ordiArrowMul(en2)
en_coord_cat = as.data.frame(scores(en2, "factors")) * ordiArrowMul(en2)


theme_set(
  theme(axis.title = element_text(size = 10, face = "bold", colour = "black"),
        panel.background = element_blank(), 
        panel.border = element_rect(fill = NA, colour = "black"), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        legend.key = element_blank(), 
        legend.title = element_text(size = 10, face = "bold", colour = "black"),
        legend.text = element_text(size = 9, colour = "black")))


#just dots - colored by plant Community
gg <- ggplot(data = e2.data.scores, aes(x = NMDS1, y = NMDS2)) 
gg <- gg + geom_point(data = e2.data.scores, aes(colour = PlantComm), size = 2, alpha = 0.6) 
#+ scale_colour_manual(values = c("orange", "steelblue"))
gg <- gg + labs(colour = "Plant Community")
gg

#by community with elises 
gg1 <- ggplot(data = e2.data.scores, aes(x = NMDS1, y = NMDS2, col=PlantComm)) 
gg1 <- gg1 + geom_point(data = e2.data.scores, aes(colour = PlantComm), size = 2, alpha = 0.6) 
  #scale_colour_manual(values = c("orange", "steelblue")) 
gg1 <- gg1 + stat_ellipse(size=1.5)
gg1 <- gg1+ labs(colour = "Plant Community")
gg1

#just dots - colored by treatment
gg <- ggplot(data = e2.data.scores, aes(x = NMDS1, y = NMDS2)) 
gg <- gg + geom_point(data = e2.data.scores, aes(colour = Treatment), size = 2, alpha = 0.6) 
        + scale_colour_manual(values = c("orange", "steelblue"))
gg <- gg + labs(colour = "Treatment")
gg

#by community with elises 
gg1 <- ggplot(data = e2.data.scores, aes(x = NMDS1, y = NMDS2, col=Treatment)) 
gg1 <- gg1 + geom_point(data = e2.data.scores, aes(colour = Treatment), size = 2, alpha = 0.6) 
 scale_colour_manual(values = c("orange", "steelblue")) 
gg1 <- gg1 + stat_ellipse(size=1.5)
gg1 <- gg1+ labs(colour = "Treatment")
gg1

##could do just high shrub types and see how treatment impacts this
#check out what type of metric people use for soil texture 
#(i.e. just the clay components, just the sand component)




#add in environmental vectors

#only vectors
gg = ggplot(data = e2.data.scores, aes(x = NMDS1, y = NMDS2))
gg = gg + geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                       data = en_coord_cont, size =1, alpha = 0.7, colour = "black") 
gg = gg + geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), 
                    check_overlap = TRUE, colour = "black", 
                    fontface = "bold", label = row.names(en_coord_cont), vjust="inward")
gg




#add vectors to gg1 - by site
gg3 <- gg1 + geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
                          data = en_coord_cont, size =1, alpha = 0.7, colour = "black") 
gg3 <- gg3 + geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), 
                       check_overlap = TRUE, colour = "black", 
                       fontface = "bold", label = row.names(en_coord_cont), vjust="inward")
gg3







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



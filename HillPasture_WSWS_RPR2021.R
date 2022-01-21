---
title: "Hill Pasture Richness analysis (2020 + 2021 data)"
author: "Georgia Harrison"
date:"Jan 19, 2021"
output: html_document
---
  
  
  
library(dplyr)
library(vegan)
library(agricolae) #for LSD
library(ggplot2)
library(RColorBrewer) # for graph color palettes

#################################
#files:
sp_list <-read.csv("Hill_splist_meta.csv", header = T)
#species list with plant habit 
#habits are AF, PF, PG, AG, and  S


hill19 <- read.csv("hill_2019_midpoint.csv", header = T)
hill20 <- read.csv("hill_2020_midpoint.csv", header = T)
hill21 <- read.csv("hill_2021_midpoint.csv", header = T)
#midpoint for each species and plot from 2019 and 2020 

plotmeta <- read.csv("hill_plot_metadata.csv", header = T)
#treatment information for all the plots
#   plotmeta$Transect == C is control


#########


#Create matrix of only species data - no plot labels or bare ground measures
#abundance matrix that is just all the plants
#all plants from each year
which(colnames(hill19)=="Aforb") #6
which(colnames(hill19)=="CHVI8") #27
all.abun.19 <- hill19[,6:27]
which(colnames(hill20)=="CAEX14") #6
which(colnames(hill20)=="UNK5") #72
all.abun.20 <- hill20[,6:72]
which(colnames(hill21)=="ACMI2") #8
which(colnames(hill21)=="ZIPA2") #87
all.abun.21 <- hill21[,8:87]


#bare ground
which(colnames(hill20)=="BGROUND") #5
which(colnames(hill19)=="Bground") #5
which(colnames(hill21)=="BAREGROUND") #7
bground.19 <-hill19[,5]
bground.20 <-hill20[,5]
bground.21 <-hill21[,7]


#need to separate out annual grasses

which(colnames(hill19)=="BRJA") #9
which(colnames(hill19)=="BRTE") #10
ag.abun.19 <- hill19[,9:10]

#same thing for 2020 data
which(colnames(hill20)=="BRJA") #17
which(colnames(hill20)=="BRTE") #18
ag.abun.20 <- hill20[,17:18]


which(colnames(hill21)=="BRJA") #none
which(colnames(hill21)=="BRTE") #25
ag.abun.21 <- hill21[,25]



#summarize average annual grass cover for each year within each treatment combo
#tot_ag_comp <- 

ag19_sum <- hill19[,c("Trt", "TRT_COMBO")]
ag19_sum$ag_cover_19  <- rowSums(ag.abun.19)


ag20_sum <- hill20[,c("Trt", "TRT_COMBO")]
ag20_sum$ag_cover_20  <- rowSums(ag.abun.20)

ag21_sum <- hill21[,c("Trt", "TRT_COMBO")]
ag21_sum$ag_cover_21  <- ag.abun.21



#combine 2019, 2020, and 2021 cover
combine_ag_cover <- merge(ag19_sum, ag20_sum, 
                      by.x=c("TRT_COMBO"), 
                      by.y=c("TRT_COMBO"))

combine_ag_cover <- merge(combine_ag_cover, ag21_sum, 
                          by.x=c("TRT_COMBO"), 
                          by.y=c("TRT_COMBO"))



###calculate Percent control of annual grasses
#formula: (1-(%weed in treatment / % weed in untreated check))*100
#first we need to know the average % weed cover in control plots
combine_ag_cover %>%
  group_by(TRT_COMBO)%>%
  summarize(mean=mean(ag_cover_20))
#average cover of AG in control plots is 42.9

combine_ag_cover %>%
  group_by(TRT_COMBO)%>%
  summarize(mean=mean(ag_cover_21))
#average cover of AG in control plots is 24.1


#create a column of P control 20
combine_ag_cover$Pcontrol20 <- 
  (1-(combine_ag_cover$ag_cover_20)/42.9)*100

#create a column of P control 20
combine_ag_cover$Pcontrol21 <- 
  (1-(combine_ag_cover$ag_cover_21)/24.1)*100


head(combine_ag_cover)



########analysis 
#first we need to know the average % weed cover in control plots
combine_ag_cover %>%
  group_by(TRT_COMBO)%>%
  summarize(mean=mean(ag_cover_20))
#average cover of AG in control plots is 41.2


exportable <- 
  combine_ag_cover %>%
  group_by(TRT_COMBO)%>%
  summarize(cover20=mean(ag_cover_20),
            cover21=mean(ag_cover_21),
            cover19=mean(ag_cover_19),
            control20 = mean(Pcontrol20),
            control21 = mean(Pcontrol21)
            )

#anovas and LSD
#########
#percent control of annual grasses
#2020 percent control
t <- aov(Pcontrol20~TRT_COMBO, data=combine_ag_cover)
summary(t)
out2020 <- LSD.test(t, "TRT_COMBO")
out2020$groups
pcontrol_20_export <-out2020$groups
pcontrol_20_export <- 
  tibble::rownames_to_column(pcontrol_20_export, "TRT_COMBO")#turn the row labels into a column


#2021 percent control
t <- aov(Pcontrol21~TRT_COMBO, data=combine_ag_cover)
summary(t)
out2021 <- LSD.test(t, "TRT_COMBO")
out2021$groups
pcontrol_21_export <-out2021$groups
pcontrol_21_export <- 
  tibble::rownames_to_column(pcontrol_21_export, "TRT_COMBO")#turn the row labels into a column




##########
#export files 
write.csv(exportable, "ag_cover_control_export.csv", row.names = FALSE)
write.csv(pcontrol_20_export, "LSDpcontrol_20_export.csv", row.names = FALSE)
write.csv(pcontrol_21_export, "LSDpcontrol_21_export.csv", row.names = FALSE)

















#########
#below here is from old analysis
#########



#lets make new columns for differences from 2020 to 2019
#2020 - 2019 so positive numbers are an increase after trt
combine_rich$pg_cover_diff <- combine_rich$pg_cover_20 - combine_rich$pg_cover_19
combine_rich$ag_cover_diff <- combine_rich$ag_cover_20 - combine_rich$ag_cover_19
combine_rich$af_cover_diff <- combine_rich$af_cover_20 - combine_rich$af_cover_19
combine_rich$pf_cover_diff <- combine_rich$pf_cover_20 - combine_rich$pf_cover_19

combine_rich$rich_dif <- combine_rich$All_rich_20 - combine_rich$All_rich_19
combine_rich$bground_dif <- combine_rich$bground20 - combine_rich$bground19





#####
#analysis

#need to remove control from LSD dataset
#create a subset of combined data set WITHOUT control
trt_only_rich <- 
  subset(combine_rich, Control == "Treatment")
control_only_rich <- 
  subset(combine_rich, Control == "Control")



#########
#percent control of annual grasses
t <- aov(Pcontrol~Trt, data=trt_only_rich)
summary(t)
out1 <- LSD.test(t, "Trt")
out1


p <- ggplot(trt_only_rich, aes(x=App, y=Pcontrol))+
  geom_boxplot(aes(fill = GPA))+
  scale_fill_brewer(palette="RdBu")+
  xlab("Application type") + ylab("% control of AG") +
  labs(fill = "App rate")+
  theme_classic()
p1 <- p+ facet_grid(Chem~.)
plot(p1)


#lets get some context for what the actual AG cover was on the site
#what is the average AG cover - both pre and post trt?
combine_rich %>%
  group_by(Trt)%>%
  summarize(mean20=mean(ag_cover_20),
            mean19=mean(ag_cover_19),
            meadn_agdiff = mean(ag_cover_diff)) 
#41.2 is 2020 avg
#73.2 id 2019 avg
#-32 is mean diff 

#what about average AG cover pre and post trt in only CONTROL plots
control_only_rich %>%
  group_by(Trt)%>%
  summarize(mean20=mean(ag_cover_20),
            mean19=mean(ag_cover_19),
            meadn_agdiff = mean(ag_cover_diff)) 


#what was the average annual grass cover pre-trt (control and trt plots)
combine_rich %>%
  summarize(mean19=mean(ag_cover_19)) 
#55.38





############
#is there a difference in perennial grass cover change by treatment group
t <- aov(pg_cover_diff~Trt, data=trt_only_rich)
summary(t)
out1 <- LSD.test(t, "Trt")
out1$groups
#remember that this is Pg cover in 2020 minus that in 2019
#positive values suggest higher cover in 2020 than 2019
#this means an increase in PG cover post treatment
#negative values suggest a decrease in PG cover 

#What is the control plot value? #32.8
combine_rich %>%
  group_by(Trt)%>%
  summarize(mean=mean(pg_cover_diff))

p <- ggplot(trt_only_rich, aes(x=App, y=pg_cover_diff))+
  geom_boxplot(aes(fill=GPA))+
  ylab("Change in PG cover") +
  scale_fill_brewer(palette="RdBu")+
  xlab("Application type") + 
  labs(fill = "App rate")+
  theme_classic()+
  theme(text = element_text(size = 20))
p <- p + geom_hline(yintercept = 0, color = "black")
p <- p+ facet_grid(Chem~.)
plot(p)




############
#is there a difference in species richness between treatment groups
#recall that this is sp richness without BRTE and BRJA
t <- aov(richness~Trt, data=trt_only_rich)
summary(t)
out1 <- LSD.test(t, "Trt")
out1
#9, 7, 15, 14, 8, 16

p <- ggplot(trt_only_rich, aes(x=App, y=richness))+
  geom_boxplot(aes(fill=GPA))+
  xlab("Treatement group") + ylab("Post-trt species richness") +
  scale_fill_brewer(palette="RdBu")+
  theme_classic()+
  theme(text = element_text(size = 20), legend.text = element_text(size = 20))
p <- p+ facet_grid(Chem~.)
p <- p + geom_hline(yintercept = 13, color = "black", linetype="dashed")
plot(p)

#what was species richness in control plots? #15
combine_rich %>%
  group_by(Trt)%>%
  summarize(mean=mean(richness))





#########
#is there a difference in bare ground cover by trt group
t<- aov(bground_dif~Trt, data=trt_only_rich)
summary(t)
out1 <- LSD.test(t, "Trt")
out1$groups
#What is the control plot value? ==> 13
combine_rich %>%
  group_by(Trt)%>%
  summarize(mean=mean(bground_dif))
combine_rich %>%
  group_by(Trt)%>%
  summarize(mean20=mean(bground20),
            mean19=mean(bground19))


p <- ggplot(trt_only_rich, aes(x=App, y=bground_dif))+
  geom_boxplot(aes(fill=GPA))+
  scale_fill_brewer(palette="RdBu")+
  ylab("Change in % bare ground") +
  xlab("Application type") + 
  labs(fill = "App rate")+
  theme_classic()
p <- p+ facet_grid(Chem~.)
p <- p + geom_hline(yintercept = 13, color = "black", linetype="dashed")
plot(p)




########end of figures used for presentation
#here are some others I was messing around with


######differences in AF cover 
combine_rich %>%
  group_by(Trt)%>%
  summarize(mean=mean(af_cover_diff))

p <- ggplot(trt_only_rich, aes(x=App, y=af_cover_diff))+
  geom_boxplot(aes(fill=GPA))+
  scale_fill_brewer(palette="RdBu")+
  ylab("Change in AF cover") +
  xlab("Application type") + 
  labs(fill = "App rate")+
  theme_classic()
p <- p+ facet_grid(Chem~.)
p <- p + geom_hline(yintercept = 0, color = "black")
p <- p + geom_hline(yintercept = 5.75, color = "red", linetype="dashed")
plot(p)



##differences in PG cover 
combine_rich %>%
  group_by(Trt)%>%
  summarize(mean=mean(pf_cover_diff))

p <- ggplot(trt_only_rich, aes(x=App, y=pf_cover_diff))+
  geom_boxplot(aes(fill=GPA))+
  scale_fill_brewer(palette="RdBu")+
  ylab("Change in PF cover") +
  xlab("Application type") + 
  labs(fill = "App rate")+
  theme_classic()
p <- p+ facet_grid(Chem~.)
p <- p + geom_hline(yintercept = 0, color = "black")
p <- p + geom_hline(yintercept = -5.75, color = "red", linetype="dashed")
plot(p)






#is there a difference in 2020 perennial grass cover by treatment
t <- aov(pg_cover_20~Trt, data=trt_only_rich)
summary(t)
out1 <- LSD.test(t, "Trt")
out1


#is there a difference in 2020 annual grass cover by treatment
t <- aov(ag_cover_20~Trt, data=trt_only_rich)
summary(t)
out1 <- LSD.test(t, "Trt")
out1












##########
#export files 
export_19 <- write.csv(rich_19, "2019_richness_export.csv", row.names = FALSE)
export_20 <- write.csv(rich_20, "2020_richness_export.csv", row.names = FALSE)
export_combines <- write.csv(combine_rich, "combined_export.csv", row.names = FALSE)





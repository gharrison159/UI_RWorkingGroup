---
title: "Hill Pasture Richness analysis (2020 data)"
author: "Georgia Harrison"
date:"Feb 9, 2021"
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
#midpoint for each species and plot from 2019 and 2020 

plotmeta <- read.csv("hill_plot_metadata.csv", header = T)
#treatment information for all the plots
#   plotmeta$Transect == C is control


#########


#Create matrix of only species data - no plot labels or bare ground measures
#abundance matrix that is just all the plants
#all plants from each year
which(colnames(hill19)=="Aforb") #5
which(colnames(hill19)=="CHVI8") #26
all.abun.19 <- hill19[,5:26]
which(colnames(hill20)=="CAEX14") #5
which(colnames(hill20)=="UNK5") #71
all.abun.20 <- hill20[,5:71]

#bare ground
which(colnames(hill20)=="BGROUND") #4
which(colnames(hill19)=="Bground") #4
bground.19 <-hill19[,4]
bground.20 <-hill20[,4]



#need to separate into plant habits
#data frame is ordered by plant habit groups, 
#so we need to know the columns where each habit group starts and ends
head(hill19)

#af
which(colnames(hill19)=="Aforb") #5
which(colnames(hill19)=="LASE") #7
af.abun.19 <- hill19[,5:7]
#ag
which(colnames(hill19)=="BRJA") #8
which(colnames(hill19)=="BRTE") #9
ag.abun.19 <- hill19[,8:9]
#pf
which(colnames(hill19)=="ERUM") #10
which(colnames(hill19)=="CHJU") #13
pf.abun.19 <- hill19[,10:13]
#pg
which(colnames(hill19)=="POSE") #14
which(colnames(hill19)=="Bbluegrass") #21
pg.abun.19 <- hill19[,14:21]
#s
which(colnames(hill19)=="ARTRV") #22
which(colnames(hill19)=="CHVI8") #26
s.abun.19 <- hill19[,22:26]


#same thing for 2020 data
head(hill20)

#af
which(colnames(hill20)=="CAEX14") #5
which(colnames(hill20)=="UnkThistle") #15
af.abun.20 <- hill20[,5:15]
#ag
which(colnames(hill20)=="BRJA") #16
which(colnames(hill20)=="BRTE") #17
ag.abun.20 <- hill20[,16:17]
#pf
which(colnames(hill20)=="ACMI2") #18
which(colnames(hill20)=="ZIPA2")#51
pf.abun.20 <- hill20[,18:51]
#pg
which(colnames(hill20)=="ACNE9") #51
which(colnames(hill20)=="UnkPoa") #62
pg.abun.20 <- hill20[,51:62]
#s
which(colnames(hill20)=="ARAR8") #63
which(colnames(hill20)=="UnkSage") #69
s.abun.20 <- hill20[,63:69]



#calculate species richness for all plant groups and each group
  #also adding in bare ground cover, PG total cover, and AG total cover 
rich_19 <- hill19[,c("Rep","Trt", "Plot")]
rich_19$All_rich_19<- rowSums(all.abun.19>0)
rich_19$All_shannon_19 <- diversity(all.abun.19) # shannon is default
rich_19$af_rich_19 <- rowSums(af.abun.19>0)
rich_19$af_cover_19 <- rowSums(af.abun.19)
rich_19$ag_rich_19 <- rowSums(ag.abun.19>0)
rich_19$ag_cover_19  <- rowSums(ag.abun.19)
rich_19$pf_rich_19 <- rowSums(pf.abun.19>0)
rich_19$pf_cover_19 <- rowSums(pf.abun.19)
rich_19$pg_rich_19  <- rowSums(pg.abun.19>0)
rich_19$pg_cover_19  <- rowSums(pg.abun.19)
rich_19$s_rich_19 <- rowSums(s.abun.19>0)
rich_19$bground19 <- bground.19
head(rich_19)

rich_20 <- hill20[,c("Rep","Trt", "Plot")]
rich_20$All_rich_20 <- rowSums(all.abun.20>0)
rich_20$All_shannon_20 <- diversity(all.abun.20) # shannon is default
rich_20$af_rich_20 <- rowSums(af.abun.20>0)
rich_20$af_cover_20 <- rowSums(af.abun.20)
rich_20$ag_rich_20 <- rowSums(ag.abun.20>0)
rich_20$ag_cover_20 <- rowSums(ag.abun.20)
rich_20$pf_rich_20 <- rowSums(pf.abun.20>0)
rich_20$pf_cover_20 <- rowSums(pf.abun.20>0)
rich_20$pg_rich_20 <- rowSums(pg.abun.20>0)
rich_20$pg_cover_20 <- rowSums(pg.abun.20)
rich_20$s_rich_20 <- rowSums(s.abun.20>0)
rich_20$bground20 <- bground.20
rich_20$richness <- 
  rowSums(af.abun.20>0) + rowSums(pf.abun.20>0) + 
  rowSums(pg.abun.20>0) + rowSums(s.abun.20>0)
  ##richness is species richness without BRTE and BRJA
head(rich_20)


#combine 2019 and 2020 cover and richness summary data
combine_rich <- merge(rich_19, rich_20, by.x=c("Rep", "Trt", "Plot"), by.y=c("Rep", "Trt", "Plot"))
#now lets add on the plot meta data about treatments
combine_rich <- merge(combine_rich, plotmeta, by.x=c("Rep", "Trt", "Plot"), by.y=c("Rep", "Trt", "Plot"))
#combine rich data set not contains plot metadata
combine_rich$Trt <- factor(combine_rich$Trt) #makes trt a factor
combine_rich$GPA <- factor(combine_rich$GPA) #makes GPA a factor


###calculate Percent control of annual grasses
#formula: (1-(%weed in treatment / % weed in untreated check))*100
#first we need to know the average % weed cover in control plots
combine_rich %>%
  group_by(Trt)%>%
  summarize(mean=mean(ag_cover_20))
#average cover of AG in control plots is 41.2

#create a column of P control
combine_rich$Pcontrol <- 
  (1-(combine_rich$ag_cover_20)/41.2)*100





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





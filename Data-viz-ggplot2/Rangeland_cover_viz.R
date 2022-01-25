---
title: "Harrison plant community analysis (2021 summer data)"
author: "Georgia Harrison"
date:"Oct 28, 2021"
output: html_document
---

#################################
#load in packages

library(tidyverse)
library(vegan)
library(agricolae) #for LSD
library(RColorBrewer) # for graph color palettes

#set working directory
#setwd("~/Research/Summer2021")
#setwd("~/UofI/Summer 2021")


#################################
### import files

#rangeland analsysis platform data 
RAP <-read.csv("RAP_cover.csv", header = T)
head(RAP)

#create labeler for facet grid
# New facet label names for supp variable
plot.labs <- c("High Shrub, High Perennial", "High Shrub, Low Perennial",
                  "Low Shrub, High Perennial", "Low Shrub, Low Perennial")
names(plot.labs) <- c("HSHG", "HSLG", "LSHG", "LSLG")

#annuals
ag <- ggplot(RAP, aes(x=year, y=AFGC))
ag <- ag + geom_point(alpha = .5) + theme_classic()
ag <- ag + geom_smooth(color = 'red', alpha = .5) + theme_classic()
ag <- ag + labs(x="Year", y="Cover (%)", title="Annual forbs & grasses")
ag <- ag + facet_wrap(~Plot_type,
                      labeller = labeller(Plot_type = plot.labs)) +
  scale_fill_brewer(palette="Dark2")
plot(ag)


#perennials
ag <- ggplot(RAP, aes(x=year, y=PFGC))
ag <- ag + geom_point(alpha = .5) + theme_classic()
ag <- ag + geom_smooth(color = 'red', alpha = .5) + theme_classic()
ag <- ag + labs(x="Year", y="Cover (%)", title="Perennial forbs & grasses")
ag <- ag + facet_wrap(~Plot_type,
                      labeller = labeller(Plot_type = plot.labs)) +
  scale_fill_brewer(palette="Dark2")
plot(ag)

#shrubs
ag <- ggplot(RAP, aes(x=year, y=SHR))
ag <- ag + geom_point(alpha = .5) + theme_classic()
ag <- ag + geom_smooth(color = 'red', alpha = .5) + theme_classic()
ag <- ag + labs(x="Year", y="Cover (%)", title="Shrubs")
ag <- ag + facet_wrap(~Plot_type,
                      labeller = labeller(Plot_type = plot.labs)) +
  scale_fill_brewer(palette="Dark2")
plot(ag)


#bare ground
ag <- ggplot(RAP, aes(x=year, y=BG))
ag <- ag + geom_point(alpha = .5) + theme_classic()
ag <- ag + geom_smooth(color = 'red', alpha = .5) + theme_classic()
ag <- ag + labs(x="Year", y="Cover (%)", title="Bare ground")
ag <- ag + facet_wrap(~Plot_type,
                      labeller = labeller(Plot_type = plot.labs)) +
  scale_fill_brewer(palette="Dark2")
plot(ag)


#litter
ag <- ggplot(RAP, aes(x=year, y=LTR))
ag <- ag + geom_point(alpha = .5) + theme_classic()
ag <- ag + geom_smooth(color = 'red', alpha = .5) + theme_classic()
ag <- ag + labs(x="Year", y="Cover (%)", title="Litter")
ag <- ag + facet_wrap(~Plot_type,
                      labeller = labeller(Plot_type = plot.labs)) +
  scale_fill_brewer(palette="Dark2")
plot(ag)



head(RAP)
#wide to long format
RAP_long <-
  RAP  %>%
  pivot_longer(cols = c("AFGC", 
                        "SHR", "PFGC"),
               names_to = 'PFC', 
               values_to = 'cover')

head(RAP_long)

ag <- ggplot(RAP_long, aes(x=year, y=cover, colour = PFC))
ag <- ag + geom_point(alpha = .45) 
ag <- ag + geom_smooth()
ag <- ag + scale_fill_brewer(palette="Dark2") + theme_classic()
ag <- ag + labs(x="Year", y="Cover (%)")
ag <- ag + facet_wrap(~Plot_type,
                      labeller = labeller(Plot_type = plot.labs)) 
plot(ag)



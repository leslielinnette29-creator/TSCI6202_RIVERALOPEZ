library(rio)
library(survival)
library(dplyr)
library(survminer)
library(ggsurvfit)
library(jskm)
library(superheat)
library(sf)
library(tigris)
library(tmap)
options(datatable.na.strings=c('NULL',''));
demographics <- import("output/csv/patients.csv") %>% mutate(BIRTHDATE=as.Date(BIRTHDATE), DEATHDATE=as.Date(DEATHDATE), 
                                                             timetoevent=coalesce(DEATHDATE,max(DEATHDATE, na.rm=TRUE)) - BIRTHDATE,
                                                             timetoevent=(as.numeric(timetoevent)/365.25),
                                                             censor=!is.na(DEATHDATE), 
                                                             survival=Surv(timetoevent,event=censor))
survivalmodel <-survfit(survival~STATE, demographics)
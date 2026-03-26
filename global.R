library(rio)

library(survival)

library(dplyr)

library(survminer)

library(ggsurvfit)

library(jskm)

options(datatable.na.strings=c('NULL',''));

demographics <- import("output/csv/patients.csv") %>% mutate(BIRTHDATE=as.Date(BIRTHDATE), DEATHDATE=as.Date(DEATHDATE), 
                                                             
                                                             timetoevent=coalesce(DEATHDATE,max(DEATHDATE, na.rm=TRUE)) - BIRTHDATE,
                                                             
                                                             timetoevent=(as.numeric(timetoevent)/365.25),
                                                             
                                                             censor=!is.na(DEATHDATE), 
                                                             
                                                             survival=Surv(timetoevent,censor))

survivalmodel <-survfit(survival~STATE, demographics)

View(demographics)

table(is.na(demographics$DEATHDATE), demographics$STATE)

plot(survivalmodel,col = c('red','green'))

#survivalpublication ready

survfit2(survival~STATE,demographics) %>%
  ggsurvfit() + ylab("Fraction Alive") + xlab("Time Since Birth") + scale_color_manual(values =c('red','green'))+
  add_censor_mark() +
  add_confidence_interval() +
  add_quantile() +
  add_risktable()

  
  #  scale_x_continuous(transform = )
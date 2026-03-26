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

table(is.na(demographics$DEATHDATE), demographics$STATE)

plot(survivalmodel,col = c('red','darkgreen'))

survfit2(survival~STATE, demographics) %>%
  ggsurvfit() +
  ylab("Fraction Alive")+
  xlab("Time Since Birth in Years")+
  scale_color_manual(values =c ('red','darkgreen'))

survfit2(survival~STATE, demographics) %>%
  ggsurvfit() + ylab("Fraction Alive") +
  xlab("Years Since Birth") + 
  scale_color_manual(values =c('red', 'darkgreen')) +
  add_censor_mark() +
  add_confidence_interval() +
  add_quantile() +
  add_risktable()

survfit2(Surv(timetoevent,event=censor)~STATE, data=demographics) %>%
  ggsurvplot(data=demographics)


fit<- survfit(Surv(timetoevent,event=censor)~STATE, data=demographics)

# Customized survival curves
ggsurvplot(fit, data = demographics,
           # surv.median.line = "hv", # Add medians survival
           
           # Change legends: title & labels
           legend.title = "Sex",
           legend.labs = c("Male", "Female"),
           # Add p-value and tervals
           pval = TRUE,
           
           conf.int = TRUE,
           # # Add risk table
           # risk.table = TRUE,
           # tables.height = 0.2,
           # tables.theme = theme_cleantable()
           # 
           # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
           # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
           palette = c("#E7B800", "#2E9FDF"),
           ggtheme = theme_bw() # Change ggplot2 theme
)

# Change font size, style and color
#++++++++++++++++++++++++++++++++++++
## Not run: 
# Change font size, style and color at the same time
ggsurvplot(fit, data = demographics,  main = "Survival curve",
           font.main = c(16, "bold", "darkblue"),
           font.x = c(14, "bold.italic", "red"),
           font.y = c(14, "bold.italic", "darkred"),
           font.tickslab = c(12, "plain", "darkgreen"))


##TABLES
table(demographics$RACE, demographics$ETHNICITY, demographics$GENDER) %>% prop.table %>% data.frame() #prop.table give you proportions

##Plot to compare race by age

ggplot(demographics, aes(x=RACE, y=timetoevent, fill = RACE, color=GENDER)) + #aes--> means that a particular feture in the data will be use for a particular role
  geom_boxplot(outliers = FALSE, notch = TRUE, color="darkblue")+
  geom_jitter(width = 0.1)+
  geom_violin(color="blue", alpha=0.5)

# ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) + #aes--> means that a particular feture in the data will be use for a particular role
#   geom_boxplot(outliers = FALSE, notch = TRUE)+
#   geom_jitter(aes(color=as.numeric(INCOME)),width = 0.1)+
#   geom_violin(alpha=0.5)

ggplot(demographics,aes(x=RACE, y=timetoevent, fill=RACE, color=GENDER)) +
  geom_boxplot(outliers=FALSE, notch=TRUE, color="blue", fill='pink') +
  geom_jitter(aes(color=as.numeric(INCOME)), width=0.1) +
  geom_violin (color="blue", alpha=0.5)

ggplot(demographics,aes(x=RACE, y=timetoevent, fill=RACE, color=GENDER)) +
  geom_boxplot(outliers=FALSE, notch=TRUE, color="blue", fill='pink') +
  geom_jitter(aes(color=as.numeric(INCOME)), width=0.1) +
  geom_violin (color="blue", alpha=0.5)

# ggplot(demographics, aes(x=RACE, y=timetoevent, fill=RACE, color=GENDER)) +
#   
#   geom_boxplot(outliers=FALSE, notch=TRUE, color="blue", fill='pink') +
#   
#   geom_jitter(aes(color=as.numeric(INCOME)), width=0.1) +
#   
#   geom_violin (color="blue", alpha=0.5)

ggplot(demographics,aes(x=RACE, y=timetoevent,color=GENDER)) +
  geom_boxplot(outliers=FALSE, notch=TRUE, color="black", fill="white") +
  geom_jitter(aes(color =as.numeric(INCOME)), width=0.1)

ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
  geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +
  geom_jitter(aes(color=as.numeric(timetoevent)), width=0.1) +
  scale_color_continuous(palette = c("black","white"),
                         aesthetics = "color",
                         guide = "colourbar",
                         na.value = "red",
                         type = getOption("ggplot2.continuous.colour"))

ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
  geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +
  geom_jitter(aes(color=as.numeric(timetoevent)), width=0.25, data = subset(demographics, STATE == "California")) +
  geom_jitter(color="lightpink", width=0.25, data = subset(demographics, STATE != "California")) +
  scale_color_continuous(palette = c("darkblue","lightgray"),
                         aesthetics = "color",
                         guide = "colourbar",
                         na.value = "red",
                         type = getOption("ggplot2.continuous.colour"))
#
ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +  
geom_jitter(color="red", width=0.1, data = subset(demographics, STATE != "California")) +
  scale_color_continuous(palette = c("black","white"),
                         aesthetics = "color",
                         guide = "colourbar",
                         na.value = "red",
                         type = getOption("ggplot2.continuous.colour"))+
geom_jitter(color="blue", width=0.1, data=subset(demographics,ETHNICITY=="hispanic"),shape="\u2665", size=3)

            

#
ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
  geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +  
  geom_jitter(color="red", width=0.1, data = subset(demographics, STATE != "California")) +
  scale_color_continuous(palette = c("black","white"),
                         aesthetics = "color",
                         guide = "colourbar",
                         na.value = "red",
                         type = getOption("ggplot2.continuous.colour"))+
  geom_jitter(color="blue", width=0.1, data=subset(demographics,ETHNICITY=="hispanic"),
              shape="\u2665", size=3)+
  
  facet_wrap("MARITAL")

ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
  geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +  
  geom_jitter(color="red", width=0.1) +
facet_grid(rows=vars(MARITAL),cols=vars(STATE))






subset(demographics,is.na(MARITAL)&RACE=="asian")

       pchShow <-
         function(extras = c("*",".", "o","O","0","+","-","|","%","#"),
                  cex = 3, ## good for both .Device=="postscript" and "x11"
                  col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
                  main = paste("plot symbols :  points (...  pch = *, cex =",
                               cex,")"))
         {
           nex <- length(extras)
           np  <- 26 + nex
           ipch <- 0:(np-1)
           k <- floor(sqrt(np))
           dd <- c(-1,1)/2
           rx <- dd + range(ix <- ipch %/% k)
           ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
           pch <- as.list(ipch) # list with integers & strings
           if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
           plot(rx, ry, type = "n", axes  =  FALSE, xlab = "", ylab = "", main = main)
           abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
           for(i in 1:np) {
             pc <- pch[[i]]
             ## 'col' symbols with a 'bg'-colored interior (where available) :
             points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
             if(cextext > 0)
               text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
           }
         }
       
       pchShow()

       ggplot(demographics, aes(x=INCOME, y=log(HEALTHCARE_EXPENSES))) +
         
         geom_point(color="darkgreen", size=0.5, alpha=.1) + #alpha= transparency
         
         geom_smooth(fill="blue", color="darkred", alpha=.2)+
         
         geom_smooth(method = "lm", se=FALSE)+
         
         geom_abline(slope = 1, intercept = 0, color="darkorange", linetype = 2)+
         
         theme_classic()
       
       ggplot(mtcars, aes(x=wt, y=mpg)) +
         
         geom_point(color="darkgreen", size=0.5, alpha=1) + #alpha= transparency
         
         geom_smooth(fill="blue", color="darkred", alpha=.2)+
         
         geom_smooth(method = "lm", se=FALSE)+
         
         geom_abline(slope = 1, intercept = 0, color="darkorange", linetype = 2)+
         
         theme_classic()
       
       ggplot(demographics, aes(x=HEALTHCARE_COVERAGE, y=log(HEALTHCARE_EXPENSES))) +
         
         geom_point(color="darkgreen", size=0.5, alpha=.1) + #alpha= transparency
         
         geom_smooth(fill="blue", color="darkred", alpha=.2)+
         
         geom_smooth(method = "lm", se=FALSE)+
         
         geom_abline(slope = 1, intercept = 0, color="darkorange", linetype = 2)+
         
         theme_classic()
       
#myplot0
myplot0<-list(geom_point(color="darkgreen", size=0.5, alpha=.7) , #alpha= transparency
                
                geom_smooth(fill="blue", color="darkred", alpha=.2),
                
                geom_smooth(method = "lm", se=FALSE),
                
                geom_abline(slope = 1, intercept = 0, color="darkorange", linetype = 2),
                
                theme_classic())
myplot0
ggplot(demographics, aes(x=HEALTHCARE_COVERAGE, y=log(HEALTHCARE_EXPENSES))) + myplot0
ggplot(demographics, aes(x=INCOME, y=log(HEALTHCARE_EXPENSES))) + myplot0
ggplot(mtcars, aes(x=wt, y=mpg)) + myplot0


superheat(mtcars)
superheat(mtcars,scale =TRUE )
superheat(mtcars,scale =TRUE,pretty.order.rows=TRUE)
arrange(mtcars,desc(mpg)) %>% superheat(scale=TRUE)
cor(mtcars,use =  "pairwise.complete.obs")%>% superheat(scale = FALSE)
abs(cor(mtcars,use="pairwise.complete.obs")) %>% superheat(scale=FALSE)

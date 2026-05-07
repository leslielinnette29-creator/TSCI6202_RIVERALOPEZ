#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

     ## USE JUST FOR DATA MANIPULATION (Web/dashboard)
     # Define server logic required to draw a histogram
     function(input, output, session) {
       
       
       # Per-Session App Data ----
       # global.R creates the ordinary objects.
       # server.R copies them into reactiveValues so later filters/widgets
       # can update the app's working data without changing the original objects.
       
       app_data <- reactiveValues(
         demographics = demographics,
         survivalmodel = survivalmodel
       )
       
       # Debug Button ----
       # This only matters if the optional Debug tab/button exists in ui.R.
       observeEvent(input$debug, {
         browser()
       })
       
       output$SurvivalPlot1<-renderPlot(
         survfit2(survival~STATE, demographics) %>%
           ggsurvfit() + ylab("Fraction Alive") +
           xlab("Years Since Birth") +
           scale_color_manual(values =c('red', 'darkgreen')) +
           add_censor_mark() +
           add_confidence_interval() +
           add_quantile() +
           add_risktable()
       )
       
       output$RelationshipPlot1<-renderPlot(
         ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
           geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +
           geom_jitter(aes(color=as.numeric(timetoevent)), width=0.25, data = subset(demographics, STATE == "California")) +
           geom_jitter(color="lightpink", width=0.25, data = subset(demographics, STATE != "California")) +
           scale_color_continuous(palette = c("darkblue","lightgray"),
                                  aesthetics = "color",
                                  guide = "colourbar",
                                  na.value = "red",
                                  type = getOption("ggplot2.continuous.colour"))+
           geom_jitter(color="#C11C84", width=0.1, data=subset(demographics,ETHNICITY=="hispanic"), shape="\u2665", size=3)+ #shape will change the shape of the point in the graph
           facet_wrap("MARITAL") )
       
       output$Geomap1<-renderTmap(
         {tmap_mode("view")
           tm_shape(FQHC_C2)+
             tm_polygons(fill="clinic_count")+
             tm_shape(shape_file_street_address)+
             tm_dots(fill="darkgreen", fill_alpha=0.5, size=0.2)}
       )
     }
     
     
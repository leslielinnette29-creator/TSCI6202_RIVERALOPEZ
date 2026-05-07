# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    
    
    sidebarPanel(
      tabPanel(
        title = "Overview",
        value = "overview_tab",
        
        h3("Spring 2026 Data Visualization Demonstration"),
        
        p(
          "This Shiny app demonstrates survival analysis, interactive ggplot controls, ",
          "and interactive geospatial mapping using a shared global data-prep layer."
        ),
        
        uiOutput("overviewCards"),
        
        h4("Dataset Snapshot"),
        tableOutput("overviewTable")
      ),
      # conditionalPanel(
      #   condition = "input.main_panel == 'overview_tab'",
      #   h4("Dashboard Controls"),
      #   p("Use the Survival and Relationships tabs to see examples of UI-controlled plots."),
      # ),
      
      conditionalPanel(
        condition = "input.main_panel == 'survival_tab'",
        h4("Survival Plot"),
        
        selectInput(
          inputId = "survival_group",
          label = "Group survival curves by:",
          choices = demo_group_columns,
          selected = default_survival_group
        ),
        
        checkboxGroupInput(
          inputId = "survival_layers",
          label = "Show plot layers:",
          choices = c(
            "Censor marks" = "censor",
            "Confidence interval" = "confidence",
            "Median survival line" = "quantile",
            "Risk table" = "risktable"
          ),
          selected = c("censor", "confidence", "quantile", "risktable")
        )
      ),
      
      conditionalPanel(
        condition = "input.main_panel == 'relationships_tab'",
        h4("Relationship Plot"),
        
        selectInput(
          inputId = "relationship_x",
          label = "X-axis:",
          choices = demo_columns,
          selected = default_relationship_x
        ),
        
        selectInput(
          inputId = "relationship_y",
          label = "Y-axis:",
          choices = demo_numeric_columns,
          selected = default_relationship_y
        ),
        
        selectInput(
          inputId = "relationship_color",
          label = "Color by:",
          choices = c("None" = "none", demo_columns),
          selected = default_relationship_color
        ),
        
        selectInput(
          inputId = "relationship_geom",
          label = "Primary geometry:",
          choices = c(
            "Jittered points" = "jitter",
            "Points" = "point",
            "Boxplot" = "boxplot"
          ),
          selected = "jitter"
        ),
        
        checkboxInput(
          inputId = "relationship_smooth",
          label = "Add smoother when possible",
          value = TRUE
        ),
        
        selectInput(
          inputId = "relationship_y_scale",
          label = "Y-axis scale:",
          choices = c(
            "Linear" = "linear",
            "Log10" = "log10"
          ),
          selected = "linear"
        )
      ),
      
      conditionalPanel(
        condition = "input.main_panel == 'map_tab'",
        h4("Map")
      )
    ),
    
    
    # Main Panel ----
    # 4 minus after a comment will create a outline header
    mainPanel(
      tabsetPanel(id="main_panel", 
                  tabPanel(title="Overview", value="overview_tab", "TSCI 6202 Class activities performed during Spring 2026"),
                  tabPanel(title="Survival", value="survival_tab", 
                           plotOutput("survivalPlot1")
                  ),
                  tabPanel(title="Relationships", value="relationships_tab", 
                           plotOutput("relationshipPlot1"),plotOutput("relationshipPlot2")),
                  tabPanel(title="Map", value="map_tab",
                           tmapOutput("Geomap1")),
                  if(file.exists('debug')){
                    tabPanel(title="Debug", value="debug_tab",
                             actionButton("debug","Debug")
                    )
                  } else {NULL}
      )
    )
  )
)

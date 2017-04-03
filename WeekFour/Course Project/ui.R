library(shiny)
library(leaflet)
library(shinythemes)

# storm data loaded in server?

if (!exists("dfMod")) {
  load(file="./usa_storm_data.RData")
  years_string <- (as.character.Date(df$BGN_DATE,format="%d/%m/%Y"))
  dates <- as.POSIXlt(years_string,format = "%d/%M/%Y")
  rm(years_string)
  dfMod <- transform( df, DATE=dates)
  rm(dates)
  rm(df)
}
  

ui <- fluidPage(
  theme =  shinythemes::shinytheme("flatly"),
#  "shinythemes",
  titlePanel("U.S. National Oceanic and Atmospheric Administration Data"),
  h4("Visualize the location of storm events and their impact"),
  sidebarLayout(
    sidebarPanel(
      selectInput('eventType', selected = "Hurricanes/Tornados/Thunderstorms",
                  'Disaster type (select one or more)', 
                  choices = sort(as.character(levels(dfMod$EVT_TYPE))),
                  multiple = TRUE),
      checkboxGroupInput("consequences", "Consequences (one or more):",
                         c("Fatalities" = "fatalities",
                           "Injuries" = "injuries"),selected = c("fatalities")),
      dateRangeInput("daterange1", "Date range:",
                     start = "1950-01-01",
                     end   = "2011-12-31",
                     min    = "1950-01-01",
                     max    = "2011-12-31"),
      actionButton("goButton", "Update")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Interactive map", br(),
                           leafletOutput("map1")
                           ),
                  tabPanel("Occurences in time", br(), 
                           plotOutput("plot0")), 
                  tabPanel("Instructions", 
                           br(), 
                           textOutput("instructionsOut1"),
                           br(), 
                           tags$div(id="fig0",class="shiny-image-output",style="width: 50% ;  height: 20%"),
                           br(), 
                           textOutput("instructionsOut2"),
                           br(),
                           tags$div(id="fig1",class="shiny-image-output",style="width: 50% ;  height: 20%"),
                           br(), 
                           textOutput("instructionsOut3"),
                           br(),
                           tags$div(id="fig2",class="shiny-image-output",style="width: 100% ;  height: 33%"),
                           br(), 
                           textOutput("instructionsOut4"),
                           br())
    ))
  )
)
  
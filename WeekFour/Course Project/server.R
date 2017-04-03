library(shiny)
library(leaflet)

#load storm data as df
load(file="./usa_storm_data.RData")
years_string <- (as.character.Date(df$BGN_DATE,format="%d/%m/%Y"))
dates <- as.POSIXlt(years_string,format = "%d/%M/%Y")
rm(years_string)
dfMod <- transform( df, DATE=dates)
rm(dates)
rm(df)

GetStateStormData <- function( stormDataFrame, EffectName1, EffectName2,EventType, dateStart, dateEnd) {
  ## stormDataFrame, EffectName=exact data frame column heading ("FATALITIES" or "INJURIES")
  ## Subset the storm.data.frame
  #storm_data_ST <- stormDataFrame[stormDataFrame$STATE == ST,]

  if((EffectName1=="FATALITIES") & (EffectName2=="INJURIES")){
    filter_fatalities <- ifelse(EffectName1=="FATALITIES", 1, 100000000)
    filter_injuries <- ifelse(EffectName2=="INJURIES", 1, 100000000)   
    storm_data_frame <- stormDataFrame[ stormDataFrame$EVT_TYPE %in% EventType
                                        & stormDataFrame$FATALITIES>=filter_fatalities
                                        | stormDataFrame$INJURIES>=filter_injuries, ]
                                        # & stormDataFrame$DATE >= dateStart
                                        # & stormDataFrame$DATE <= dateEnd, ]

  }
  if ((EffectName1!="FATALITIES") & (EffectName2=="INJURIES")){
    filter_injuries <- ifelse(EffectName2=="INJURIES", 1, 100000000)   
    storm_data_frame <- stormDataFrame[ stormDataFrame$EVT_TYPE %in% EventType
                                        & stormDataFrame$INJURIES>=filter_injuries, ]
                                        # & stormDataFrame$DATE >= dateStart
                                        # & stormDataFrame$DATE <= dateEnd, ]
  }
  
  if ((EffectName1=="FATALITIES") & (EffectName2!="INJURIES")){
    filter_fatalities <- ifelse(EffectName1=="FATALITIES", 1, 100000000)
    storm_data_frame <- stormDataFrame[ stormDataFrame$EVT_TYPE %in% EventType
                                        & stormDataFrame$FATALITIES>=filter_fatalities, ]
                                        # & stormDataFrame$DATE >= dateStart
                                        # & stormDataFrame$DATE <= dateEnd, ]
  }
  
  df <- storm_data_frame[storm_data_frame$LATITUDE > 0 & storm_data_frame$LONGITUDE > 0,]
  dfMod <- transform( df, LATITUDE=df$LATITUDE/100, LONGITUDE=-df$LONGITUDE/100)
}


UpdateState <- function(event_types,consequences,dates){
  event_type_list<-unlist(event_types,use.names = FALSE)
  # cat("event type = ",  event_type_list, "\n")
  # cat("length of consequences = ", length(consequences), "\n")
  if( length(consequences)<2) {
    effect_1<-ifelse(consequences[1] == "fatalities", "FATALITIES", "")
    effect_2 <- ifelse(consequences[1] == "injuries", "INJURIES", "")
  }
  if( length(consequences)>1){
    effect_1<-ifelse(consequences[1] == "fatalities", "FATALITIES", "")
    effect_2<-ifelse(consequences[2] == "injuries", "INJURIES", "")
  }
  # cat("consequences = ",  consequences, "\n", "effect_1= ", effect_1,"\n","effect_2= ",effect_2,"\n")
  
  date_start <- as.POSIXlt(format(dates[1]),format = "%Y-%M-%d")
  date_end<- as.POSIXlt(format(dates[2]),format = "%Y-%M-%d")
  # cat("start date = ", format(dates[1]), "; end date = ", format(dates[2]), "\n")
  # cat("class of date: ", class(date_start), "\n")
  # 
  # cat("length of event type list: ", length(event_type_list), "\n")
  # 
  df <- GetStateStormData(dfMod,effect_1,effect_2,event_type_list,date_start,date_end)
}

renderTornadoPlot <- function(Tornados,HighWind,ThunderWind,MarineWind){
  leaflet() %>% 
    addTiles() %>% 
    addMarkers(data= Tornados,
               lat = ~ LATITUDE, 
               lng = ~ LONGITUDE, 
               label = ~ Label,
               labelOptions = labelOptions(noHide = F, textsize = "12px"),
               clusterOptions = markerClusterOptions(),
               group = "Tornados") %>%
    addMarkers(data= HighWind,
               lat = ~ LATITUDE, 
               lng = ~ LONGITUDE, 
               label = ~ Label,
               labelOptions = labelOptions(noHide = F, textsize = "12px"),
               clusterOptions = markerClusterOptions(),
               group = "High Winds") %>%
    addMarkers(data= ThunderWind,
               lat = ~ LATITUDE, 
               lng = ~ LONGITUDE, 
               label = ~ Label,
               labelOptions = labelOptions(noHide = F, textsize = "12px"),
               clusterOptions = markerClusterOptions(),
               group = "Thunderstorms") %>%
    addMarkers(data= MarineWind,
               lat = ~ LATITUDE, 
               lng = ~ LONGITUDE, 
               label = ~ Label,
               labelOptions = labelOptions(noHide = F, textsize = "12px"),
               clusterOptions = markerClusterOptions(),
               group = "Marine Winds") %>%  
    addLayersControl(
      overlayGroups=c("Tornados","High Winds","Thunderstorms","Marine winds"),
      options=layersControlOptions(collapsed=FALSE))    
}

renderStormEffectsPlot <- function(stormEffects){
  leaflet() %>% 
    addTiles() %>% 
    addMarkers(data= stormEffects,
               lat = ~LATITUDE,
               lng = ~LONGITUDE,
               label = ~ Label,
               labelOptions = labelOptions(noHide=F, textsize="12px"),
               clusterOptions = markerClusterOptions())
    
}


shinyServer(function(input, output) {

  # update the data frame for the map:
  # state_vals <- reactiveValues(evnType=input$eventType,
  #                              consequence=input$consequences,
  #                              dates=input$daterange1)
#  dfFatalities <- GetStateStormData(dfMod,"FATALITIES","Hurricanes/Tornados/Thunderstorms")  
  
  data_frame<-eventReactive( eventExpr = input$goButton, 
                             valueExpr = { UpdateState(input$eventType,input$consequences,input$daterange1) })
  
    
  output$plot0 <- renderPlot({
    
    df <- data_frame()
    num_plots <- length(isolate({input$consequences}))
    # cat("num plots =", num_plots,"\n")
    par(mfrow=c(num_plots,1))

    if (num_plots==1) {
      effect_label<-ifelse(test = ("fatalities" %in% input$consequences), "FATALITIES", "INJURIES")
      if(effect_label=="FATALITIES"){
        plot(x = df$DATE,y = df$FATALITIES, xlab="Dates", ylab=effect_label)  
      }
      else{
        plot(x = df$DATE,y = df$INJURIES, xlab="Dates", ylab=effect_label)  
      }
      
    }
    
    if (num_plots>1){
      effect_label_1<-ifelse(test = ("fatalities" %in% input$consequences), "FATALITIES", "")
      effect_label_2 <- ifelse(test = ("injuries" %in% input$consequences), "INJURIES", "")
      plot(x=df$DATE, y= df$FATALITIES, xlab="Dates", ylab=effect_label_1)
      plot(x=df$DATE, y= df$INJURIES, xlab="Dates", ylab=effect_label_2)
    }    
  })
  
  output$map1 <- renderLeaflet({
    # listOfEventTypes <- state_vals$evnType
    # event_type <- listOfEventTypes[[1]]
    # 
    # dfFatalities <- GetStateStormData(dfMod,event_type,categoryList[[1]])  

    dfFatalities <- data_frame()    
    years <-as.character(as.Date(dfFatalities$DATE,format= "%d/%m/%Y"),"%Y-%b-%d")
    popup_labels <- mapply(FUN=function(x1,x2,y,z) paste0(ifelse(is.na(y), "No date:", paste0(y, ": ")),
                                                          paste0(z, ": "),
                                                          ifelse(x1>0,
                                                                 paste0(as.character(x1),
                                                                        ifelse(x1<2," fatality"," fatalities")),
                                                                 ""),
                                                          ifelse(x2>0,
                                                                 paste0(as.character(x2),
                                                                        ifelse(x2<2," injury"," injuries")),
                                                                 "")),
                           dfFatalities$FATALITIES,
                           dfFatalities$INJURIES,
                           years, 
                           dfFatalities$EVTYPE)
    dfFatalities$Label <- popup_labels
    
    if (FALSE) {
      dfFatalTornados <- dfFatalities[dfFatalities$EVTYPE=="TORNADO",]
      dfFatalHighWind <- dfFatalities[dfFatalities$EVTYPE=="HIGH WIND",]
      dfFatalThunderWind <- dfFatalities[dfFatalities$EVTYPE=="THUNDERSTORM WIND",]
      dfFatalMarineWind <- dfFatalities[(dfFatalities$EVTYPE=="MARINE STRONG WIND") | (dfFatalities$EVTYPE == "MARINE THUNDERSTORM WIND"),]

      renderTornadoPlot(dfFatalTornados,dfFatalHighWind,dfFatalThunderWind,dfFatalMarineWind)
    }
    renderStormEffectsPlot(dfFatalities)
  })
  
  output$instructionsOut1 = renderText("1. Select one or more types of wheather events")
  output$instructionsOut2 = renderText("2. Select a range of dates")
  output$instructionsOut3 = renderText("3. Run the selection by clicking on the \"Show\" button")
  output$instructionsOut4 = renderText("4. Further filter by sub-types of events on the interactive map")  

  filename0 <- normalizePath(file.path('./images', paste('fig0.png', sep='')))  
  filename1 <- normalizePath(file.path('./images', paste('fig1.png', sep='')))  
  filename2 <- normalizePath(file.path('./images', paste('fig2.png', sep='')))  
  
  output$fig0 = renderImage(list(src=filename0), deleteFile = FALSE)
  output$fig1 = renderImage(list(src=filename1), deleteFile = FALSE)
  output$fig2 = renderImage(list(src=filename2), deleteFile = FALSE)
  
  })
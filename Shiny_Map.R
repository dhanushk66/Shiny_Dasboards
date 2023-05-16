library(tidyverse)
library(leaflet)
library(lubridate)
library(shiny)
library(shinyWidgets)
# Set the URL of the CSV file to download

weather<-read_csv("/Users/dhanushkikkisetti/Documents/Spring_2023/Data_613_Datascience/613_dataset/WeatherEvents.csv")
str(weather)
nrow(weather)
#Data Cleaning and Data Preprocessing
weather%>%
  summarise(across(everything(),~sum(is.na(.))))
weather%>%
  filter(!is.na(City),!is.na(ZipCode))->weather
df<-weather[sample(nrow(weather),size = 80000),]

df$`StartTime(UTC)`<-as.Date(df$`StartTime(UTC)`)
df$`EndTime(UTC)`<-as.Date(df$`EndTime(UTC)`)

df%>%
  select(-c(EventId,TimeZone))->df

df%>%
  filter(`StartTime(UTC)`>'2017-10-04'&`StartTime(UTC)`<'2019-10-04')->d
d%>%
  group_by(LocationLat,LocationLng,Type)%>%
  summarise(c=n(),percep=sum(`Precipitation(in)`))->d1
d1
lj1<-left_join(d1,d,by=c('LocationLat'='LocationLat','LocationLng'='LocationLng','Type'='Type'))
lj1%>%
  select(LocationLat,LocationLng,Type,c,`StartTime(UTC)`,percep)%>%
  group_by(LocationLat,LocationLng,Type,c,percep) %>% 
  mutate(Dates = paste0(`StartTime(UTC)`, collapse = ","))%>%
  select(-c(`StartTime(UTC)`))%>%
  unique()->lj1

pal <- colorFactor(
  palette = 'Dark2',
  domain = lj1$Type
)
leaflet(lj1) %>%
  addTiles() %>%
  setView(lng = -96.25, lat = 39.50, zoom = 30) %>%
  fitBounds(lng1 = min(d$LocationLng), 
            lat1 = min(d$LocationLat), 
            lng2 = max(d$LocationLng), 
            lat2 = max(d$LocationLat))%>%
  addMarkers(clusterOptions = markerClusterOptions(),lng = ~LocationLng, lat = ~LocationLat,
             popup =paste(sep = '<br/>',
                          "Weather Condition",lj1$Type,"Number of days",lj1$c,"Dates",lj1$Dates,"Total Precipitation",lj1$percep))
#Shinny App starts from here
ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("daterange", "Choose the Date range:",
                     start  = "2015-01-01",
                     end    = "2016-12-31",
                     min    = "2015-01-01",
                     max    = "2019-12-21",
                     ),
      
      tableOutput('t1')
    ),
    mainPanel(
      leafletOutput('l1',height="100vh")
    )
  )
)
server<-function(input,output){
  update<-reactive({
    df%>%
      filter(`StartTime(UTC)`>input$daterange[1]&`StartTime(UTC)`<input$daterange[2])
     
  })
  output$l1<-renderLeaflet({
    update()%>%
      group_by(LocationLat,LocationLng,Type)%>%
      summarise(c=n(),percep=sum(`Precipitation(in)`))->d1
    lj1<-left_join(d1,update(),by=c('LocationLat'='LocationLat','LocationLng'='LocationLng','Type'='Type'))
    lj1%>%
      select(LocationLat,LocationLng,Type,c,`StartTime(UTC)`,percep)%>%
      group_by(LocationLat,LocationLng,Type,c,percep) %>% 
      mutate(Dates = paste0(`StartTime(UTC)`, collapse = ","))%>%
      select(-c(`StartTime(UTC)`))%>%
      unique()->lj1
    
    leaflet(lj1) %>%
      addTiles() %>%
      setView(lng = -96.25, lat = 39.50, zoom = 30) %>%
      fitBounds(lng1 = min(d$LocationLng), 
                lat1 = min(d$LocationLat), 
                lng2 = max(d$LocationLng), 
                lat2 = max(d$LocationLat))%>%
      addMarkers(clusterOptions = markerClusterOptions(),lng = ~LocationLng, lat = ~LocationLat,
                 popup =paste(sep = '<br/>',
                              "Weather Condition",lj1$Type,"Number of days",lj1$c,"Dates",lj1$Dates,"Total Precipitation",lj1$percep))
    
  })
  
}
shinyApp(ui,server)

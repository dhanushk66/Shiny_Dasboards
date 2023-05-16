library(tidyverse)
library(shiny)
library(dplyr)
library(shinyWidgets)
library(glue)
#install.packages("bslib")
library(bslib)
library(ggmap)
library(osmdata)
library(ggpie)
library(plotrix)
library(lubridate)
library(ggrepel)
library(highcharter) 
thematic::thematic_shiny(font = "auto")

data=read_csv('//Users/dhanushkikkisetti/Documents/Spring_2023/Data_613_Datascience/613_dataset/Satelliete.csv')
head(data)
str(data)
options(digits = 4)
data%>%
  summarise(across(everything(),~sum(is.na(.))))%>%
  pivot_longer(cols = 'Official Name of Satellite':'NORAD Number',
               names_to='variable',
               values_to='NA')%>%
  print(n=26)


satellite = select(data, select = -c(`Country of Operator/Owner`,`Eccentricity`,
                                     `Period (Minutes)`,`Power (Watts)`,`Launch Site`,`NORAD Number`,
                                     `Detailed Purpose`))


# replacing NA values with unknown 
satellite$`Type of Orbit` <- ifelse(is.na(satellite$`Type of Orbit`), "unknown", satellite$`Type of Orbit`)

# Remove non-numeric characters from the value column
satellite$`Dry Mass (Kilograms)` <- as.numeric(gsub("[^0-9]", "", satellite$`Dry Mass (Kilograms)`))

# Replacing values by mean or median ?
hist(satellite$`Launch Mass (Kilograms)`)
# the graph is skewed to right so we will use median approach 
hist(satellite$`Dry Mass (Kilograms)`)
# the graph is skewed to right so we will use median approach 

# Calculate the median of the non-missing values
# the median value is 1178.  
median_mass <- median(satellite$`Launch Mass (Kilograms)`, na.rm = TRUE)
# Impute the missing values with the median
satellite$`Launch Mass (Kilograms)`[is.na(satellite$`Launch Mass (Kilograms)`)] <- median_mass
# another variable 
median_mass <- median(satellite$`Dry Mass (Kilograms)`, na.rm = TRUE)
# the median value is 980.  
# Impute the missing values with the median
satellite$`Dry Mass (Kilograms)`[is.na(satellite$`Dry Mass (Kilograms)`)] <- median_mass

# mean 

satellite$`Expected Lifetime (Years)` <- as.numeric(gsub("[^0-9]", "", satellite$`Expected Lifetime (Years)`))
mean_mass <- mean(satellite$`Expected Lifetime (Years)`, na.rm = TRUE)
satellite$`Expected Lifetime (Years)`[is.na(satellite$`Expected Lifetime (Years)`)] <- mean_mass

# omitting rest all values form data
satellite <- na.omit(satellite) 



# Separate date string into day, month, and year columns
#satellite <- separate(satellite, col =mdy(`Date of Launch`), into = c("month", "day", "year"), sep = "/")
#satellite$newdate <- strptime(as.character(satellite$`Date of Launch`), "%m/%d/%Y")
satellite$`Date of Launch`=parse_date_time(x = satellite$`Date of Launch`,
                orders = c("m/d/Y", "m/d/y"))
satellite$'Year'<-year(satellite$`Date of Launch`)
# COUNT 
unique(satellite$`Country of Contractor`)
table(satellite$`Country of Contractor`)
satellite%>%
  mutate(orbit=`Class of Orbit`)->satellite
# -----------------------------------------------------------------------------
# Data cleaning completed 

# Calculating Na values in data set after cleaning 
na_counts <- satellite %>% 
  summarise_all(~ sum(is.na(.)))%>%
  gather()
# View the results
head(na_counts,18)


# number of rows 
nrow(satellite)
# number of colums 
ncol(satellite)
# name of colums 
colnames(satellite)



#For page one(Home) 
satellite%>%
  select(`Country of Contractor`,`Users`,`Purpose`,`Year`,`orbit`)%>%
  mutate('Country of Contractor'<-gsub(",","/",satellite$`Country of Contractor`))%>%
  separate( col = `Country of Contractor`, into = c("c1", "c2", "c3",'c4'), sep = "/")%>%
  separate(col=Users,into=c('u1','u2'),sep="/")%>%
  separate(col=Purpose,into=c('p1','p2','p3'),sep="/")%>%
  pivot_longer(c('c1','c2','c3','c4'),
               names_to = c("type"),
               values_to = c('country'))%>%
  pivot_longer(c('u1','u2'),
               names_to = c('usertype'),
               values_to = c("user"))%>%
  pivot_longer(c('p1','p2','p3'),
               names_to=c('purposetype'),
               values_to = c('purpose'))%>%
  filter(!is.na(country),!is.na(user),!is.na(purpose))%>%
  group_by(country,user,purpose,Year,orbit)%>%
  summarise(count=n())%>%
  arrange(desc(count))->country_satellite

country_satellite%>%
  filter(!(user=='Commerical'),!(user=='Gov'),!(country=="France, UK, Germany"))->country_satellite


#Tables for more statistics
var=c('country','purpose','user','orbit')
variables<-country_satellite[c('country','purpose','user','orbit')]
df=replicate(length(variables),data.frame(),simplify = FALSE)
for(i in 1:length(variables)){
  df[i]<-list(data.frame(table(variables[i])))
  df[[i]]%>%
    mutate(percentage=paste0(as.character(signif((Freq/sum(Freq))*100),digits=2)," %"))%>%
    arrange(desc(Freq))%>%
    select(var[i],percentage)->df[[i]]
}

#Selecting particular country page
satellite%>%
  select(`Country of Contractor`,Users,Purpose,`Operator/Owner`,orbit,Year,
         `Type of Orbit`,`Longitude of Geosynchronous Orbit (Degrees)`,`Apogee (Kilometers)`,
         `Perigee (Kilometers)`,`Launch Mass (Kilograms)`,`Expected Lifetime (Years)`)%>%
  mutate('Country of Contractor'<-gsub(",","/",satellite$`Country of Contractor`))%>%
  separate( col = `Country of Contractor`, into = c("c1", "c2", "c3",'c4'), sep = "/")%>%
  separate(col=Users,into=c('u1','u2'),sep="/")%>%
  separate(col=Purpose,into=c('p1','p2','p3'),sep="/")%>%
  pivot_longer(c('c1','c2','c3','c4'),
               names_to = c("type"),
               values_to = c('country'))%>%
  pivot_longer(c('u1','u2'),
               names_to = c('usertype'),
               values_to = c("user"))%>%
  pivot_longer(c('p1','p2','p3'),
               names_to=c('purposetype'),
               values_to = c('purpose'))%>%
  filter(!is.na(country),!is.na(user),!is.na(purpose))%>%
  select(-c('purposetype','usertype','type'))->satellite_analysis
  
pie(country_satellite$count,
    labels = paste(country_satellite$country), 
    col = rainbow(length(country_satellite$country)), 
    main = "Number of Stellietes launched by each country")
pie3D(country_satellite$count,labels=country_satellite$country,explode=0.1,
      main="Pie Chart of Countries ")

#Working with maps
m$x[which(m$names=='USA')]
df=data_frame(country=character(),
              lat=double(),
              long=double())
m <- map('world',
         col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
         mar=rep(0,4),border=0, ylim=c(-80,80))
for(b in (country_satellite$country)){
  df[nrow(df)+1,]=c(b,m$x[which(m$names==b)],m$y[which(m$names==b)])
}
country_map=inner_join(country_satellite,df,by='country')
country_map
options(digits=12)
country_map%>%
  mutate(lat=as.numeric(lat),long=as.numeric(long))->country_map
country_map$long<-as.numeric(as.character(country_map$long))
world <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = country_map,
    aes(lat, long, color = country),
    alpha = 0.7
  )
country_satellite%>%
  ggplot( aes(x = "", y = count, fill = `country`)) +
  geom_bar(stat = "identity", width = 10, color = "black") +
  coord_polar("y") +
  theme_void() +
  theme() +
  #theme(legend.position = "top") +
  labs(title = "60 countires have satellite")

geocode('USA')
## Selecting particular country and finding the pie chart for what purpose they are using.
## Select particular country and showing a scatterplot if their increase over time
## Getting barplot for the type of user using the sattllite.
## Getting summary table for numeric data in the dataset for a country.

## Finding the average of perigee and apogee for each type of orbit
## The type of users holding their satellite


ui <- fluidPage(
  sliderInput("x", "x", value = 1, min = 0, max = 10),
  sliderInput("y", "y", value = 2, min = 0, max = 10),
  sliderInput("z", "z", value = 3, min = 0, max = 10),
  textOutput("total")
)
server <- function(input, output, session) {
  observeEvent(input$x, {
    message(glue("Updating y from {input$y} to {input$x * 2}"))
    updateSliderInput(session, "y", value = input$x * 2)
  })
  
  total <- reactive({
    total <- input$x + input$y + input$z
    message(glue("New total is {total}"))
    total
  })
  
  output$total <- renderText({
    total()
  })
}
shinyApp(ui,server)

library(glue)
name <- "Hadley"
message(glue("Hello {name}"))


library(shiny)
custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B",
  base_font = "Maven Pro"
)
# ui
ui<-navbarPage(title = "Satellietes In World ",
           tabPanel(title = "Satellietes Overview", 
                    fluidPage(
                      setBackgroundColor(
                        color = c("white", "light blue"),
                        gradient = "linear",
                        direction = "bottom"
                      ),
                      
                      fluidRow(
                        column(5,offset = 1,
                               div(actionButton(inputId ='id1',"About Satelliete Dataset"),
                                   style="margin:auto;width:30%;")),
                        column(5,offset = 1,
                               helpText("Check Down for more Exploration",
                               style="font-size:115%;font-style:italic;"),
                               br())),
                      fluidRow(column(9,offset = 3,
                                      img(src="/Users/dhanushkikkisetti/Downloads/WhatsApp Image 2023-05-10 at 14.44.45.jpeg",height=25))),
                      fluidRow(
                        column(3,offset = 2,br(),
                               h1("2089",align='center',
                                  style="font-size:350%;letter-spacing:3px;"),
                               h3("Satellites",align='center',
                                  style='opacity:0.75;'),br(),
                               div(actionButton(inputId = 'm1',"More Statistics"),
                                   style="margin:auto;width:30%;")),
                        column(6,plotOutput('o1',height="400px")),
                        column(1)),
                      fluidRow(
                        column(3,offset = 2,br(),
                               h1("13",align='center',
                                  style='font-size:350%;letter-spacing:3px'),
                               h3("Purposes in the World",align='center',
                                  style='opacity:0.75;'),br(),
                               div(actionButton(inputId = 'm2',"More Statistics"),
                                   style="margin:auto;width:30%;")),
                        column(6,plotOutput('o2',height = '400px')),
                        column(1)),
                      fluidRow(
                        column(3,offset =2,br(),
                               h1('4',align='center',
                                  style='font-size:350%;letter-spacing:3px'),
                               h3("Users",align='center',
                                  style='opacity:0.75;'),br(),
                               div(actionButton(inputId = 'm3',"More Statistics"),
                                   style="margin:auto;width:30%;")),
                        column(6,plotOutput('o3',height = '400px')),
                        column(1)),
                      fluidRow(
                        column(3,offset = 2,
                               h1("4",align='center',
                                  style='font-size:350%;letter-spacing:3px'),
                               h3("Classes of Orbit",align='center',
                                  style='opacity:0.75;'),br(),
                               div(actionButton(inputId = 'm4',"More Statistics"),
                                   style="margin:auto;width:30%;")),
                        column(6,plotOutput('o4',height = '400px')),
                        column(1))
                  )
                ),
           tabPanel(title = "Country Analysis",
                    fluidPage(
                      fluidRow(
                        column(8,offset = 4,
                        selectizeInput('c1',"Choose the country for analysis",choices=c("USA","Russia","China","France","Germany","UK","Italy","India")))
                        ),
                      fluidRow(
                        column(5,offset = 1,
                               plotOutput('p1',height = '400px')),
                        column(5,highchartOutput('p2')),
                        column(1)
                      ),
                      fluidRow(
                        column(8,offset = 2,
                               verbatimTextOutput('p3')),
                        column(2)
                      )
                    )
                  ),
           tabPanel(title = "Time Series Analysis ",
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            inputId = 'rb',
                            label="Choose the Type of Satelliete ",
                            choices = c("Nano-Satelliete","General Satelliete")
                          ),
                          numericInput(inputId ='ni',label = "Use 1-25 numbers for Different Shape",value = 1),
                          numericInput(inputId ='ni2',label = "Want to Increase Size of the point?",value = 1),
                          selectInput(inputId = 'si',label = "Add Other Variable",choices = c("Red","Blue","Green","Yellow"))),
                          mainPanel(
                           plotOutput('p4',height = '500px')
                        )
                      )
                    ))
)

# server
server<-function(input, output) {
  
 output$o1<-renderPlot({
   country_satellite%>%
     group_by(country)%>%
     summarise(count=n())%>%
     ggplot(aes(count,reorder(country,-count),label=count))+
        geom_segment(aes(x=0,y=reorder(country,-count),xend=count,yend=reorder(country,-count)),size=0.6,color="grey50")+
        geom_point(size=6)+
        geom_text(color='white',size=3)+
        coord_flip()+
        theme(axis.text.x = element_text(size = 12,angle=90,hjust=1),
              axis.text.y = element_text(size=12),
              axis.title.y=element_blank(),
              axis.title.x = element_blank(),
              axis.ticks.x=element_line(size=0))
     
 })
 output$o2<-renderPlot({
   country_satellite%>%
     group_by(purpose)%>%
     summarise(count=n())%>%
     ggplot(aes(count,reorder(purpose,-count),label=count))+
     geom_segment(aes(x=0,y=reorder(purpose,-count),xend=count,yend=reorder(purpose,-count)),size=0.6,color="grey50")+
     geom_point(size=8)+
     geom_text(color='white',size=3)+
     coord_flip()+
     theme(axis.text.x = element_text(size = 12,angle=90,hjust=1),
           axis.text.y = element_text(size=12),
           axis.title.y=element_blank(),
           axis.title.x = element_blank(),
           axis.ticks.x=element_line(size=0))
   
 })
 output$o3<-renderPlot({
   country_satellite%>%
     group_by(user)%>%
     summarise(count=n())%>%
     ggplot(aes(count,reorder(user,-count),label=count))+
     geom_segment(aes(x=0,y=reorder(user,-count),xend=count,yend=reorder(user,-count)),size=0.6,color="grey50")+
     geom_point(size=8)+
     geom_text(color='white',size=3)+
     coord_flip()+
     theme(axis.text.x = element_text(size = 12,angle=90,hjust=1),
           axis.text.y = element_text(size=12),
           axis.title.y=element_blank(),
           axis.title.x = element_blank(),
           axis.ticks.x=element_line(size=0))
   
 })
 output$o4<-renderPlot({
   country_satellite%>%
     group_by(orbit)%>%
     summarise(count=n())%>%
     ggplot(aes(count,reorder(orbit,-count),label=count))+
     geom_segment(aes(x=0,y=reorder(orbit,-count),xend=count,yend=reorder(orbit,-count)),size=0.6,color="grey50")+
     geom_point(size=8)+
     geom_text(color='white',size=3)+
     coord_flip()+
     theme(axis.text.x = element_text(size = 12,angle=90,hjust=1),
           axis.text.y = element_text(size=12),
           axis.title.y=element_blank(),
           axis.title.x = element_blank(),
           axis.ticks.x=element_line(size=0))
   
 })
 observeEvent(input$id1,{
   showModal(modalDialog(
     print('Introduction'),
     p("The Dataset consist of variables like :Official Name of Satellite,
       Country/Organization of UN Registry,Users,Purpose,Class of Orbit,Type of Orbit,Dry mass,Launch mass"),
     easyClose = TRUE,
     footer = modalButton("Close")
   ))
 })
 observeEvent(input$m1,{
   showModal(modalDialog(
     renderTable({
       df[[1]]}),
     easyClose = TRUE,
     footer = modalButton("Close")
   ))
 })
 observeEvent(input$m2,{
   showModal(modalDialog(
     renderTable({df[[2]]}),
     easyClose = TRUE,
     footer = modalButton("Close")
   ))
 })
 observeEvent(input$m3,{
   showModal(modalDialog(
     renderTable({df[[3]]}),
     easyClose = TRUE,
     footer = modalButton('Close')
   ))
 })
 observeEvent(input$m4,{
   showModal(modalDialog(
     renderTable({df[[4]]}),
     easyClose = TRUE,
     footer=modalButton("Close")
   ))
 })
 #For reactivity
 datainput<-reactive({
   req(input$c1)
   satellite_analysis%>%
     filter(country==input$c1)
 })
 #For plot 1
output$p1<-renderPlot({
  d<-datainput()%>%
        group_by(`Type of Orbit`)%>%
        summarise(count=n())%>%
        mutate(prop=round((count/sum(count))*100,2))
  d1<-d %>% 
    mutate(csum = rev(cumsum(rev(prop))), 
           pos = prop/2 + lead(csum, 1),
           pos = if_else(is.na(pos), prop/2, pos))
  ggplot(d, aes(x = "" , y = prop, fill = fct_inorder(`Type of Orbit`))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Pastel1") +
    geom_label_repel(data = d1,
                     aes(y = pos, label = paste0(prop, "%")),
                     size = 4.5, nudge_x = 1, show.legend = FALSE) +
    guides(fill = guide_legend(title = "Type of Orbit")) +
    theme_void()+
    ggtitle(paste("Distribution of satellites in ",input$c1))
  
})
output$p2<-renderHighchart({
  d<-datainput()%>%
    group_by(purpose,user)%>%
    summarise(count=n())
  #ggplot(d,aes(x=user,y=count,fill=purpose))+
   # geom_bar(position="stack", stat="identity")
  d %>% 
    hchart('column', hcaes(x = 'user', y = 'count', group = 'purpose'),stacking = "normal")
})
output$p3<-renderPrint({
  d<-datainput()%>%
    filter(`Longitude of Geosynchronous Orbit (Degrees)`!=0)
  summary(d[,c("Launch Mass (Kilograms)","Expected Lifetime (Years)","Perigee (Kilometers)","Apogee (Kilometers)" )])
})
type<-reactive(
  switch(input$rb,
         "Nano-Satelliete"=satellite_analysis%>%filter(`Longitude of Geosynchronous Orbit (Degrees)`==0),
         "General Satelliete"=satellite_analysis%>%filter(`Longitude of Geosynchronous Orbit (Degrees)`!=0))
)
#v<-reactive(
 # switch(input$si,
  #       "Users"=user,
   #      "Purpose"=purpose,
    #     "Orbit"=orbit,
     #    "Country"=country)
#)
output$p4<-renderPlot({
  df<-type()%>%
      group_by(Year)%>%
      summarise(count=n())
  ggplot(df,aes(x=Year,y=count) )+
    geom_point(shape=as.numeric(input$ni),size=input$ni2,color=input$si)+
    xlab("Years")+
    ylab("Frequency")+
    ggtitle(paste("Evolution of ",input$rb))
})
}
shinyApp(ui = ui, server = server)
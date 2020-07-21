#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(stringi)
library(tidyverse)

library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
library(plyr)
library(rockchalk)
ui <- dashboardPage(skin='red',dashboardHeader(title='Netflix Data Visualisation',titleWidth = 300),
                    dashboardSidebar(sidebarMenu(menuItem("Movies",tabName = "movies",icon = icon("film")),
                                                 menuItem("Series",tabName = "tv_series", icon = icon("tv")),
                                                 menuItem("Information",tabName = "info", icon = icon("address-card"))
                                                 
                    )),
                    dashboardBody(tabItems(
                        tabItem(tabName = "movies",fluidPage( h1("Movies"),
                                                              box(plotlyOutput("correlation_plot"),width = "100%",height = "100%")
                                                              
                        )
                        
                        
                        ),
                        tabItem(tabName = "tv_series", fluidPage(h1("TV shows"),
                                                                 box(plotOutput("boxplot"),width = "100%",height = "100%")
                                                                 
                        )),
                        tabItem(tabName = "info", fluidPage(h1("About"),includeHTML("text.html"))
                        )
                        
                    ))
)





#design the output that we have defined in ui
server <- function(input,output){
    
    
    films <- read.csv('netflix_titles.csv')
    
    #separate movies from TV shows
    movies=films[which(films$type=='Movie'),]
    series=films[which(films$type=='TV Show'),]
    ###SHINY R preparation###
    #for MOVIES only##
    #countries column factor
    countries=movies$country
    countries_char=as.character(countries)
    cntr_short=substr(countries_char,1,5)
    countries_f=as.factor(cntr_short)
    movies$country<-countries_f
    #new_countries<-c("","Argentina",'Australia',"Bangladesh","Belgium","Brazil", "Bulgaria" ,"Cambodia", "Canada", "Chile", "China", "Colombia" ,"Croatia" ,"CzechR", "Denmark", "DominicR", "Egypt",
                     #"Finland", "France" ,"Georgia" ,"Germany", "Ghana" ,"Guatemala" ,"HongKong ", "Hungary" ,"Iceland", "India" ,"Indonesia" ,"Iran," ,"Ireland" ,"Israel", "Italy", "Japan", "Lebanon",
                     #"Malaysia" ,"Mexico" ,"Netherlands" ,"New Zealand", "Nigeria", "Norway" ,"Pakistan" ,"Paraguae" ,"Peru","Philipinnes" ,"Poland", "Portugal" ,"Romania" ,"Russia", "SaudiArab", "Serbia",
                     #"Singapore", "Slovenia" ,"Somalia", "SouthKorea" ,"Soviet", "Spain" ,"Sweden" ,"Switzerland", "Taiwan" ,"Thailand" ,"Turkey", "USA" ,"Uruguay" ,"Venezuela" ,"Vietnam", "WestGermany ")
    #levels(movies$country) <- new_countries
    
    
    
    
    #genres column factor
    genres=movies$listed_in
    gnr_char=as.character(genres)
    gnrs_short=substr(gnr_char,1,5)
    gnr_f=as.factor(gnrs_short)
    movies$listed_in <- gnr_f
    levels(gnr_f) <- c('Action','Anime','Children','Classics','Comedy','Culture','Documentaries','Drama','Horror','Indie','International','Movie','Music','Romance',
                       'Sci-Fi','Sport','Stand-up','Thriller')
    levels(movies$listed_in) <- levels(gnr_f)
    
    
    #release year for x axis
    year=movies$release_year
    
    # duration for y axis
    duration=movies$duration
    chars=as.character(duration)
    duration_chr=substr(chars,1,nchar(chars)-4)
    duration_int=as.numeric((duration_chr))
    movies$duration <- duration_int
    
    
    ##FOR SERIES###
    #Concatenate factor labels
    series$rating<-combineLevels(series$rating,levs = c("G","TV-G"),newLabel = "G")
    series$rating<-combineLevels(series$rating,levs = c("PG","TV-PG"),newLabel = "PG")
    series$rating<-combineLevels(series$rating,levs = c("","NR"),newLabel = "NR")
    series$rating<-combineLevels(series$rating,levs = c("TV-MA","R"),newLabel = "R")

    
    output$correlation_plot <- renderPlotly({gg <-ggplot(movies,aes(x=duration,y=release_year,text=paste("director: ",movies$director,"<br>title: ",movies$title),colour=factor(country),shape=factor(listed_in)))+
        geom_point(alpha=0.7,position = position_jitter())+
        scale_shape_manual(name="genres",values=seq(0,18))+
        scale_color_hue(name="countries")
    ggplotly(gg,dynamicTicks = TRUE)})  
  
    
    output$boxplot <- renderPlot({
        theme_set(theme_minimal())
        ##release year above 2000
        #c2=rainbow(12,alpha=0.8)
        # Plot
        g <- ggplot(series, aes(release_year,rating,fill=rating))
        p<-g + geom_boxplot( outlier.colour = NA) +
            coord_cartesian(xlim = c(2000,2020))+
            labs(title="Series rating through the years", 
                 
                 caption="Source: Netflix Data",
                 x="Release year",
                 y="Rating")
        p+scale_fill_discrete(name = "Appropriate for: ", labels = c("Children from 14 years old","All children","Children from 7 years old","Children from 7 years old-Contains Fantasy Violence","General audiences","Children with parental guidance","Not rating available","People from 17 years old"))
        
    }) 
    
   
}




shinyApp(ui,server)


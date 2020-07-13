library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(dplyr)
#new shiny dashboard
#films <- read.csv('netflix_titles.csv')

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
levels(countries_f)<-droplevels(countries_f,"Peru,")
movies$country<-countries_f
new_countries<-c("","Argentina",'Australia',"Bangladesh","Belgium","Brazil", "Bulgaria" ,"Cambodia", "Canada", "Chile", "China", "Colombia" ,"Croatia" ,"CzechR", "Denmark", "DominicR", "Egypt",
                 "Finland", "France" ,"Georgia" ,"Germany", "Ghana" ,"Guatemala" ,"HongKong ", "Hungary" ,"Iceland", "India" ,"Indonesia" ,"Iran," ,"Ireland" ,"Israel", "Italy", "Japan", "Lebanon",
                 "Malaysia" ,"Mexico" ,"Netherlands" ,"New Zealand", "Nigeria", "Norway" ,"Pakistan" ,"Paraguae" ,"Peru","Philipinnes" ,"Poland", "Portugal" ,"Romania" ,"Russia", "SaudiArab", "Serbia",
                 "Singapore", "Slovenia" ,"Somalia", "SouthKorea" ,"Soviet", "Spain" ,"Sweden" ,"Switzerland", "Taiwan" ,"Thailand" ,"Turkey", "USA" ,"Uruguay" ,"Venezuela" ,"Vietnam", "WestGermany ")
levels(movies$country) <- new_countries




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



ui <- dashboardPage(skin='red',dashboardHeader(title='Netflix Data Visualisation',titleWidth = 300),
                    dashboardSidebar(sidebarMenu(menuItem("Movies",tabName = "movies",icon = icon("film")),
                                                 menuItem("Series",tabName = "tv_series", icon = icon("tv")),
                                                 menuItem("Information",tabName = "info", icon = icon("address-card"))
                                                 
                                     )),
                    dashboardBody(tabItems(
                      tabItem(tabName = "movies",fluidPage( h1("Movies"),
                                                  box(plotlyOutput("correlation_plot"),width = "100%",height = "100%")
                                                  # box(verbatimTextOutput("click"))
                                                 # box(selectInput("countries","Countries: ",movies$country),width = 5),
                                                  #box(selectInput("genres","Listed in: ",movies$listed_in),width=5)
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
  
  #plotly_gg <- plotly_gg %>% 
  output$correlation_plot <- renderPlotly({gg <-ggplot(movies,aes(x=duration,y=release_year,text=paste("director: ",movies$director,"<br>title: ",movies$title),colour=factor(country),shape=factor(listed_in)))+
    geom_point(alpha=0.7,position = position_jitter())+
    scale_shape_manual(name="genres",values=seq(0,18))+
    scale_color_hue(name="countries")
  ggplotly(gg,dynamicTicks = TRUE)})  
  # output$click <- renderPrint({
  #   d<-event_data("plotly_click",source = gg$text)
  #   if(!is.null(d)) d })

   output$boxplot <- renderPlot({
     theme_set(theme_minimal())
     ##release year above 2000
     c2=rainbow(12,alpha=0.8)
     # Plot
     g <- ggplot(series, aes(release_year,rating))
     g + geom_boxplot( outlier.colour = NA,col=c2) +
       coord_cartesian(xlim = c(2000,2020))+
       labs(title="Series rating through the years", 
            
            caption="Source: Netflix Data",
            x="Release year",
            y="Rating")
    # legend("bottom",title = "Explanation",legend = "HEY") 
   }) 
   
   #output$information <- renderText({print("hello")})
  # output$ratings<-renderImage({
  #   # A temp file to save the output.
  #   # This file will be removed later by renderImage
  #   outfile <- tempfile(fileext = '.png')
  #   
  #   # Generate the PNG
  #   png(filename = "ratings.png", width = 400, height = 300)
  #   hist(rnorm(input$obs), main = "Generated in renderImage()")
  #   dev.off()
  #   
  #   # Return a list containing the filename
  #   list(src = outfile,
  #        contentType = 'image/png',
  #        width = 400,
  #        height = 300,
  #        alt = "This is alternate text")
  # }, deleteFile = TRUE)
  
}
 
  


shinyApp(ui,server)

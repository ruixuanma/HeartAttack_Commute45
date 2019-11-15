#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tigris)
library(blscrapeR)
library(leaflet)

mergeDt <- read_rds('/Users/maruixuan/Documents/GitHub/HeartAttack_Commute45/shiny_presentation/mergeDt.rds')
rawPop <- read.csv('/Users/maruixuan/Documents/GitHub/HeartAttack_Commute45/shiny_presentation/FLpopulation.csv')

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "Biostatistician Programmer Assessment",
    tabPanel("Population Data",
             titlePanel("Population for Each County"),
             dataTableOutput("popdt")),
    tabPanel("Rates Calculation",
             titlePanel("Calculate rates for heart attack hospitalization and long time commute"),
             tableOutput("report")),
    tabPanel("Correlation plot",
             titlePanel("Scatter plot for relationship between heart attack hospitalization and long time commute"),
             plotOutput("point")),
    tabPanel("Map",
             titlePanel("map of county"),
             leafletOutput("mymap")),
    tabPanel("Code",
             h2("See the code",
                a("here", href = "https://github.com/ruixuanma/HeartAttack_Commute45"), "!"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mymap <- renderLeaflet({
        fl <- counties(12, cb = TRUE, year = 2016)
        
        mapRatio <- mergeDt %>%
            mutate(ratio_unround = num_ha / num_45) %>%
            mutate(ratio = round(ratio_unround, 3)) %>%
            select("countyFIPS", "ratio") %>%
            rename(GEOID = countyFIPS)
        
        mapRatio$GEOID = as.character(mapRatio$GEOID)
        class(mapRatio$GEOID)
        
        leafmap <- geo_join(fl, mapRatio, by = "GEOID")
        
        popup_dat <- paste0("<strong>County: </strong>", 
                            leafmap$NAME, 
                            "<br><strong>Heart Attacks / Long Commute: </strong>", 
                            leafmap$ratio)
        
        pal3 <- colorBin(palette="YlOrRd", domain=c(min(leafmap$ratio), max(leafmap$ratio)), bins = 6, na.color = NULL, pretty=FALSE, alpha = TRUE)
        
        leaflet(data = leafmap) %>% addTiles() %>%
            addPolygons(fillColor = ~pal3(leafmap$ratio), 
                        fillOpacity = 1, 
                        color = "#BDBDC3", 
                        weight = 1,
                        popup = popup_dat) %>%
            addLegend(pal = pal3,
                      values  = leafmap$ratio,
                      position = "bottomleft",
                      title = "Heart Attacks / Long Commute ")
        
        
        
    })
    
    output$popdt <- renderDataTable({
        rawPop
    })
    
    output$report <- renderTable({
        reportDt <- mergeDt %>%
            filter(countyFIPS == 12011 | countyFIPS ==12086 | countyFIPS == 12087 | countyFIPS == 12099) %>%
            select("NAME", "hospRate", "commRate")
    })
    
    output$point <- renderPlot({
        
        com_ha_plot <- ggplot(mergeDt, aes(x = num_45 , y = num_ha)) + geom_point(color = "#69b3a2") +
            labs(title = 'Number of People Who Commute 45+ Minutes vs. Hospitalization from Heart Attack \n All FL Counties: 5-year estimates 2012-2016', 
                 x = 'Number of People Who Commute 45+ Minutes', y = "Number of Heart Attack Hospitalizations") +
            geom_smooth(method = "loess", formula = y ~ x) +
            theme_classic()
        plot(com_ha_plot)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

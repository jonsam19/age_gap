##################### Settings ##################
rm(list=ls())

## load packages
library(tidyverse)
library(haven)
library(plotly)
library(lme4)
library(sjstats)
library(shiny)

## setting working directory   
PATH="C:/R/age_gap/"

setwd(PATH)

source("agegap_app/prepare_data.R")

countries = unique(data_fevtreat$country) %>% as.vector()


ui <- fluidPage(
    
    navbarPage("Child health & parental age gap",
               
               tabPanel("Descriptive",
                        
                        sidebarPanel(h4("Percentage"),
                                     selectInput("country", "Select country",
                                                 choices=countries)),
                        mainPanel(plotlyOutput("descriptive_plot"))),
               
    )
)

server <- function(input, output, session) {
    
    descriptive_plot <- reactive(
        data_fevtreat %>% filter(country==input$country) %>% group_by(agediff5) %>% 
            count(fevtreat,wt=perweight) %>% pivot_wider(values_from=n,names_from=fevtreat) %>% 
            mutate(percent=`Received treatment`/(`No treatment`+`Received treatment`)*100) %>% 
            ggplot(aes(agediff5,percent)) + geom_bar(stat="identity")
    )
    
    output$descriptive_plot <- renderPlotly(ggplotly(descriptive_plot()))
}

# Run the application 
shinyApp(ui = ui, server = server)

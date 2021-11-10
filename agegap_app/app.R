##################### Settings ##################
rm(list=ls())

## load packages
library(tidyverse)
library(haven)
library(plotly)
library(shiny)

## setting working directory   
# PATH="C:/R/age_gap/agegap_app"
# 
# setwd(PATH)

source("prepare_data.R")

countries = unique(data_fevtreat$country) %>% as.vector() %>% sort()

ui <- fluidPage(
    
    navbarPage("Child health & parental age gap",
               
               tabPanel("Descriptive",
                        
                        sidebarPanel(h4("Percentage"),
                                     radioButtons("health_outcome", "Select health outcome",
                                                  choices=list("Fever treatment"="fevtreat",
                                                               "Measles vaccination"="measles",
                                                               "Underweight"="underweight")),
                                     selectInput("country", "Select country",
                                                 choices=c("All",countries))),
                        mainPanel(plotlyOutput("descriptive_plot"))),
               
               tabPanel("Model results")
               
    )
)

server <- function(input, output, session) {
    
    
    
    descriptive_plot <- reactive(
        
            if (input$health_outcome == "fevtreat"){
                if(input$country=="All"){
                    data_fevtreat %>% group_by(agediff5) %>% 
                        count(fevtreat,wt=perweight) %>% pivot_wider(values_from=n,names_from=fevtreat) %>% 
                        mutate(percent=`Received treatment`/(`No treatment`+`Received treatment`)*100) %>% 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% treated for fever") + xlab("Parental age difference")
                    
                }else{
                    data_fevtreat %>% filter(country==input$country) %>% group_by(agediff5) %>% 
                        count(fevtreat,wt=perweight) %>% pivot_wider(values_from=n,names_from=fevtreat) %>% 
                        mutate(percent=`Received treatment`/(`No treatment`+`Received treatment`)*100) %>% 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% treated for fever") + xlab("Parental age difference")
                }
            }
            else if (input$health_outcome == "measles"){
                if(input$country=="All"){
                    data_measles %>% group_by(agediff5) %>% 
                        count(measles,wt=perweight) %>% pivot_wider(values_from=n,names_from=measles) %>% 
                        mutate(percent=`Vaccinated`/(`Not vaccinated`+`Vaccinated`)*100) %>% 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% vaccinated against measles") + xlab("Parental age difference")
                    
                } else {
                    data_measles %>% filter(country==input$country) %>% group_by(agediff5) %>% 
                        count(measles,wt=perweight) %>% pivot_wider(values_from=n,names_from=measles) %>% 
                        mutate(percent=`Vaccinated`/(`Not vaccinated`+`Vaccinated`)*100) %>% 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% vaccinated against measles") + xlab("Parental age difference")
                }
            } else {
                if(input$country=="All"){
                    data_underweight %>% group_by(agediff5) %>% 
                        count(underweight,wt=perweight) %>% pivot_wider(values_from=n,names_from=underweight) %>% 
                        mutate(percent=`Underweight`/(`Normal weight`+`Underweight`)*100) %>% 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% underweight") + xlab("Parental age difference")
                    
                } else {
                    data_underweight %>% filter(country==input$country) %>% group_by(agediff5) %>% 
                        count(underweight,wt=perweight) %>% pivot_wider(values_from=n,names_from=underweight) %>% 
                        mutate(percent=`Underweight`/(`Normal weight`+`Underweight`)*100) %>% 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% underweight") + xlab("Parental age difference")
                }
            
        }
        
    )
    
    output$descriptive_plot <- renderPlotly(ggplotly(descriptive_plot()))
}

# Run the application 
shinyApp(ui = ui, server = server)

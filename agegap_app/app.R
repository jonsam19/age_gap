# rsconnect::deployApp()

##################### Settings ##################
rm(list=ls())

## load packages
library(tidyverse)
library(lme4)
library(broom)
library(broom.mixed)
library(haven)
library(plotly)
library(shiny)
library(shinydashboard)
library(sjPlot)

## setting working directory   
# PATH="C:/R/age_gap/agegap_app"
# setwd(PATH)

source("prepare_data.R")

countries = unique(data_fevtreat$country) |> as.vector() |> sort()

ui <- dashboardPage(
    dashboardHeader(title ="Child health"),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Descriptive", tabName="Descriptive"),
            menuItem("Models", tabName="Models"))),
    
    dashboardBody(
        tabItems(
            tabItem(tabName="Descriptive",
                    fluidRow(box(plotlyOutput("descriptive_plot"),width=9),
                             box(radioButtons("health_outcome", "Select health outcome",
                                              choices=list("Fever treatment"="fevtreat",
                                                           "Measles vaccination"="measles",
                                                           "Underweight"="underweight")),
                                 selectInput("country", "Select country",
                                             choices=c("All",countries)),width=3))),
            tabItem(tabName="Models",
                    fluidRow(box(plotlyOutput("model_plot"),width=9),
                             box(radioButtons("health_outcome2", "Select health outcome",
                                              choices=list("Fever treatment"="fevtreat",
                                                           "Measles vaccination"="measles",
                                                           "Underweight"="underweight")),
                                 radioButtons("coefficients","Show coefficients",
                                              choices=list("Yes"="yes","No"="no")))))
        )
        
    ))

server <- function(input, output, session) {
    
    descriptive_plot <- reactive(
        
            if (input$health_outcome == "fevtreat"){
                if(input$country=="All"){
                    data_fevtreat |> group_by(agediff5) |> 
                        count(fevtreat,wt=perweight) |> pivot_wider(values_from=n,names_from=fevtreat) |> 
                        mutate(percent=`Received treatment`/(`No treatment`+`Received treatment`)*100) |> 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none",
                              plot.title = element_text(size=13)) +
                        ylab("% treated for fever") + xlab("Parental age difference") +
                        ggtitle("Percentage of children under five who were given any treatment 
                                if they had fever or cough in the last two weeks")
                    
                }else{
                    data_fevtreat |> filter(country==input$country) |> group_by(agediff5) |> 
                        count(fevtreat,wt=perweight) |> pivot_wider(values_from=n,names_from=fevtreat) |> 
                        mutate(percent=`Received treatment`/(`No treatment`+`Received treatment`)*100) |> 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% treated for fever") + xlab("Parental age difference") +
                        ggtitle("Percentage of children under five who were given any treatment 
                                if they had fever or cough in the last two weeks")
                }
            }
            else if (input$health_outcome == "measles"){
                if(input$country=="All"){
                    data_measles |> group_by(agediff5) |> 
                        count(measles,wt=perweight) |> pivot_wider(values_from=n,names_from=measles) |> 
                        mutate(percent=`Vaccinated`/(`Not vaccinated`+`Vaccinated`)*100) |> 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% vaccinated against measles") + xlab("Parental age difference") +
                        ggtitle("Percentage of children between one and five 
                                who have received the first round of measles vacciens")
                    
                } else {
                    data_measles |> filter(country==input$country) |> group_by(agediff5) |> 
                        count(measles,wt=perweight) |> pivot_wider(values_from=n,names_from=measles) |> 
                        mutate(percent=`Vaccinated`/(`Not vaccinated`+`Vaccinated`)*100) |> 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% vaccinated against measles") + xlab("Parental age difference") +
                        ggtitle("Percentage of children between one and five 
                                who have received the first round of measles vacciens")
                }
            } else {
                if(input$country=="All"){
                    data_underweight |> group_by(agediff5) |> 
                        count(underweight,wt=perweight) |> pivot_wider(values_from=n,names_from=underweight) |> 
                        mutate(percent=`Underweight`/(`Normal weight`+`Underweight`)*100) |> 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% underweight") + xlab("Parental age difference") +
                        ggtitle("Percentage of children under five who are underweight")
                    
                } else {
                    data_underweight |> filter(country==input$country) |> group_by(agediff5) |> 
                        count(underweight,wt=perweight) |> pivot_wider(values_from=n,names_from=underweight) |> 
                        mutate(percent=`Underweight`/(`Normal weight`+`Underweight`)*100) |> 
                        ggplot(aes(agediff5,percent,fill=agediff5)) + geom_bar(stat="identity") +
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                              panel.background = element_blank(), legend.position = "none") +
                        ylab("% underweight") + xlab("Parental age difference")
                }
            
        }
        
    )
    
    model_plot <- reactive(
      
      if (input$health_outcome2 == "fevtreat" & input$coefficients=="yes") {
        fevtreat_model2 |> plot_model()
        # rbind(fevtreat_model1,fevtreat_model2,fevtreat_model3) |> 
        #   filter(term!="sd__(Intercept)") |> 
        #   mutate(term=case_when(term=="agediff5<0"~"<0",
        #                         term=="agediff50-4"~"0-4",
        #                         term=="agediff510-14"~"10-14",
        #                         term=="agediff515+"~"15+",
        #                         term=="kidsexfemale"~"Female",
        #                         term=="kidcurage1 year"~"1 year",
        #                         term=="kidcurage2 years"~"2 years",
        #                         term=="kidcurage3 years"~"3 years",
        #                         term=="kidcurage4 years"~"4 years",
        #                         term=="kidbord2nd"~"2nd",
        #                         term=="kidbord3rd"~"3rd",
        #                         term=="kidbord4th"~"4th",
        #                         term=="kidbord5th"~"5th",
        #                         term=="kidbord6th or >"~"6th or >",
        #                         term=="polyOther wives"~"Polygynous",
        #                         term=="mage15-19"~"15-19",
        #                         term=="mage20-24"~"20-24",
        #                         term=="mage30-34"~"30-34",
        #                         term=="mage35-39"~"35-39",
        #                         term=="mage40+"~"40+",
        #                         term=="educlvlprimary"~"Primary",
        #                         term=="educlvlsecondary"~"Secondary",
        #                         term=="educlvlHigher"~"Higher",
        #                         term=="husedlvlprimary"~"Primary",
        #                         term=="husedlvlsecondary"~"Secondary",
        #                         term=="husedlvlHigher"~"Higher",
        #                         term=="wealthqpoorer"~"Poorer",
        #                         term=="wealthqmiddle"~"Middle",
        #                         term=="wealthqricher"~"Richer",
        #                         term=="wealthqrichest"~"Richest",
        #                         term=="urbanUrban"~"Urban",
        #                         TRUE~term)) |> 
        #   dwplot(group=1,
        #     vline = geom_vline(
        #            xintercept = 1,
        #            colour = "grey60",
        #            linetype = 2)) +
        #   theme_bw() 
      } else if (input$health_outcome2 == "fevtreat" & input$coefficients=="no") {
        fevtreat_model1 |> plot_model()
      }
    )
    
## outputs
    output$descriptive_plot <- renderPlotly(ggplotly(descriptive_plot()))
    output$model_plot <- renderPlotly(ggplotly(model_plot()))
}

# Run the application 
shinyApp(ui = ui, server = server)

##################### Settings ##################
# rm(list=ls())

## load packages
library(tidyverse)
library(knitr)
library(kableExtra)
library(lme4)
library(broom)
library(broom.mixed)
library(haven)
library(plotly)
library(shiny)
library(shinydashboard)
library(sjPlot)
library(freqtables)

## setting working directory   
# PATH="C:/R/age_gap/agegap_app"
# setwd(PATH)

source("prepare_data.R")


################################# UI #######################################
ui <- dashboardPage(
  
  dashboardHeader(title="Master thesis"),

    dashboardSidebar(
        sidebarMenu(
          menuItem("Overview",tabName="overview"),
          menuItem("Descriptive", tabName="Descriptive"),
          menuItem("Model results",
                   menuSubItem("Plot",tabName="plot_results"),
                   menuSubItem("Table",tabName="table_results"),
                   menuSubItem(radioButtons("health_outcome", "Select health outcome",
                                            choices=list("Fever treatment"="fevtreat",
                                                         "Measles vaccination"="measles",
                                                         "Underweight"="underweight"))))
            )),
    
    dashboardBody(
        tabItems(
          tabItem(tabName="overview",
                  fluidRow(box(h3("Partner age gap and child health in Sub-Saharan Africa"),
                               h4("Abstract:"),htmlOutput("abstract"),width=7),
                           box(uiOutput("link"),width=5),
                           box(h4("Outcome variables"),htmlOutput("variables"),width=5))),
            tabItem(tabName="Descriptive",
                    fluidRow(box(tableOutput("descriptive_table"),width=9),
                             box(selectInput("country", "Select country",
                                             choices=c("All",countries)),width=3))),
            tabItem(tabName="plot_results",
                    fluidRow(box(plotlyOutput("model_plot"),width=9,height=550),
                             box(radioButtons("coefficients_plot","Select model",
                                              choices=list("Only age gap"="agediff",
                                                           "Child coefficients"="child",
                                                           "Full model"="full")),width=3))),
            tabItem(tabName="table_results",
                    fluidRow(box(tableOutput("model_table"),height=1550,width=11)))
        )
        
    ))


################################# Server #######################################
server <- function(input, output, session) {
    
    descriptive_table <- reactive(
        
            if(input$country=="All"){
                  data_fevtreat |> 
                    freq_table(agediff5,fevtreat) |> freq_format(recipe="n (percent_row)",digits=1) |> 
                    select(row_cat,col_cat,formatted_stats) |> 
                    pivot_wider(names_from=row_cat,values_from=formatted_stats) |> 
                    slice(match(c("Received treatment","No treatment"),col_cat)) |> 
                rbind(data_measles |> 
                        freq_table(agediff5,measles) |> freq_format(recipe="n (percent_row)",digits=1) |> 
                        select(row_cat,col_cat,formatted_stats) |> 
                        pivot_wider(names_from=row_cat,values_from=formatted_stats) |> 
                        slice(match(c("Vaccinated","Not vaccinated"),col_cat))) |> 
                rbind(data_underweight |> 
                        freq_table(agediff5,underweight) |> freq_format(recipe="n (percent_row)",digits=1) |> 
                        select(row_cat,col_cat,formatted_stats) |> 
                        pivot_wider(names_from=row_cat,values_from=formatted_stats) |> 
                        slice(match(c("Underweight","Normal weight"),col_cat))) |> 
                  kable(col.names=c("Variable","<0","0-4","5-9","10-14","15+")) %>%
                    kable_styling(bootstrap_options=c("striped","hover","condensed")) %>%
                    add_header_above(c(" "=1,"Age group"=5),bold=TRUE) %>%
                    row_spec(0,bold=TRUE) |> column_spec(2:6,width="2.5cm") |> 
                    pack_rows("Fever treatment",1,2) |> pack_rows("Measles vaccination",3,4) |> 
                      pack_rows("Underweight",5,6)
                
                } else {
                  data_fevtreat |> filter(country==input$country) |> 
                    freq_table(agediff5,fevtreat) |> freq_format(recipe="n (percent_row)",digits=1) |> 
                    select(row_cat,col_cat,formatted_stats) |> 
                    pivot_wider(names_from=row_cat,values_from=formatted_stats) |> 
                    slice(match(c("Received treatment","No treatment"),col_cat)) |> 
                    rbind(data_measles |> filter(country==input$country) |> 
                            freq_table(agediff5,measles) |> freq_format(recipe="n (percent_row)",digits=1) |> 
                            select(row_cat,col_cat,formatted_stats) |> 
                            pivot_wider(names_from=row_cat,values_from=formatted_stats) |> 
                            slice(match(c("Vaccinated","Not vaccinated"),col_cat))) |> 
                    rbind(data_underweight |> filter(country==input$country) |> 
                            freq_table(agediff5,underweight) |> freq_format(recipe="n (percent_row)",digits=1) |> 
                            select(row_cat,col_cat,formatted_stats) |> 
                            pivot_wider(names_from=row_cat,values_from=formatted_stats) |> 
                            slice(match(c("Underweight","Normal weight"),col_cat))) |> 
                    kable(col.names=c("Variable","<0","0-4","5-9","10-14","15+")) %>%
                    kable_styling(bootstrap_options=c("striped","hover","condensed")) %>%
                    add_header_above(c(" "=1,"Age group"=5),bold=TRUE) %>%
                    row_spec(0,bold=TRUE) |> column_spec(2:6,width="2.5cm") |> 
                  pack_rows("Fever treatment",1,2) |> pack_rows("Measles vaccination",3,4) |> 
                    pack_rows("Underweight",5,6)
        }
        
    )
    
    model_plot <- reactive(
      
      if (input$health_outcome=="fevtreat" & input$coefficients_plot=="agediff") {
        fevtreat_plot1 |> layout(height=500)
      } else if (input$health_outcome=="fevtreat" & input$coefficients_plot=="child") {
        fevtreat_plot2 |> layout(height=500)
      } else if (input$health_outcome=="fevtreat" & input$coefficients_plot=="full") {
        fevtreat_plot3 |> layout(height=500)
        } else if (input$health_outcome=="measles" & input$coefficients_plot=="agediff") {
          measles_plot1 |> layout(height=500)
        } else if (input$health_outcome=="measles" & input$coefficients_plot=="child") {
          measles_plot2 |> layout(height=500)
        } else if (input$health_outcome=="measles" & input$coefficients_plot=="full") {
          measles_plot3 |> layout(height=500)
        } else if (input$health_outcome=="underweight" & input$coefficients_plot=="agediff") {
          underweight_plot1 |> layout(height=500)
        } else if (input$health_outcome=="underweight" & input$coefficients_plot=="child") {
          underweight_plot2 |> layout(height=500)
        } else if (input$health_outcome=="underweight" & input$coefficients_plot=="full") {
          underweight_plot3 |> layout(height=500)
        }
    )
    
    model_table <- reactive(
      
      if (input$health_outcome=="fevtreat") {
        HTML(fevtreat_table$knitr)
      
      } else if (input$health_outcome=="measles") {
        HTML(measles_table$knitr)
      
      } else if (input$health_outcome=="underweight") {
        HTML(underweight_table$knitr)
      }
    )
    
## outputs
    output$descriptive_table <- renderText(descriptive_table())
    output$model_plot <- renderPlotly((model_plot()))
    output$model_table <- renderUI(model_table())
    
    output$link <- renderUI(tagList(a("Link to article", 
                             href="https://su.figshare.com/articles/preprint/Partner_age_gap_and_child_health_in_Sub-Saharan_Africa/13186829")))
    
    output$abstract <- renderText(HTML(paste(
      "This thesis explores the association between the age gap between parents and health outcomes for children in Sub-Saharan Africa.", 
      "An average man-older age gap between partners has been observed all over the world and is the largest in many Sub-Saharan African countries. A large age gap is common in patriarchal societies and has been associated with less female autonomy and impeded decision-making for the couple, resulting in less contraceptive use and a possible higher risk of interpersonal violence.",
      "This thesis examines another association with age gaps by focusing on the health outcomes for children in families with large and small age gaps between the mother and her partner. It is hypothesized that children will have worse health outcomes in families where the age gap between the mother’s partner and the mother herself is larger than average.",
      "Using data from the Demographic and Health Surveys (DHS), multilevel logistic regression is run to test the association between three health indicators while controlling for confounding variables such as mother’s age, education level and wealth. The health indicators are treatment of fevers, vaccination against measles and underweight.",
      "The results show some statistically significant associations, with all three variables supporting the hypothesis that children in age heterogamous families are doing worse. Children of couples with a larger than average age gap have lower likelihood of being treated for fever or cough, and a higher likelihood of being underweight, and children of couples with a smaller than average age gap have a higher likelihood of having received the first measles vaccination. The results show that the age gap between parents is a factor to take into consideration when studying child health and family structures in Sub-Saharan Africa.",sep="<br/>")))
    
    output$variables <- renderText(HTML(paste("<b>Fever treatment:</b>","The mother was asked if the child
had any fever or cough in the last 14 days, and whether they sought any advice or treatment for
the child. For children under five who reportedly had fever or cough, the variable reports
whether any sort of treatment or advice was sought.",
                                              "<b>Measles vaccination:</b>","The information was taken from the
vaccination card with immunization records for their children if this was available, otherwise
they are not included in the study. The information refers to measles as an individual
vaccination or as a vaccine containing measles. Surviving children either under the age of three or under the age of five were
included depending on the survey country. The first dose of measles vaccination is
recommended between 12 and 15 months of age, so children under age one are excluded and
the reference group for the model is 12-23 months.",
                                              "<b>Underweight:</b>","Indicates if the child has a BMI score indicating underweight according to a
baseline by WHO (WHO | Global Database on Child Growth and Malnutrition, 2020)",
                                              sep="<br/>")))
}



# Run the application 
shinyApp(ui = ui, server = server)

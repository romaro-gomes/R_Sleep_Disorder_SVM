library(here)
library(tidyverse)
library(tidymodels)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(knitr)


data=readRDS(file=here('code/RDATA/data_eda.R'))
model=readRDS(file=here('code/RDATA/model_svm_linear.R'))
data_shiny = subset(head(data,0), select = -c(sleep_disorder))

ui <- dashboardPage(
    dashboardHeader(title ="SVM Model Sleep Disoder Probailities"),
    dashboardSidebar(disable=TRUE),
    
    dashboardBody(
        
        fluidRow(
            column(width= 6,
            selectInput(
                "gender",
                label = "Sex",
                choices =levels(data$gender),
                multiple = FALSE,
                selected =levels(data$gender)[1] 
            ),
            sliderInput(
                'age',
                label ='Age',
                min=18,
                max=99,
                value=25
            ),
            selectInput(
                "occupation",
                label = "Occupation",
                choices =levels(data$occupation),
                multiple = FALSE,
                selected = levels(data$occupation)[1]
            ),
            
            sliderInput(
                'sleep_duration',
                label ='Sleep Duration',
                min=min(data$sleep_duration),
                max=max(data$sleep_duration),
                value=mean(data$sleep_duration) |> round(1) ,
                step = 0.1
            ),
            selectInput(
                "quality_of_sleep",
                label = "Qualidade do sono",
                choices = levels(data$quality_of_sleep) |> sort(),
                selected = levels(data$quality_of_sleep)[1]
            ),
            sliderInput(
                'physical_activity_level',
                label = 'Physical Activity',
                min=min(data$physical_activity_level),
                max=max(data$physical_activity_level),
                value=mean(data$physical_activity_level)
            )),
            
            
            column(width=6, 
            selectInput(
                'stress_level',
                "Stress Level",
                choices = levels(data$stress_level) |> sort(),
                selected = levels(data$stress_level)[1]
            ),
            selectInput(
                'bmi_category',
                "BMI",
                choices =levels(data$bmi_category) |> sort(),
                selected = levels(data$bmi_category)[1]
            ),
            sliderInput(
                'systolic_pressure',
                label = 'Systolic Pressure',
                min=min(data$systolic_pressure),
                max=max(data$systolic_pressure),
                value=mean(data$systolic_pressure)
            ),
            
            sliderInput(
                'diastolic_pressure',
                label = 'Diastolic Pressure',
                min=min(data$diastolic_pressure),
                max=max(data$diastolic_pressure),
                value=mean(data$diastolic_pressure)
            ),
            sliderInput(
                'heart_rate',
                label = 'Heart Rate',
                min=min(data$diastolic_pressure),
                max=max(data$diastolic_pressure),
                value=mean(data$diastolic_pressure)
            ),
            numericInput(
                'daily_steps',
                label = 'Daily Steps',
                min=min(data$daily_steps),
                max=max(data$daily_steps),
                value=mean(data$daily_steps) |> round()
            ))
        ),
        fixedRow(
           
            box(align="center",
            tableOutput('pred'), width = 12,
            title ="Predictions",
            )
           
            
            
        )
        ))
    
    
   

server <- function(input, output) {
    output$pred =renderTable({
        
        data_shiny[1, ]=list(input$gender ,
                             input$age,
                             input$occupation  ,
                             input$sleep_duration,
                             input$quality_of_sleep,
                             input$physical_activity_level,
                             input$stress_level,
                             input$bmi_category,
                             input$systolic_pressure,
                             input$diastolic_pressure,
                             input$heart_rate,
                             input$daily_steps
        )
        
        
        pred = predict(model,data_shiny,type = 'prob') |> round(2) |> table() 
        
        
        return(pred)
    })
}

shinyApp(ui, server)

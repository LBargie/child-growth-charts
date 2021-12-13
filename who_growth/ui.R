library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = shinytheme("superhero"),

    # Application title
    titlePanel("Child Growth Charts"),
    p("Weight, length and head circumeference measurements according to Wolrd Health Organisation growth standards"),
    p("Provide information below to generate growth charts for your child's measurements on the right-hand side"),

    # Sidebar with user inputs for date of birth/measurement, gender, weight, length, head circumference
    sidebarLayout(
        
        sidebarPanel(
            
            dateInput("dob",
                      "Date of Birth (yyyy-mm-dd)",
                      format = "yyyy-mm-dd"),
            
            dateInput("dom",
                      "Date of Measurement (yyyy-mm-dd)",
                      format = "yyyy-mm-dd"),
            
            selectInput("gender",
                        "Gender",
                        choices = c("Boy", "Girl")),
            
            numericInput("wt",
                         "Weight (in kg)",
                         value = ""),
            
            numericInput("lh",
                         "Length (in cm)",
                         value = ""),
            
            numericInput("hc",
                         "Head Circumference (in cm)",
                         value = "")
        ),

        # Show growth charts
        mainPanel(
            
            tabsetPanel(
                type = "tabs",
                
            tabPanel("Summary",
                     
                     tableOutput("wtTable"),
                     
                     tableOutput("lhTable"),
                     
                     tableOutput("hcTable")),
            
            tabPanel("Charts", 
                     plotOutput("wtPlot",
                     height = 600, 
                     width = 1000),
            
            br(),
            
            plotOutput("lhPlot",
                       height = 600, 
                       width = 1000),
            
            br(),
            
            plotOutput("hcPlot",
                       height = 600, 
                       width = 1000)
            )
        )
    )
)
)
)

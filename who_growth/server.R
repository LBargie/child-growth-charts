library(shiny)
library(tidyverse)
library(lubridate)
library(ggrepel)
source("Growth chart functions.R")

# For plotting growth charts
files1 <-
    c("Boys wt 1yr.csv", 
      "Boys lh.csv", 
      "Boys hc.csv", 
      "Girls wt 1yr.csv", 
      "Girls lh.csv", 
      "Girls hc.csv", 
      "Boys wt 1yr plus.csv", 
      "Girls wt 1yr plus.csv")

df1 <- map(files1, read_csv)

# For percentile calculation
files2 <-
    list("https://raw.githubusercontent.com/dirkschumacher/anthro/master/data-raw/growthstandards/weianthro.txt",
         "https://raw.githubusercontent.com/dirkschumacher/anthro/master/data-raw/growthstandards/lenanthro.txt",
         "https://raw.githubusercontent.com/dirkschumacher/anthro/master/data-raw/growthstandards/hcanthro.txt")

df2 <- map(files2, read_tsv)

df2 <- df2 %>%
    map(~ mutate(.data = ., sex = case_when(.$sex == 1 ~ "m",
                                            .$sex == 2 ~ "f")))

df2[[2]] <- df2[[2]] %>%
    filter(loh == "L")

# Define server functions to calculate percentiles and plot the growth charts
shinyServer(function(input, output) {
    

    # Calculates age from DOB in days (for percentile calc), weeks and months
    days <- reactive({
      
      age(input$dom, input$dob, "days")
        
    })
    
    weeks <- reactive({
      
      age(input$dom, input$dob, "weeks")
        
    })
    
    months <- reactive({
      
      age(input$dom, input$dob, "months")
        
    })
    
    setGender <- reactive({
      
      if (input$gender == "Boy"){
        
        gender <- "m"
        
      }else{
        
        gender <- "f"
        
      }
      
      return(gender)
      
    })

    # percentile() calculates percentile for each measurement    
    wtPerc <- reactive({ 
      
      req(df2[[1]])
      req(days())
      req(input$wt)
      req(setGender())
      
      percentile(df2[[1]], days(), input$wt, setGender())
      
    })
    
    lhPerc <- reactive({
      
      req(df2[[2]])
      req(days())
      req(input$lh)
      req(setGender())
      
      percentile(df2[[2]], days(), input$lh, setGender())
      
    })
    
    hcPerc <- reactive({
      
      req(df2[[3]])
      req(days())
      req(input$hc)
      req(setGender())
      
      percentile(df2[[3]], days(), input$hc, setGender())
      
    })
    
    wtData <- reactive({
      
      if (input$gender == "Boy" & weeks() <= 52){
        
        wtdf <- df1[[1]]
        
      }else if (input$gender == "Boy" & weeks() > 52){
        
        wtdf <- df1[[7]]
        
      }else if (input$gender == "Girl" & weeks() <= 52){
        
        wtdf <- df1[[4]]
        
      }else if (input$gender == "Girl" & weeks() > 52){
        
        wtdf <- df1[[8]]
        
      }
      
      return(wtdf)
      
    })
    
    lhhcData <- reactive({
      
      if (input$gender == "Boy"){
        
        lhdf <- df1[[2]]
        
        hcdf <- df1[[3]]
        
      }else{
        
        lhdf <- df1[[5]]
        
        hcdf <- df1[[6]]
        
      }
      
      return(list(lhdf, hcdf))
      
    })
    
    # Convert weight in kg to pounds and ounces
    lb <- reactive({
      
      round(input$wt*2.205, 2)
      
    })
    
    oz <- reactive({
      
      round((lb() - as.numeric(str_split(lb(), "\\.", simplify = TRUE)[1]))*16, 2)
      
    })
    
    output$wtTable <- renderTable({
      
      req(input$wt)
    
      data.frame(`Weight (kg)` = paste(input$wt, " (", lb(), "lb", oz(), "oz)"),
                 `Age (Weeks)` = weeks(),
                 `Age (Months)` = months(),
                 `Percentile (%)` = wtPerc(),
                 check.names = FALSE)
      
    }, 
    bordered = TRUE,
    width = 750,
    align = "c",
    digits = 0)
    
    output$lhTable <- renderTable({
      
      req(input$lh)
      
      data.frame(`Length (cm)` = input$lh,
                 `Age (Weeks)` = weeks(),
                 `Age (Months)` = months(),
                 `Percentile (%)` = lhPerc(),
                 check.names = FALSE)
      
    },
    bordered = TRUE,
    width = 750,
    align = "c",
    digits = 0)
    
    output$hcTable <- renderTable({
      
      req(input$hc)
      
      data.frame(`Head Circumference (cm)` = input$hc,
                 `Age (Weeks)` = weeks(),
                 `Age (Months)` = months(),
                 `Percentile (%)` = hcPerc(),
                 check.names = FALSE)
      
      
    },
    bordered = TRUE,
    width = 750,
    align = "c",
    digits = 0)
    
    output$wtPlot <-
      
      renderPlot({
        
        p <- growthChart(wtData(), "Age (Weeks)", "Weight (Kg)") + 
          addMeasurements(weeks(), input$wt, wtPerc())
        
        if (weeks() <= 52){
          
          labelling <- list(
            scale_x_continuous(expand = c(0,NA), breaks = seq(0,52, 4), limits = c(0,54)),
            annotate("label",
                     x = 10.5,
                     y = 11,
                     label = paste("Gender:", input$gender,
                                   "\nAge:", weeks(), "weeks",
                                   "\nWeight:", input$wt, "kg",paste0("(",lb()," lb ", oz(), " oz)")),
                     colour = "blue",
                     size = 6,
                     label.padding = unit(0.75, "lines"))
          )
          
          
        }else{
          
          labelling <- list(
            scale_x_continuous(expand = c(52,NA), breaks = seq(52,260,8), limits = c(52,270)),
            annotate("label",
                     x = 100,
                     y = 22.5,
                     label = paste("Gender:", input$gender,
                                   "\nAge:", weeks(), "weeks",paste0("(",months()," months)"),
                                   "\nWeight:", input$wt, "kg",paste0("(",lb()," lb ", oz(), " oz)")),
                     size = 6,
                     colour = "blue",
                     label.padding = unit(0.75, "lines"))
          )
          
        }
        
        p + labelling
        
      })
    
    output$lhPlot <-
      
      renderPlot({
        
        growthChart(lhhcData()[[1]], "Age (Months)", "Length (cm)") + 
          addMeasurements(months(), input$lh, lhPerc()) +
          scale_x_continuous(expand = c(0,NA), breaks = seq(0, 24, 1), limits = c(0, 26)) +
          scale_y_continuous(expand = c(0,NA)) +
          annotate("label", 
                   x = 3, 
                   y = 85, 
                   label = paste("Gender:", input$gender,
                                 "\nAge:", months(),"months",
                                 "\nLength:", input$lh, "cm"),
                   size = 6,
                   colour = "blue",
                   label.padding = unit(0.75, "lines"))
        
      })
    
    output$hcPlot <-
      
      renderPlot({
        
        growthChart(lhhcData()[[2]], "Age (Months)", "Circumference (cm)") + 
          addMeasurements(months(), input$hc, hcPerc()) +
          scale_x_continuous(expand = c(0,NA), breaks = seq(0, 60, 2), limits = c(0,62)) +
          scale_y_continuous(expand = c(0,NA)) +
          annotate("label", 
                   x = 50, 
                   y =40, 
                   label = paste("Gender:", input$gender,
                                 "\nAge:", months(),"months",
                                 "\nLength:", input$hc, "cm"),
                   size = 6,
                   colour = "blue",
                   label.padding = unit(0.75, "lines"))
        
      })
    
})

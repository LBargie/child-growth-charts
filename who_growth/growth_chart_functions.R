### Functions for WHO growth chart app ###

# Function for calculating weight percentile using LMS method
percentile <- function(df, x, y, gender){
  
  # Where 'x' is age in days
  # Where 'y' is the measurement, i.e. weight, length or head circumference
  # Where 'gender' is 'm' or 'f'
  
  # Get L, M, S values from dataframe for algorithm
  l <- as.numeric(df[(df$age == x) & (df$sex == gender), "l"])
  m <- as.numeric(df[(df$age == x) & (df$sex == gender), "m"])
  s <- as.numeric(df[(df$age == x) & (df$sex == gender), "s"])
  
  value <- round(pnorm(((((as.numeric(y)/m)**l) - 1)/(l*s)))*100, 0)
  
  return(value)
  
}

# Functions for creating the growth charts.

# First functions creates the generic template with the percentiles displayed.

# Second function adds the measurements supplied by the user to the growth chart.
growthChart <- function(df, xlabel, ylabel){
  
  ggplot(data = df, aes(df$Age)) +
    geom_line(aes(y = df$P1), colour = "skyblue", linetype = "dashed") +
    geom_line(aes(y = df$P10), colour = "skyblue", linetype = "dashed") +
    geom_line(aes(y = df$P25), colour = "skyblue", linetype = "dashed") +
    geom_line(aes(y = df$P50), colour = "skyblue", linetype = "dashed") +
    geom_line(aes(y = df$P75), colour = "skyblue", linetype = "dashed") +
    geom_line(aes(y = df$P90), colour = "skyblue", linetype = "dashed") +
    geom_line(aes(y = df$P99), colour = "skyblue", linetype = "dashed") +
    annotate("text",
             x = max(df$Age), 
             y = c(max(df$P1), max(df$P10), max(df$P25), max(df$P50), max(df$P75), max(df$P90), max(df$P99)), 
             label = c("1st", "10th", "25th", "50th", "75th", "90th", "99th"),
             vjust = -0.25) +
    labs(x = xlabel,
         y = ylabel,
         title = case_when(min(df$Age) == 52 & df$Measurement == "wt" & df$Gender == "boy" ~ "BOYS WEIGHT (kg) 1-5 years",
                           min(df$Age) == 52 & df$Measurement == "wt" & df$Gender == "girl" ~ "GIRLS WEIGHT (kg) 1-5 years",
                           df$Measurement == "wt" & df$Gender == "boy" ~ "BOYS WEIGHT (kg) 0-1 year",
                           df$Measurement == "lh" & df$Gender == "boy" ~ "BOYS LENGTH (cm) 0-2 years",
                           df$Measurement == "hc" & df$Gender == "boy" ~ "BOYS HEAD CIRCUMFERENCE (cm) 0-5 years",
                           df$Measurement == "wt" & df$Gender == "girl" ~ "GIRLS WEIGHT (kg) 0-1 years",
                           df$Measurement == "lh" & df$Gender == "girl" ~ "GIRLS LENGTH (cm) 0-2 years",
                           df$Measurement == "hc" & df$Gender == "girl" ~ "GIRLS HEAD CIRCUMFERENCE (cm) 0-5 years")) +
    theme_bw() +
    theme(plot.title = element_text(colour = "red",
                                    size = 16),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16))
  
}

addMeasurements <- function(x, y, percentile){
  
  
  list(geom_point(aes(x = x, y = y),
                  size = 4,
                  shape = 13),
       geom_label_repel(data = data.frame(x = x, y = y),
                        aes(x = x, y = y),
                        label = paste0(percentile, "%"),
                        size = 8,
                        colour = "red",
                        min.segment.length = 0,
                        box.padding = 1,
                        direction = "y",
                        hjust = 0))
  
}

# Function for calculating age based on DOB and DOM
age <- function(x, y, units){
  
  round(time_length(ymd(x) - ymd(y), unit = units), 0)
  
}






library(shiny)
library(plotly)
library(tidyverse)
library(plotly)
library(ggplot2)
library(lubridate)

# Load data
dat <- read.csv('merged_data.csv')

# only select the columns of interest
df <- dat[c('state','date','positive','negative','recovered','death','totalTestResults','Location','Governor.Political.Affiliation','State.Senate.Majority.Political.Affiliation','State.House.Majority.Political.Affiliation','State.Attorney.General.Political.Affiliation','State.Insurance.Commissioner.Political.Affiliation','closed')]
# select only the latest date
df$date <- as.Date(df$date)
df <- df[which(df$date==max(df$date)),]
# turn state into factor 
df$state <- factor(df$state)
df$Governor.Political.Affiliation <- factor(df$Governor.Political.Affiliation)
df$hover <- with(df, paste(state, '<br>',positive))
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

ui <- fluidPage(
  
  # Application title
  titlePanel("COVID cases in US"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Governor.Political.Affiliation",
        label = "Governor.Political.Affiliation",
        choices=df$Governor.Political.Affiliation,
        selected=1,
        multiple=FALSE
      )),
    
    # Show a plot of the generated distribution
    mainPanel(
      plot_geo(df, locationmode = 'USA-states') %>%
      add_trace(text = ~hover, locations = ~state,
                               color = ~Governor.Political.Affiliation
      ) %>%
        layout(
          title = 'lallaa',
          geo = g
        ))))


server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)

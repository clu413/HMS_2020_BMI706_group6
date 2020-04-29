#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(readr)
library(plotly)
library(tidyr)
merged_data <- read_csv("merged_data.csv")
merged_data$date_diff <- as.numeric(merged_data$date - merged_data$StateClosureStartDate)
merged_data$percent_positive <- merged_data$positive/merged_data$total

percent_change <- function(row, value, df) {
    #browser()
    df <- subset(df, df$state == row['state'])
    df <- subset(df, df$date < as.Date(row['date'])) %>% arrange(desc(date)) %>% slice(1)
    if (nrow(df) > 0) {
        return ((as.numeric(row[value])-df[,value][[1,1]])/df[,value][[1,1]])
    }
    else {
        return (NA)
    }
}

merged_data$positive_percent_change <- apply(merged_data, 1, percent_change, 'positive', merged_data)
merged_data$percent_positive_percent_change <- apply(merged_data, 1, percent_change, 'percent_positive', merged_data)
merged_data$hospitalized_percent_change <- apply(merged_data, 1, percent_change, 'hospitalized', merged_data)
merged_data$death_percent_change <- apply(merged_data, 1, percent_change, 'death', merged_data)

plot_data <- merged_data %>% pivot_longer(c(positiveIncrease, positive_percent_change, percent_positive, percent_positive_percent_change, hospitalizedIncrease, hospitalized_percent_change, deathIncrease, death_percent_change))

# Define UI for application that draws cases over time
ui <- fluidPage(

    # Application title
    titlePanel("Case Rate Over Time"),

    # Sidebar with a slider input to select data, select state, slide input
    sidebarLayout(
        sidebarPanel(
            selectInput("name", "Plot Value:", choices = unique(plot_data$name)),
            pickerInput("state", "Select a state:", 
                        choices = unique(plot_data$state), 
                        options = list(`actions-box` = TRUE), 
                        selected = unique(plot_data$state), multiple = T),
            sliderInput("innoculation",
                        "Innoculation Time (days):",
                        min = 1,
                        max = 10,
                        value = 5)
        ),

        # Show a plot
        mainPanel(
           plotlyOutput("plot")
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$plot <- renderPlotly({
        #linear models
        plot_data_subset <- subset(plot_data, state %in% input$state & name %in% input$name)
        plot_data_subset_before <- subset(plot_data_subset, value < Inf & !is.na(value) & date_diff <= input$innoculation)
        plot_data_subset_after <- subset(plot_data_subset, value < Inf & !is.na(value) & date_diff > input$innoculation)
        if (nrow(plot_data_subset > 0)) {
            l1 <- lm(value ~ date_diff, data = plot_data_subset_before)
            l2 <- lm(value ~ date_diff, data = plot_data_subset_after)
        }
        
        # draw plot
        plot_ly() %>% 
            add_lines(data= plot_data_subset, x = ~date_diff, y= ~value, color=~state, text = ~state, line=(list(width = 1, opacity = 0.8))) %>%
            add_trace(data = plot_data_subset_before, x = plot_data_subset_before$date_diff, y = predict(l1), type = 'scatter', mode = 'lines', line=list(color = 'purple', width = 4), name = "Trend Until Schools Closed") %>%
            add_trace(data = plot_data_subset_after, x = plot_data_subset_after$date_diff, y = predict(l2), type = 'scatter', mode = 'lines', line=list(color = 'blue', width = 4), name = "Trend After Schools Closed") %>%
            layout(shapes = list(type = "rect", fillcolor = "blue", line=list(color="blue"), opacity = 0.2, 
                                 x0=0, x1=input$innoculation, xref = 'x', 
                                 y0=0, y1= 1, yref='paper')) #%>%
            # layout(annotations = list(
            #     list(
            #         text= "Trend Until Schools Closed",
            #         xref = 'paper',
            #         yref = 'y',
            #         x = 0.05,
            #         y = predict(l1, newdata = plot_data_subset_before %>% arrange(date_diff))[1],
            #         xanchor = 'right',
            #         yanchor = 'middle',
            #         showarrow = FALSE),
            #     list(text= "Trend /after Schools Closed",
            #          xref = 'paper',
            #          yref = 'y',
            #          x = 0.95,
            #          y = predict(l2, newdata = plot_data_subset_after %>% arrange(desc(date_diff)))[1],
            #          xanchor = 'left',
            #          yanchor = 'middle',
            #          showarrow = FALSE)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

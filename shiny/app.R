library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)

#---load data---
dat.change <- read_csv(url('https://raw.githubusercontent.com/luchenyue95/HMS_2020_BMI706_group6/master/data/fixed_data_percent_change.csv'))
dat.filt   <- read_csv(url('https://raw.githubusercontent.com/luchenyue95/HMS_2020_BMI706_group6/master/data/filtered_data.csv'))

#---preprocess data---
preproc <- function(df) {
    df$date <- as.Date(df$date)
    df$state <- factor(df$state)
    df$Governor.Political.Affiliation <- factor(df$Governor.Political.Affiliation)
    return(df)
}
dat.change <- preproc(dat.change)
dat.filt   <- preproc(dat.filt)

#---UI---
ui <- fluidPage(
    # Application title
    titlePanel('What is a good title?'),
    
    sidebarLayout(
        sidebarPanel(
            selectInput('name', 'Select a category:',
                        c('Positive Increase'='positiveIncrease',
                          'Positive % Change'='positive_percent_change',
                          '% Positive'='percent_positive',
                          'Hospitalized Increase'='hospitalizedIncrease',
                          'Hospitalized % Change'='hospitalized_percent_change',
                          'Death Increase'='deathIncrease',
                          'Death % Change'='death_percent_change')),
            pickerInput('state', 'Select a state:', 
                        choices = unique(levels(dat.change$state)), 
                        options = list(`actions-box` = TRUE), 
                        selected = unique(dat.change$state), multiple = T),
            sliderInput('innoculation',
                        'Innoculation Time (days):',
                        min = 1,
                        max = 10,
                        value = 5)
        ),
        mainPanel(
            plotlyOutput('map'),
            plotlyOutput('pcp'),
            plotlyOutput('lineplot'),
            plotlyOutput('heatmap'),
        )
    )
)

# Define server logic
server <- function(input, output) {
    #--Chen--
    output$map <- renderPlotly({
        df <- dat.filt[which(dat.filt$date==max(dat.filt$date)),]
        df$hover <- with(df, paste(state, '<br>',positive))
        plot_geo(df, locationmode = 'USA-states') %>%
            add_trace(text = ~hover, locations = ~state,
                      color = ~Governor.Political.Affiliation
            ) %>%
            layout(
                title = 'lallaa',
                geo = g <- list(
                    scope = 'usa',
                    projection = list(type = 'albers usa'),
                    showlakes = TRUE,
                    lakecolor = toRGB('white')
                )
            )
    })
    
    #--Dany--
    output$pcp <- renderPlotly({
      
        df <- dat.filt[which(dat.filt$date==max(dat.filt$date)),] %>% as.data.frame() %>% filter(state %in% input$state)
        party <- unique(df$Governor.Political.Affiliation)
        pcdat <- df %>%
            select(state, total, StateClosureStartDate, Governor.Political.Affiliation) %>%
            subset(!is.na(Governor.Political.Affiliation)) %>% filter(state %in% input$state)
        pcdat$StateClosureStartDate <- as.factor(pcdat$StateClosureStartDate)
        pcdat$state <- as.factor(pcdat$state)
        factor_cols <- sapply(pcdat, is.factor)
        pcdat[, factor_cols] <- sapply(pcdat[, factor_cols], unclass)
        pcdat$state <- as.factor(pcdat$state) %>% unclass()
        
        pcdat %>%
            plot_ly() %>%
            add_trace(type = 'parcoords', line = list(color = ~Governor.Political.Affiliation, colorscale = list(c(0,'blue'),c(1,'red'))),
                      dimensions = list(
                          list(range = c(1,length(df$state)),
                               tickvals = c(1:length(df$state)),
                               label = 'state',
                               ticktext = c(paste(df$state)),
                               #multiselect = TRUE,
                               #constraintrange = list(c(1,1), c(25,25), c(49,50)),
                               #constraintrange = c(state_restraint),
                               values = ~state ),
                          
                          list(range = c(~min(total),~max(total)),
                               label = 'total tests',
                               tickvals = c(~min(total),~max(total)),
                               values= ~total),
                          
                          list(range=c(1,2),
                               tickvals = c(1, 2),
                               label = 'party',
                               ticktext = c(paste(party)),
                               values = ~Governor.Political.Affiliation ),
                          
                          list(range = c(1,7),
                               tickvals = c(1,7),
                               label = 'school closure date',
                               ticktext = c(paste(dat.filt$StateClosureStartDate)),
                               values = ~StateClosureStartDate
                          )
                      )
                      
                      
            )
        
    })
    
    #--Kath--
    heatmapMatrix <- reactive({
        # obtain 30 timestamps after school closure (including school closure date)
        heatmap.width <- 30
        cat <- input$name
        states <- unique(dat.change$state)
        mat <- matrix(rep(NA, length(states)*heatmap.width), nrow=length(states))
        for (i in 1:length(states)) {
            state <- states[i]
            # filter by state
            df <- dat.change[dat.change$state == state & dat.change$name == cat,]
            closure_date <- df$StateClosureStartDate[1] - 10
            df <- df %>%
                filter(date >= closure_date) %>%
                arrange(date)
            mat[i,] <- df$value[1:heatmap.width]
        }
        rownames(mat) <- states

        return(mat)
    })
    
    output$heatmap <- renderPlotly({
        mat <- heatmapMatrix()
        plot_ly(
            y=rownames(mat),
            x=seq(1,30),
            z=mat, type='heatmap',
            height=800,
        ) %>%
            layout(
                xaxis=list(
                    title='Days',
                    dtick=1,
                    zeroline=F,
                    showline=F,
                    showticklabels=T,
                    showgrid=T
                ),
                yaxis=list(
                    title='States',
                    dtick=1,
                    zeroline=F,
                    showline=F,
                    showticklabels=T,
                    showgrid=T
                )) %>%
            add_segments(x=10, xend=10, y='AK', yend='WY')
    })
    
    #--Jon--
    output$lineplot <- renderPlotly({
        #linear models
        dat_subset <- subset(dat.change, state %in% input$state & name %in% input$name)
        dat_subset_before <- subset(dat_subset, value < Inf & !is.na(value) & date_diff <= input$innoculation)
        dat_subset_after <- subset(dat_subset, value < Inf & !is.na(value) & date_diff > input$innoculation)
        if (nrow(dat_subset > 0)) {
            l1 <- lm(value ~ date_diff, data = dat_subset_before)
            l2 <- lm(value ~ date_diff, data = dat_subset_after)
        }
        
        # draw plot
        plot_ly() %>% 
            add_lines(data= dat_subset, x = ~date_diff, y= ~value, color=~state, text = ~state, line=(list(width = 1, opacity = 0.8))) %>%
            add_trace(data = dat_subset_before, x = dat_subset_before$date_diff, y = predict(l1), type = 'scatter', mode = 'lines', line=list(color = 'purple', width = 4), name = "Trend Until Schools Closed") %>%
            add_trace(data = dat_subset_after, x = dat_subset_after$date_diff, y = predict(l2), type = 'scatter', mode = 'lines', line=list(color = 'blue', width = 4), name = "Trend After Schools Closed") %>%
            layout(shapes = list(type = "rect", fillcolor = "blue", line=list(color="blue"), opacity = 0.2, 
                                 x0=0, x1=input$innoculation, xref = 'x', 
                                 y0=0, y1= 1, yref='paper'))
    })
}

#--Run the application--
shinyApp(ui = ui, server = server)

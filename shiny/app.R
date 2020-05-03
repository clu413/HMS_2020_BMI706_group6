library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(lme4)
library(shinyjs)
library(RColorBrewer)

#---load data---
dat.change <- read_csv('../data/fixed_data_percent_change.csv')
dat.filt   <- read_csv('../data/filtered_data.csv')

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
  titlePanel('When did US states close their schools when the COVID pandemic hit?'),
  
  fluidRow(
    column(4, sidebarPanel(
      selectInput('name', 'Select a value to display:',
                  c('Positive Increase'='positiveIncrease',
                    'Positive % Change'='positive_percent_change',
                    '% Positive'='percent_positive',
                    'Total New Tests' = 'totalTestResultsIncrease',
                    'Total % change' = 'total_tests_percent_change',
                    'Hospitalized Increase'='hospitalizedIncrease',
                    'Hospitalized % Change'='hospitalized_percent_change',
                    'Death Increase'='deathIncrease',
                    'Death % Change'='death_percent_change'
                  )),
      selectInput('category', 'Color by:',
                  c('None' = 'state',
                    'Governor Political Affiliation'='Governor.Political.Affiliation',
                    'Region'='Region',
                    'Time of Closure'='ClosureDateCat'
                  )),
      pickerInput('state', 'Select states:', 
                  choices = unique(levels(dat.change$state)), 
                  options = list(`actions-box` = TRUE), 
                  selected = unique(dat.change$state), multiple = T),
      sliderInput('innoculation',
                  'Innoculation Time (days):',
                  min = 1,
                  max = 10,
                  value = 5),
      materialSwitch('normalize', label = 'Normalize by state population?', status='primary'),
      width = 12
    )),
    column(8, plotlyOutput('pcp'))),
  
  fluidRow(
    # column(4, plotlyOutput('lineplot')),
    column(6, plotlyOutput('map')),
    column(6, plotlyOutput('lineplot'))
    )
)


# Define server logic
server <- function(input, output, session) {
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
                      
                      
            ) #%>% onRender("
            # function(el) {
            #     $('.axis-title').click(function() {
            #         Shiny.onInputChange('name', 'percent_positive');
            #         $(this).css('fill', 'red');
            #     });
            # }
            #     
            # ")
        
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
        #subset by states selected
        dat_subset <- subset(dat.change, state %in% input$state & name %in% input$name & positive > 100)
        
        # normalize
        if (input$normalize == TRUE & input$category %in% c('positiveIncrease', 'totalTestResultsIncrease', 'hospitalizedIncrease', 'deathIncrease')) {
          dat.change$value <- dat.change$value / (dat_subset$POPESTIMATE2019/1e5)
          dat_subset$value <- dat_subset$value / (dat_subset$POPESTIMATE2019/1e5)
          y.label <- paste(input$name, "per 100K")
        }
        else {
          y.label <- input$name
        }
       
        #linear models
        dat_subset_before <- subset(dat_subset, value < Inf & !is.na(value) & date_diff <= input$innoculation)
        dat_subset_after <- subset(dat_subset, value < Inf & !is.na(value) & date_diff > input$innoculation)
        if (nrow(dat_subset) > 0) {
          if(input$category != 'state') {
              dat_subset_before$fv <- dat_subset_before %>% lm(formula(paste("value ~ date_diff * ", input$category)), ., na.action = na.exclude) %>% fitted.values()
              dat_subset_after$fv <- dat_subset_after %>% lm(formula(paste("value ~ date_diff * ", input$category)), ., na.action = na.exclude) %>% fitted.values()
              colorby <- input$category
              if(input$category == 'Governor.Political.Affiliation') {
                colorlist = c("blue", "red")
              }
              else {
                colorlist = c("darkgreen","gold", "darkred", "purple")
              }
              
          }
          else {
              dat_subset_before$fv <- dat_subset_before %>% lm(value ~ date_diff, ., na.action = na.exclude) %>% fitted.values()
              dat_subset_after$fv <- dat_subset_after %>% lm(value ~ date_diff, ., na.action = na.exclude) %>% fitted.values()
              colorby <- 1
              colorlist = colorRampPalette(brewer.pal(8, "Dark2"))(length(input$state))
          }
        }
        
        # draw plot
        # subset(dat.change, name %in% input$name & positive > 100) %>% group_by(state) %>% plot_ly(x = ~date_diff, y=~value) %>%
        #   add_lines(text=~state, alpha = 0.2, color = "lightgrey", showlegend=FALSE) %>%
        #   add_fun(function(plot) {
        #        plot %>% filter(state %in% input$state) %>%
        #        add_lines(x = ~date_diff, y=~value, name = formula(paste0("~", input$category)), color=formula(paste0("~", input$category)), colors=colorlist, alpha = 1, inherit = FALSE)
        # 
        #    }) %>%
        g <- ggplot(subset(dat.change, name %in% input$name & positive > 100)) + 
          geom_line(aes(x = date_diff, y=value, group = state), alpha = 0.1, size = 0.5, color = 'lightgrey') + theme_minimal() +             
          geom_rect(xmin = 0, xmax=input$innoculation, ymin=0, ymax = 12000, size=0, fill = "lightblue", alpha = 0.5) +
          scale_color_manual(values = colorlist) + 
          labs(title = "Activity over time", x="Days from school closure", y = y.label)
        if (nrow(dat_subset)>0) {
          g <- g+ geom_line(data = dat_subset, alpha = 0.2, size=0.5, aes_string(x = 'date_diff', y='value', group = 'state', color = input$category))
          if (colorby == 1) {
            g <- g + geom_smooth(data = dat_subset_before, size = 1, aes(x = date_diff, y=value, group = 1), color = 'blue', method = lm) +
                     geom_smooth(data = dat_subset_after, size = 1, aes(x = date_diff, y=value, group = 1), color = 'purple', method = lm)
          }
          else {
            g <- g+ geom_smooth(data = dat_subset_before, size = 1, aes_string(x = 'date_diff', y='value', color = colorby), method = lm) +
                    geom_smooth(data = dat_subset_after, size = 1, aes_string(x = 'date_diff', y='value', color = colorby), method = lm)
          }
        }
        ggplotly(g) %>% layout(margin = list(b=90)) %>% 
          add_annotations(x=0.5, xref='paper', xanchor='center',
                          y=-0.3, yref='paper', font=list(size = 10),
                          text = "Dashed rine represents linear models before and after policy implementation",
                          showarrow = FALSE) %>%
          add_annotations(x=input$innoculation/2, xref='x',y=1, yref='paper', xanchor='center', font=list(size=10), text="Innoculation \nperiod", showarrow=FALSE)
          #shapes = list(type = "rect", fillcolor = "blue", line=list(color="blue"), opacity = 0.2,
                     #                               x0=0, x1=input$innoculation, xref = 'x',
                      #                              y0=0, y1= 1, yref='paper'))
                                      # yaxis = list(title = y.label),
                                      # margin = list(b=90),

        # ggplotly(g) %>%
        #   add_lines(data = dat_subset_before %>% group_by_at(input$category),
        #             x = dat_subset_before$date_diff,
        #             y = ~fv,
        #             inherit = FALSE,
        #             colors= colorlist2,
        #             line=list(width = 4, dash = 'dash'),
        #             name = "Trend Until Schools Closed", showlegend = FALSE) %>%
        #   add_lines(data = dat_subset_after  %>% group_by_at(input$category),
        #             x = dat_subset_after$date_diff,
        #             y = ~fv,
        #             inherit = FALSE,
        #             colors=colorlist2,
        #             line=list(width = 4, dash = 'dash'),
        #             name = "Trend After Schools Closed", showlegend = FALSE)

        # dat_subset %>% group_by(state) %>% plot_ly(x = ~date_diff, y=~value) %>%
        #     add_lines(line=(list(width = 1, opacity = 0.6, color = 'lightgrey'))) %>%
        #     add_trace(data = dat_subset_before,
        #               x = dat_subset_before$date_diff,
        #               y = ~fv,
        #               type = 'scatter', mode = 'lines', color=colorby, inherit = FALSE,
        #               line=list(width = 4, dash = 'dash', colors= colorlist),
        #               name = "Trend Until Schools Closed", showlegend = FALSE) %>%
        #     add_trace(data = dat_subset_after,
        #               x = dat_subset_after$date_diff,
        #               y = ~fv,
        #               type = 'scatter', mode = 'lines', color=colorby, inherit = FALSE,
        #               line=list(width = 4, dash = 'dash', colors=colorlist),
        #               name = "Trend After Schools Closed", showlegend = FALSE) %>%

        
        

    })
    output$click <- renderPrint({
      d<- event_data("plotly_relayout")
      if (is.null(d) == T) return (NULL) else{
        str(d)
        #        updateSelectInput(session, 'state', selected = d)
      }
      
    })
}

#--Run the application--
shinyApp(ui = ui, server = server)


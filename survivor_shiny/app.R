#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(survivoR)
library(tidyverse)
library(plotly)
library(shinythemes)




confessionals_df = confessionals %>% 
  filter(version == "US") %>%
  dplyr::select(-c(version, version_season, season_name)) %>%
  rename(Episode = episode, Castaway = castaway)

confessionals_per_season = confessionals_df %>%
  group_by(season, Castaway) %>%
  summarize(sum_confessional = sum(confessional_count),
            mean_confessionals = mean(confessional_count))

seasons = list()
for(i in 1:46)
{
  seasons[[i]] = confessionals_df %>%
    filter(season == i) %>% 
    group_by(Castaway)
}

plots = list()
for(i in 1:46)
{
  plots[[i]] = ggplotly(ggplot(data = seasons[[i]], aes(x = Episode, y = confessional_count, color = Castaway)) +
                          geom_line() +
                          geom_point() +
                          ggtitle(paste0("Confessionals per Episode in Season ", i))) %>%
    layout(xaxis = list(title = 'Episode'), yaxis = list(title = 'Number of Confessionals'))
}

plots_2 = list()

seasons_2 = list()
for(i in 1:46)
{
  seasons_2[[i]] = confessionals_per_season %>%
    filter(season == i) %>%
    arrange(-sum_confessional) %>%
    mutate(Castaway = as.factor(Castaway))
}


for(i in 1:46)
{
  plots_2[[i]] = ggplotly(ggplot(data = seasons_2[[i]], aes(Castaway, sum_confessional, fill = Castaway)) +
                            geom_col() + 
                            ggtitle(paste0("Number of Confessionals per Contestant in Season ", i)) +
                            xlab("Castaway") + ylab("Number of Confessionals") + 
                            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))) %>%
    layout(xaxis = list(title = 'Castaway'), yaxis = list(title = 'Number of Confessionals'))
}

plots_3 = list()

seasons_3 = list()
for(i in 1:46)
{
  seasons_3[[i]] = confessionals_per_season %>%
    filter(season == i) %>%
    arrange(-mean_confessionals) %>%
    mutate(Castaway = as.factor(Castaway))
}


for(i in 1:46)
{
  plots_3[[i]] = ggplotly(ggplot(data = seasons_3[[i]], aes(Castaway, mean_confessionals, fill = Castaway)) +
                            geom_col() + 
                            ggtitle(paste0("Mean # of Confessionals per Contestant (While on Show) in Season ", i)) +
                            xlab("Castaway") + ylab("Mean Number of Confessionals") + 
                            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))) %>%
    layout(xaxis = list(title = 'Castaway'), yaxis = list(title = 'Mean Number of Confessionals'))
} 

ui <- fluidPage(
  theme = shinytheme("united"),
  tags$style('body { background-color: white;'),
  tags$div(class = "jumbotron text-center bg-primary", style = "margin-bottom: 30px;margin-top:0px;background-color: #b1dab1;",
           tags$h2(class = 'jumbotron-heading', style = 'margin-bottom:0px;margin-top:0px', 'Survivor Confessional Data Dashboard (46 Seasons)'),
           p('View Information from Your Favorite Castaways')
  ),
  fluidRow(
    column(
      3,
      selectInput("plot", "Choose Season:", choices = 1:46),
      tags$h1("This dashboard allows users to select a season of interest and view the number of confessionals
              each contestant received during each episode as well as the total (and average) numbers of confessionals for each contestant across the season.", style = "font-size:15px;")
    ),
    column(
      9, 
      mainPanel(
        width = 12,
        plotlyOutput("myplot"),
        plotlyOutput("myplot2"),
        plotlyOutput("myplot3")
      )
    )
  )
)


server = function(input, output){
  output$myplot = renderPlotly({
    
    i = as.integer(input$plot)
    
    subplot(plots[[i]], shareX = TRUE, shareY = TRUE)
    
  })
  output$myplot2 = renderPlotly({
    
    i = as.integer(input$plot)
    
    subplot(plots_2[[i]], shareX = TRUE, shareY = TRUE, widths = 1)
    
  })
  output$myplot3 = renderPlotly({
    
    i = as.integer(input$plot)
    
    subplot(plots_3[[i]], shareX = TRUE, shareY = TRUE, widths = 1)
    
  })
}


shinyApp(ui = ui, server = server)

rm(list=ls())
cat('\014')

library(dplyr)
library(ggplot2)
library(ggvis)
library(reshape2)
library(shiny)

if (!require('GGally')) {
  install.packages('GGally')
  library('GGally')
}

# setwd("~/msan/vis/hw3/src/")


format_for_parallel <- function(df) {
  df <- group_by(df, type) %>% summarize(Reach = mean(lifetime_post_total_reach), 
                                         Impressions = mean(lifetime_post_total_impressions),
                                         Users = mean(lifetime_engaged_users),
                                         Consumers = mean(lifetime_post_consumers))  
  df$Reach <- normalize_list(df$Reach)
  df$Impressions <- normalize_list(df$Impressions)
  df$Users <- normalize_list(df$Users)
  df$Consumers <- normalize_list(df$Consumers)
  return(melt(df, 'type'))
}


load_fb <- function() {
  df <- read.csv("../Facebook_metrics/dataset_Facebook.csv", sep = ";", stringsAsFactors = FALSE)
  names(df) <- c('page_total_likes', 
                 'type',
                 'category',
                 'post_month',
                 'post_weekday',
                 'post_hour',
                 'paid',
                 'lifetime_post_total_reach',
                 'lifetime_post_total_impressions',
                 'lifetime_engaged_users',
                 'lifetime_post_consumers', 
                 'lifetime_post_consumptions', 
                 'lifetime_post_impressions_by_people_who_have_liked_your_page',
                 'lifetime_post_reach_by_people_who_like_your_page', 
                 'lifetime_people_who_have_liked_your_page_and_engaged_with_your_post',
                 'comment',
                 'like',
                 'share',
                 'total_interactions')
  df$post_weekday = as.factor(df$post_weekday)
  df$post_hour = as.factor(df$post_hour)
  df$category = as.factor(df$category)
  df$type = as.factor(df$type)
  return(df)
}


load_db <- function() {
  return(read.csv("../dataset_diabetes/diabetic_data.csv", sep = ",", stringsAsFactors = FALSE))
}


normalize <- function(x, minimum, maximum) {
  return ((x - minimum) / (maximum - minimum))
}


normalize_list <- function(l) {
  return(vapply(l, FUN = normalize, FUN.VALUE = 1, min(l), max(l)))
}


plot_heatmap <- function(df) {
  
  df %>% 
    ggvis(~post_weekday, ~post_hour) %>% 
    layer_rects(width = band(), height = band(), 
                fill = input_radiobuttons(c('like', 'share', 'comment'), 
                                          label = "Heatmap value choice", 
                                          map = as.name)) %>%
    add_legend("fill", title = "Scale") %>%
    scale_nominal("x", padding = 0, points = FALSE) %>%
    scale_nominal("y", padding = 0, points = FALSE) %>%
    add_tooltip(hover_heat, "hover") %>%
    bind_shiny("heat", "p_ui")
}


plot_parallel <- function(df) {
  format_for_parallel(df) %>% 
    ggvis(~variable, ~value, stroke = ~type, strokeWidth := 5) %>% 
    layer_lines() %>%
    add_tooltip(hover_parallel, "hover") %>%
    bind_shiny("parallel", "p_ui")
}


hover_heat <- function(x) {
  paste(x$value)
}


hover_parallel <- function(x) {
  paste(x$Reach)
}


server <- function(..., session, output) {
  fb_df <- load_fb()
  plot_heatmap(fb_df)
  # plot_scatter_matrix(fb_df)
  output$scatter_matrix <- renderPlot(
    ggpairs(fb_df[, c(8, 9, 10, 11)], colour="gray20") 
  ) 
  plot_parallel(fb_df)
}


ui <- fluidPage(
  titlePanel("Homework 3"),
  mainPanel(
    tabsetPanel(
      tabPanel("Heat Map", ggvisOutput("heat")),
      tabPanel("Scatterplot Matrix", plotOutput("scatter_matrix")),
      tabPanel("Parallel Corodinates Plot", ggvisOutput("parallel"))
    )
  ),
  uiOutput("p_ui")
)


shinyApp(ui = ui, server = server)

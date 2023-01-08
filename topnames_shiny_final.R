
# Load packages -----------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(data.table)
library(plotly)
library(shinyWidgets)
library(wordcloud2)
library(ggthemes)

# Load data ---------------------------------------------------------
df <- read.csv("BabyNames.csv")
df <- data.frame(df)


# create a shiny dashboard app--------------------------------------------------

ui <- fluidPage(
  titlePanel(h1("Popular Baby Names Over Time", align = "center")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("gender", "Select gender:", choices = c("M", "F"), inline = TRUE),
      br(),
      selectInput("year", "Select year", choices = setNames(nm = list(unique(df$Year))), selected = 2001),
      br(),
      sliderInput("top", " Number of top names", min=0, max=50, value = 15),
      br(),
      actionButton("show", label = "Show")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Word Cloud", wordcloud2Output("wordcloud")),
        tabPanel("Plot", plotlyOutput("trend")),
        tabPanel("Table", dataTableOutput("table"))
      )
    )
  )
)

server <- function(input, output, session) {
  top_names <- eventReactive(
    eventExpr = input$show,
    valueExpr = {
      df %>%
        filter(Gender == input$gender) %>%
        filter(Year == input$year) %>%
        arrange(desc(Count)) %>%
        head(n=input$top)
    }
  )
  
  #output 1
  output$wordcloud <- renderWordcloud2({
    df_wc <- top_names() %>%
      select(Name, Count)
    
    df_wc$Name <- as.factor(df_wc$Name)
    
    wordcloud2(
      data = df_wc, size = 0.65,
      color = if (input$gender == "M") {
        rep_len(c("#26495c", "#c4a35a", "#c66b3d"), nrow(df_wc))
      } else {
        rep_len(c("#c83349", "#e06377", "#eeac99"), nrow(df_wc))
      },
      backgroundColor = "white"
    )
  })
  
  # output 2
  output$trend <- renderPlotly({
    top_names() %>%
      ggplot(aes(x = reorder(Name, Count), y=Count)) +
      geom_col(fill = if (input$gender == "M") {
        fill <- "#3686C9"
      } else {
        fill <- "#E687B8"
      }) + ylim(0,90000)+
      coord_flip()+theme_bw() + labs(x = "")
  })
  
  #output 3
  output$table <- renderDataTable({

    df_prev_1 <- df %>%
      filter(Gender == input$gender) %>%
      filter(Year == as.numeric(input$year) - 1) %>%
      mutate(Rank_prev_1 = dense_rank(desc(Count))) %>%
      select(Name, Year, Count, Rank_prev_1)
    
    df_prev_2 <- df %>%
      filter(Gender == input$gender) %>%
      filter(Year == as.numeric(input$year) - 2) %>%
      mutate(Rank_prev_2 = dense_rank(desc(Count))) %>%
      select(Name, Year, Count, Rank_prev_2)

    df_foll_1 <- df %>%
      filter(Gender == input$gender) %>%
      filter(Year == as.numeric(input$year) + 1) %>%
      mutate(Rank_foll_1 = dense_rank(desc(Count))) %>%
      select(Name, Year, Count, Rank_foll_1)
    
    
    df_foll_2 <- df %>%
      filter(Gender == input$gender) %>%
      filter(Year == as.numeric(input$year) + 2) %>%
      mutate(Rank_foll_2 = dense_rank(desc(Count))) %>%
      select(Name, Year, Count, Rank_foll_2)

    
    df_final_prev <- merge(x = df_prev_1, y = df_prev_2, by = "Name", all = TRUE)
    df_final_foll <- merge(x = df_foll_1, y = df_foll_2, by = "Name", all = TRUE)
    df_final <- merge(x = df_final_prev, y = df_final_foll, by = "Name", all = TRUE)


    # df to refer to the total Count for percentage calculation
    count_total <- df %>%
      filter(Gender == input$gender) %>%
      filter(Year == input$year)

    # adding new columns with percentage & rank
    top_names() %>%
      mutate(Perc = round(Count * 100 / sum(count_total$Count), digits = 2)) %>%
      mutate(Rank = dense_rank(desc(Count))) %>%
      merge(df_final, by = "Name", all = TRUE) %>%
      mutate(Rank_Change_2prev_y=Rank_prev_2 - Rank_prev_1) %>% 
      mutate(Rank_Change_2foll_y=Rank_foll_1 - Rank_foll_2) %>% 
      select(Name, Count, Perc, Rank, Rank_Change_2prev_y, Rank_Change_2foll_y) %>%
      arrange(desc(Count)) %>%
      head(n = input$top)
  })
  
}

shinyApp(ui = ui, server = server)

#Bar chart & table of top names over chosen time + Show button

# Load packages -----------------------------------------------------
library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library (plotly)
library(shinyWidgets)

# Load data ---------------------------------------------------------
df = read.csv("C:/Users/olga.eppaturi/My_Docs/R training/BabyNames.csv")
df<- data.frame(df)

#create a shiny dashboard app--------------------------------------------------

ui<-fluidPage(
  titlePanel(h1("Popular Names Trend Over Time",align="center")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("gender", "Select gender:", choices=c("M", "F"),inline=TRUE),
      br(),
      sliderInput("year", "Select year", min=1880, max=2014, value=2000),
      br(),
      numericInput("top"," Number of top names", value =10),
      br(),
      actionButton("show", label = "Show")),
                       
    
    mainPanel(
      plotlyOutput("trend"),
      dataTableOutput("table"))
    )
)
  
server<-function(input, output, session){
  
    top_names<- eventReactive(
      eventExpr = input$show,
      valueExpr = {df %>% 
          filter(Gender==input$gender) %>%
          filter(Year==input$year) %>% 
          slice_max(Count, n=input$top)
      }
    )
                     
    output$trend<- renderPlotly({
      top_names()%>% 
        ggplot(top_names, mapping=aes(x=Name, y=Count))+
        geom_col(fill= if (input$gender=="M") {fill="#0066CC"} else {fill="#CC3366"})
    })
    
    output$table<-renderDataTable({
      Count_total<-df %>% filter(Gender==input$gender) %>% filter(Year==input$year)
      top_names()%>% 
        group_by (Name) %>% 
        summarise(Count=sum(Count)) %>%
        mutate (Perc = round(Count*100/sum (Count_total$Count), digits=2)) %>%
        slice_max(Count, n=input$top)
    })
} 

shinyApp(ui=ui, server=server)
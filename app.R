packages <- c("shiny", 
              "tidyverse", 
              "shinydashboard", 
              "plotly", 
              "DT")
missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(shiny)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(DT)

post_data.df<-read_csv("https://raw.githubusercontent.com/jsgriffin96/shiny_r_workshop/refs/heads/main/data/Post%20Data.csv")%>%
  mutate(across(c("Date", "Revenue"), ~ str_extract(.x, "\\d+")))%>%
  mutate(across(c("Date", "Revenue"), as.numeric))%>%
  mutate(Post_Type=factor(Post_Type, levels=c("Featured Product","Announcement","Event Showcase","Employee of the Week")))%>%
  arrange(Date)


ui <- dashboardPage(
  dashboardHeader(title = "Social Media Posts"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("post_type", "Post Type:", choices = unique(post_data.df$Post_Type), multiple=TRUE),
      menuItem("Monthly Conversion Metrics", tabName = "conversion", icon = icon("chart-bar")),
      menuItem("Data Download", tabName = "download", icon = icon("download"))
    )
  ),
  dashboardBody(
      tabItems(
        tabItem(tabName = "conversion",
                fluidRow(
                  box(
                    title = "Average Conversions by Post Type", status = "primary", solidHeader = TRUE, 
                    collapsible = TRUE, width = 6,
                    uiOutput("conversionBox")  
                  ),
                  box(
                    title = "Top 5 Posts with Lowest Conversion Rate", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    collapsible = TRUE, 
                    width = 6,
                    DTOutput("lowestConversionTable")
                  )
                ),
                
                fluidRow(
                  box(title = "Total Conversions per Day", 
                      status = "primary", 
                      solidHeader = TRUE, 
                      collapsible = TRUE, 
                      width = 12,
                      plotlyOutput("conversionsTimeSeries"))
                )
        ),
        
        tabItem(tabName = "download",
                fluidRow(
                  box(
                    title = "Average Post Interaction by State", status = "primary", solidHeader = TRUE, 
                    collapsible = TRUE, width = 12,
                    DTOutput("rawDataTable")  
                  )
                ),
                fluidRow(
                  div(
                    class = "text-center",
                    downloadButton("download_data", "Download Raw Data")
                  )
                )
        )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  filtered_df<-reactive({
    req(input$post_type)
    
    post_data.df %>%
      filter(Post_Type %in% input$post_type)
  })
  
  conversion_data<-reactive({
    filtered_df()%>%
      group_by(Post_Type) %>%
      summarize(avg_conversion_rate = mean(Conversions / Clicks, na.rm = TRUE)) %>%
      ungroup()
  })
  
  conversions_by_day <- reactive({
    filtered_df() %>%
      group_by(Post_Type, Date) %>%
      summarize(total_conversions = sum(Conversions, na.rm = TRUE)) %>%
      arrange(Date)
  })
  
  low_conversion_posts <- reactive({
    filtered_df() %>%
      mutate(Conversion_Rate = Conversions / Clicks) %>%
      arrange(Conversion_Rate) %>%
      slice_head(n = 5) %>%
      select(!Reactions:Shares)
  })
  

  ### outputs ###
  output$conversionBox <- renderUI({
    valueBoxes <- lapply(1:nrow(conversion_data()), function(i) {
      valueBox(
        subtitle = conversion_data()$Post_Type[i],
        value = paste0(round(conversion_data()$avg_conversion_rate[i] * 100, 2), "%"),
        color = "blue",
        width = 3
      )
    })
  })
  
  output$lowestConversionTable <- renderDT({
    datatable(low_conversion_posts(), 
              options = list(pageLength = 5, autoWidth = FALSE),
              rownames = FALSE)
  })
  
  output$conversionsTimeSeries <- renderPlotly({
    plot_ly(data = conversions_by_day(), x = ~Date, y = ~total_conversions, color = ~Post_Type, type = 'scatter', mode = 'lines')
  })
  
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("raw_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_df(), file, row.names = FALSE)
    }
  )
  
  output$rawDataTable <- renderDT({
    datatable(filtered_df(), 
              options = list(pageLength = 5, autoWidth = FALSE),
              rownames = FALSE)
  })
  
}

shinyApp(ui = ui, server = server)

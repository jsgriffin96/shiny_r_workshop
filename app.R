library(shiny)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(DT)
#setwd("~/Library/Mobile Documents/com~apple~CloudDocs/School/Grad/CCIDM/Shiny Workshop/Workshop_Test")

post_data.df<-read_csv("data/Post Data.csv")%>%
  mutate(across(c("Date", "Revenue"), ~ str_extract(.x, "\\d+")))%>%
  mutate(across(c("Date", "Revenue"), as.numeric))%>%
  mutate(`Post Type`=factor(`Post Type`, levels=c("Featured Product","Announcement","Event Showcase","Employee of the Week")))%>%
  arrange(Date)



ui <- dashboardPage(
  dashboardHeader(title = "Social Media Posts"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("post_type", "Post Type:", choices = NULL, multiple=TRUE),
      selectInput("state", "State:", choices = NULL, multiple=TRUE), 
      menuItem("Monthly Conversion Metrics", tabName = "conversion", icon = icon("chart-bar")),
      menuItem("Engagement Metrics", tabName = "funnel", icon = icon("chart-bar"))
      #menuItem("Data Download", tabName = "download", icon = icon("download"))#DT table to view and download dataa?
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
         #funnelPlot { height: calc(100vh - 150px) !important; } /* Adjusts height relative to viewport */
         #commentPlot { height: calc(100vh - 150px) !important; }
      ")) #custom css to have plot fill dynamically
    ),
    tabItems(
      tabItem(tabName = "funnel",
              fluidRow(
                box(
                  title = "Average Post Interaction by State", status = "primary", solidHeader = TRUE, 
                  collapsible = TRUE, width = 12,
                  plotlyOutput("funnelPlot")  
                )
              ),
              fluidRow(
                box(
                  title = "Comment Distribution by State", status = "primary", solidHeader = TRUE, 
                  collapsible = TRUE, width = 12,
                  plotlyOutput("commentPlot")  
                )
              )
      ),
      tabItem(tabName = "conversion",
              fluidRow(
                box(
                  title = "Average Conversions by State(Conversions/Clicks)", status = "primary", solidHeader = TRUE, 
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
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observe({
    updateSelectInput(session, "post_type", choices = unique(post_data.df$`Post Type`))
    updateSelectInput(session, "state", choices = unique(post_data.df$State))
  })
  
  filtered_df<-reactive({
    req(input$post_type, input$state)  # Ensure all inputs are set
    
    post_data.df %>%
      filter(
        `Post Type` %in% input$post_type,
        State %in% input$state
      )
  })
  
  funnel_data <- reactive({
    data <- filtered_df()
    
    data %>%
      group_by(State, `Post Type`) %>%
      summarise(
        Reactions = mean(Reactions, na.rm = TRUE),
        Comments = mean(Comments, na.rm = TRUE),
        Shares = mean(Shares, na.rm = TRUE),
        Clicks = mean(Clicks, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = Reactions:Clicks, names_to = "Stage", values_to = "Count") %>%
      mutate(Stage=factor(Stage, levels=c("Reactions","Comments","Shares","Clicks")))%>%
      ungroup()
  })
  
  conversion_data<-reactive({
    filtered_df()%>%
      group_by(State) %>%
      summarize(avg_conversion_rate = mean(Conversions / Clicks, na.rm = TRUE)) %>%
      ungroup()
  })
  
  conversions_by_day <- reactive({
    filtered_df() %>%
      group_by(State, Date) %>%
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
  
  # Plot the funnel
  output$funnelPlot <- renderPlotly({
    
    post_type_colors <- c(
      "Featured Product" = "#4E79A7", 
      "Employee of the Week" = "#F28E2B",   
      "Event Showcase" = "#59A14F",  
      "Announcement" = "#E15759"    
    )
    
    plot<-ggplot(funnel_data(), aes(x = Stage, y = Count, fill=`Post Type`)) +
      geom_col(position="stack") +
      facet_wrap(~State)+
      #coord_flip() +
      labs(
        x = "Interaction",
        y = "Count"
      ) +
      theme_minimal(base_size = 14)+
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16)
        )+
      scale_fill_manual(values = post_type_colors)
    
    ggplotly(plot, tooltip = c("Stage", "Count", "Post Type"))
  })
  
  output$commentPlot <- renderPlotly({
    
    plot<-ggplot(filtered_df(), aes(x = Clicks, fill=`Post Type`)) +
      geom_histogram(stat="bin", binwidth = 500, color = "black") +
      facet_wrap(~State)+
      theme_minimal(base_size = 14)+
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16)
      )
    
    ggplotly(plot, tooltip = c("Stage", "Count", "Post Type"))
  })
  
  output$conversionBox <- renderUI({
    valueBoxes <- lapply(1:nrow(conversion_data()), function(i) {
      valueBox(
        subtitle = conversion_data()$State[i],
        value = paste0(round(conversion_data()$avg_conversion_rate[i] * 100, 2), "%"),
        color = "blue",
        width = 3
      )
    })
    #tagList(valueBoxes)
    #do.call(fluidRow, valueBoxes)
  })
  
  output$lowestConversionTable <- renderDT({
    datatable(low_conversion_posts(), 
              options = list(pageLength = 5, autoWidth = FALSE),
              rownames = FALSE)
  })
  
  output$conversionsTimeSeries <- renderPlotly({
    plot_ly(data = conversions_by_day(), x = ~Date, y = ~total_conversions, color = ~State, type = 'scatter', mode = 'lines')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

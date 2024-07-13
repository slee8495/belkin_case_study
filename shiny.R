# Load necessary libraries
library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(readxl)
library(shinyWidgets)
library(skimr)
library(caret)
library(patchwork)

# Set the maximum upload size to 50 MB
options(shiny.maxRequestSize = 100 * 1024^2, shiny.trace = TRUE)
# UI 
ui <- navbarPage(
  theme = shinythemes::shinytheme("flatly"),
  "Belkin Case Study",
  tabPanel("Data Upload",
           sidebarLayout(
             sidebarPanel(
               fileInput("belkin_file", "Upload Belkin Case Study file (XLSX format);", accept = c(".xlsx"))
             ),
             mainPanel()
           )
  ),
  tabPanel("Data Summary",
           DTOutput("data_summary")
  ),
  tabPanel("Model Summary",
           verbatimTextOutput("model_summary")
  ),
  tabPanel("Scatter Plots",
           plotOutput("combined_scatter_plot")
  ),
  tabPanel("Line Plots",
           plotOutput("combined_line_plot")
  )
)

# Server logic
server <- function(input, output, session) {
  belkin_data <- reactiveVal()
  
  observeEvent(input$belkin_file, {
    req(input$belkin_file)
    belkin_data <- read_xlsx(input$belkin_file$datapath)
    belkin_data(belkin_data)
  })
  
  output$data_summary <- renderDT({
    req(belkin_data())
    skimmed_data <- skim(belkin_data())
    datatable(skimmed_data, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$model_summary <- renderPrint({
    req(belkin_data())
    
    model_data <- belkin_data() %>% janitor::clean_names() %>% 
      mutate(ordered_revenue_amount = as.numeric(ordered_revenue_amount),
             ordered_units = as.numeric(ordered_units),
             asp = as.numeric(asp),
             marketing_spend = as.numeric(marketing_spend),
             views = as.numeric(views),
             category = as.factor(category),
             subcategory = as.factor(subcategory)) %>%
      replace_na(list(ordered_revenue_amount = 0,
                      ordered_units = 0,
                      asp = 0,
                      marketing_spend = 0,
                      views = 0)) %>%
      mutate(across(everything(), ~ replace(.x, is.infinite(.x) | is.nan(.x), 0)))
    
    data_dummies <- dummyVars(" ~ .", data = model_data)
    data_prepared <- predict(data_dummies, newdata = model_data) %>% as.data.frame()
    
    set.seed(123)
    trainIndex <- createDataPartition(data_prepared$ordered_revenue_amount, p = 0.8, list = FALSE, times = 1)
    data_train <- data_prepared[trainIndex,]
    data_test <- data_prepared[-trainIndex,]
    
    model <- lm(ordered_revenue_amount ~ ., data = data_train)
    
    options(scipen = 999)
    summary(model)
  })
  
  output$combined_scatter_plot <- renderPlot({
    req(belkin_data())
    
    belkin_data_clean <- belkin_data() %>% janitor::clean_names() %>% 
      mutate(
        asp = as.numeric(asp),
        ordered_revenue_amount = as.numeric(ordered_revenue_amount),
        marketing_spend = as.numeric(marketing_spend),
        views = as.numeric(views)
      )
    
    p1 <- ggplot(belkin_data_clean, aes(x = marketing_spend, y = ordered_revenue_amount, color = asp, size = views)) +
      geom_point(alpha = 0.6) +
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = "Revenue vs. Marketing Spend with ASP", x = "Marketing Spend ($)", y = "Revenue ($)", color = "ASP ($)") +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8))
    
    p2 <- ggplot(belkin_data_clean, aes(x = views, y = ordered_revenue_amount, color = marketing_spend, size = asp)) +
      geom_point(alpha = 0.7) +
      scale_color_gradient(low = "blue", high = "red") +
      labs(title = "Revenue vs. Views with Marketing Spend and ASP",
           x = "Views",
           y = "Revenue ($)",
           color = "Marketing Spend ($)",
           size = "ASP ($)") +
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8))
    
    combined_plot <- p1 / p2
    combined_plot
  })
  
  output$combined_line_plot <- renderPlot({
    req(belkin_data())
    
    t1 <- belkin_data() %>% janitor::clean_names() %>% 
      mutate(week_ending = as.Date(week_ending)) %>%
      group_by(week_ending) %>%
      summarise(total_revenue = sum(ordered_revenue_amount)) %>%
      ggplot(aes(x = week_ending, y = total_revenue)) +
      geom_line(color = "blue") +
      labs(title = "Revenue Over Time", x = " ", y = "Total Revenue ($)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12))
    
    t2 <- belkin_data() %>% janitor::clean_names() %>% 
      mutate(week_ending = as.Date(week_ending)) %>%
      group_by(week_ending) %>%
      summarise(total_views = sum(views)) %>%
      ggplot(aes(x = week_ending, y = total_views)) +
      geom_line(color = "red") +
      labs(title = "Views Over Time",
           x = "Week Ending",
           y = "Total Views") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12))
    
    combined_plot_2 <- t1 / t2
    combined_plot_2
  })
}

# Run the app
shinyApp(ui = ui, server = server)

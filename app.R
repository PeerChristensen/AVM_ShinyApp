# 
# library(dplyr)
# library(ggplot2)
# library(readr)
# library(tidyr)
# library(shiny)
# 
# df  <- read_csv("avm_all_3.csv") %>% drop_na()
# #municipality <- df %>% dplyr::pull(MUNI_NAME) %>% factor()
# municipality <- factor(df$MUNI_NAME)
# 
# # Define UI for application that plots features of movies
# ui <- fluidPage(
#   
#   titlePanel("AVM"),
#   
#   # Sidebar layout with a input and output definitions
#   sidebarLayout(
#     
#     # Inputs
#     sidebarPanel(
#       
#       selectInput(inputId = "muni",
#                   label   = "Municipality:",
#                   choices = levels(municipality),
#                   selected = "København"),
#     
#     selectInput(inputId = "x",
#                 label   = "X-axis:",
#                 choices = c("PROP_PURPRICE","PROP_VALUASQM_AVM","PROP_VALUATION_AVM"),
#                 selected = "PROP_PURPRICE"),
#   
#    selectInput(inputId = "y",
#               label   = "Y-axis:",
#               choices = c("PROP_PURPRICE","PROP_VALUASQM_AVM","PROP_VALUATION_AVM"),
#               selected = "PROP_VALUATION_AVM"),
#    
#    sliderInput(inputId = "alpha",
#                label   = "Alpha:",
#                min = .01,max=1,step=.01,
#                value = .1)),
#     
#     # Outputs
#     mainPanel(
#       plotOutput(outputId = "scatterplot",height=600),
#       textOutput(outputId = "corr")
#     )
#   )
# )
# 
# # Define server function required to create the scatterplot
# server <- function(input, output) {
#   
#   # Create the scatterplot object the plotOutput function is expecting
#   
#   output$scatterplot <- renderPlot({
# 
#     df  <- read_csv("avm_all_3.csv") %>% drop_na()
#     #municipality <- df %>% pull(MUNI_NAME) %>% factor()
#     municipality <- factor(df$MUNI_NAME)
#   
#     df <- df %>%
#       select(PROP_PURPRICE,PROP_VALUASQM_AVM,PROP_VALUATION_AVM,MUNI_NAME) %>%
#       filter(MUNI_NAME == input$muni,
#              PROP_PURPRICE      < 10000000,
#              PROP_VALUATION_AVM < 10000000,
#              PROP_VALUASQM_AVM  < 75000,
#              PROP_VALUASQM_AVM  > 0,
#              PROP_PURPRICE      > 0)
#     
#     ggplot(data = df, aes_string(x = input$x,y = input$y)) +
#       geom_point(colour = "steelblue", alpha = input$alpha, size = 3) +
#       theme_minimal() +
#       theme(plot.title = element_text(size=24),
#             axis.title = element_text(size=18),
#             axis.text = element_text(size=16)) +
#       ggtitle(input$muni)
#     
#   })
#     
#     output$corr <- renderText({
#       
#       df <- df %>%
#         select(PROP_PURPRICE,PROP_VALUASQM_AVM,PROP_VALUATION_AVM,MUNI_NAME) %>%
#         filter(MUNI_NAME == input$muni,
#                PROP_PURPRICE      < 10000000,
#                PROP_VALUATION_AVM < 10000000,
#                PROP_VALUASQM_AVM  < 75000,
#                PROP_VALUASQM_AVM  > 0,
#                PROP_PURPRICE      > 0)
#       
#       r <- round(cor(df[, input$x], df[, input$y], use = "pairwise"), 3)
#         paste0("Correlation = ", r)
#       
#   })
# }
# 
# 
# # Create a Shiny app object
# shinyApp(ui = ui, server = server)

library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(ggthemes)

df  <- read_csv("avm_all_5.csv")

municipality <- factor(df$MUNI_NAME)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel("AVM"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      selectInput(inputId = "muni",
                  label   = "Municipality:",
                  choices = levels(municipality),
                  selected = "København"),
      
      selectInput(inputId = "x",
                  label   = "X-axis:",
                  choices = c("PROP_PURPRICE","PROP_VALUASQM_AVM","PROP_VALUATION_AVM"),
                  selected = "PROP_PURPRICE"),
      
      selectInput(inputId = "y",
                  label   = "Y-axis:",
                  choices = c("PROP_PURPRICE","PROP_VALUASQM_AVM","PROP_VALUATION_AVM"),
                  selected = "PROP_VALUATION_AVM"),
      
      selectInput(inputId = "colour",
                  label   = "Colour by:",
                  choices = c("Year","Type"),
                  selected = "Year"),
      
      sliderInput(inputId = "alpha",
                  label   = "Alpha:",
                  min = .01,max=1,step=.01,
                  value = .1)),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "scatterplot",height=600),
      textOutput(outputId = "corr")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  
  output$scatterplot <- renderPlot({
    
    df  <- read_csv("avm_all_5.csv")
    df$Type = sample(c("a","b","c"),nrow(df), replace =T)
    municipality <- factor(df$MUNI_NAME)
    
    df <- df %>% filter(MUNI_NAME == input$muni)
    
    if (input$colour == "Year") {
      
    ggplot(data = df, aes_string(x = input$x, y = input$y, colour = input$colour)) +
      geom_point(alpha = input$alpha, size = 3) +
      theme_minimal() +
      theme(plot.title = element_text(size=24),
            axis.title = element_text(size=18),
            axis.text = element_text(size=16)) +
      ggtitle(input$muni) +
        scale_colour_gradient2_tableau()
      }
    else {
      ggplot(data = df, aes_string(x = input$x, y = input$y, colour = input$colour)) +
        geom_point(alpha = input$alpha, size = 3) +
        theme_minimal() +
        theme(plot.title = element_text(size=24),
              axis.title = element_text(size=18),
              axis.text = element_text(size=16)) +
        ggtitle(input$muni) +
        scale_colour_tableau()
    }
  })
  
  output$corr <- renderText({
    
    df <- df %>% filter(MUNI_NAME == input$muni)
    
    r <- round(cor(df[, input$x], df[, input$y], use = "pairwise"), 3)
    paste0("Correlation = ", r)
    
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)


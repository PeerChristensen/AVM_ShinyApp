
library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(ggthemes)

df  <- read_csv("avm_all_5.csv")
df <- df %>% rename(Købspris = PROP_PURPRICE,
              Vurderingspris_kvm = PROP_VALUASQM_AVM,
              Vurderingspris = PROP_VALUATION_AVM,
              Købsår = Year,
              Kommune = MUNI_NAME) %>%
              mutate(Kommune = factor(Kommune))

df$Type = recode(df$Type,"FARM" = "Gård",
                 "MULTI" = "Multi",
                 "ONE_FAM" = "Enfamilie",
                 "OTH" = "Andet",
                 "ROW" = "Række",
                 "STORY" = "Etage")

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel("AVM"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      selectInput(inputId = "muni",
                  label   = "Kommune:",
                  choices = levels(df$Kommune),
                  selected = "København"),
      
      selectInput(inputId = "x",
                  label   = "X:",
                  choices = c("Købspris","Vurderingspris_kvm","Vurderingspris"),
                  selected = "Købspris"),
      
      selectInput(inputId = "y",
                  label   = "Y:",
                  choices = c("Købspris","Vurderingspris_kvm","Vurderingspris"),
                  selected = "Vurderingspris"),
      
      selectInput(inputId = "colour",
                  label   = "Farv efter:",
                  choices = c("Købsår","Type"),
                  selected = "Købsår"),
      
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
    df <- df %>% rename(Købspris = PROP_PURPRICE,
                        Vurderingspris_kvm = PROP_VALUASQM_AVM,
                        Vurderingspris = PROP_VALUATION_AVM,
                        Købsår = Year,
                        Kommune = MUNI_NAME)  %>%
                        mutate(Kommune = factor(Kommune))
    
    df$Type = recode(df$Type,"FARM" = "Gård",
                     "MULTI" = "Multi",
                     "ONE_FAM" = "Enfamilie",
                     "OTH" = "Andet",
                     "ROW" = "Række",
                     "STORY" = "Etage")
    
    df <- df %>% filter(Kommune == input$muni)
    
    if (input$colour == "Købsår") {
      
    ggplot(data = df, aes_string(x = input$x, y = input$y, colour = input$colour)) +
      geom_point(alpha = input$alpha, size = 3) +
      theme_minimal() +
      theme(plot.title = element_text(size=24),
            axis.title = element_text(size=18),
            axis.text = element_text(size=16)) +
      ggtitle(input$muni) +
        scale_colour_gradient2_tableau() +
        guides(colour = guide_legend(override.aes = list(alpha=1))) +
        scale_y_continuous(labels = function(n) {
          trans = n / 1000
          paste0(trans, "K")
        }) +
        scale_x_continuous(labels = function(n) {
          trans = n / 1000
          paste0(trans, "K")
        }) 
    }
    
    
    else {
      ggplot(data = df, aes_string(x = input$x, y = input$y, colour = input$colour)) +
        geom_point(alpha = input$alpha, size = 3) +
        theme_minimal() +
        theme(plot.title = element_text(size=24),
              axis.title = element_text(size=18),
              axis.text = element_text(size=16)) +
        ggtitle(input$muni) +
        scale_colour_tableau() +
        guides(colour = guide_legend(override.aes = list(alpha=1))) +
        scale_y_continuous(labels = function(n) {
          trans = n / 1000
          paste0(trans, "K")
        }) +
        scale_x_continuous(labels = function(n) {
          trans = n / 1000
          paste0(trans, "K")
        }) 
    }
  })
  
  output$corr <- renderText({
    
    df <- df %>% filter(Kommune == input$muni)
    
    r <- round(cor(df[, input$x], df[, input$y], use = "pairwise"), 3)
    paste0("Correlation = ", r)
    
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)



library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(ggthemes)

df  <- read_csv("avm_all_5.csv")
df <- df %>% rename(Handelspris = PROP_PURPRICE,
              Vurderingspris_kvm = PROP_VALUASQM_AVM,
              Vurderingspris = PROP_VALUATION_AVM,
              Handelsår = Year,
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
                  choices = c("Handelspris","Vurderingspris_kvm","Vurderingspris"),
                  selected = "Handelspris"),
      
      selectInput(inputId = "y",
                  label   = "Y:",
                  choices = c("Handelspris","Vurderingspris_kvm","Vurderingspris"),
                  selected = "Vurderingspris"),
      
      selectInput(inputId = "colour",
                  label   = "Farv efter:",
                  choices = c("Handelsår","Type"),
                  selected = "Handelsår"),
      
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
    df <- df %>% rename(Handelspris = PROP_PURPRICE,
                        Vurderingspris_kvm = PROP_VALUASQM_AVM,
                        Vurderingspris = PROP_VALUATION_AVM,
                        Handelsår = Year,
                        Kommune = MUNI_NAME)  %>%
                        mutate(Kommune = factor(Kommune))
    
    df$Type = recode(df$Type,"FARM" = "Gård",
                     "MULTI" = "Multi",
                     "ONE_FAM" = "Enfamilie",
                     "OTH" = "Andet",
                     "ROW" = "Række",
                     "STORY" = "Etage")
    
    df <- df %>% filter(Kommune == input$muni)
    
    p <- ggplot(data = df, aes_string(x = input$x, y = input$y, colour = input$colour)) +
      geom_point(alpha = input$alpha, size = 3) +
      theme_minimal() +
      theme(plot.title = element_text(size=24),
            axis.title = element_text(size=18),
            axis.text = element_text(size=16)) +
      ggtitle(input$muni) +
        guides(colour = guide_legend(override.aes = list(alpha=1)))
    
    if (input$colour == "Handelsår") {
      p <- p + scale_colour_gradient2_tableau()
    
    } else {
      p <- p + scale_colour_tableau()
    }
    
    if (input$x == "Vurderingspris_kvm") {
      p <- p + scale_x_continuous(labels = function(n) {
        trans = n / 1000
        paste0(trans, " K")
      })
      
    } else {
      p <- p + scale_x_continuous(labels = function(n) {
         trans = n / 1000000
         paste0(trans, " M")
       })
    }
    
    if (input$y == "Vurderingspris_kvm") {
      p + scale_y_continuous(labels = function(n) {
        trans = n / 1000
        paste0(trans, " K")
      })
      
    } else {
      p + scale_y_continuous(labels = function(n) {
        trans = n / 1000000
        paste0(trans, " M")
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


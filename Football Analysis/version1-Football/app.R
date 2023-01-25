#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(tidyverse) ## String and data

passing_url = "https://www.pro-football-reference.com/years/2022/passing.htm"
rushing_url = "https://www.pro-football-reference.com/years/2022/rushing.htm"



passing_df <- read_html(x = passing_url) %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE) %>% 
  .[[1]]

rushing_df <- read_html(x = rushing_url) %>%
  html_nodes("table") %>%
  .[1] %>%
  html_table(fill = TRUE) %>% 
  .[[1]]

colnames(rushing_df) = rushing_df[1,] 

mod_1_rushing_df = rushing_df %>% 
  filter(!str_detect(Rk, "Rk")) %>% 
  mutate_if(is.numeric, as.numeric)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Football"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput(inputId = "x_var",
                        label = "Select the explanatory variable",
                        choices = c("Yds","Att","Age","TD"),
                        selected = "TD"),
            selectInput(inputId = "y_var",
                        label = "Select the response variable",
                        choices = c("Yds","Att","Age","TD"),
                        selected = "Yds" )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("footballPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$footballPlot <- renderPlot({
      x_variable = input$x_var; y_variable = input$y_var
      
      mod_1_rushing_df %>% 
        ggplot(aes_string(x = x_variable,y = y_variable)) +
        geom_point() +
        theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

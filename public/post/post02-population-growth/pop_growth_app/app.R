#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(readxl)
library(here)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Demand pressures"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          sliderInput(inputId = "range", 
                      label = "Select years:",
                      sep="",
                      min = 2013, max = 2031,
                      value = c(2013,2020))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         # tableOutput("values"),
         plotOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Set the working directory
  setwd(here("content","post","post02-population-growth","pop_growth_app"))
  
  # Read in population data
  df_pop <-  read_csv('data/pop_data.csv') %>%
              # Create an ID column
              unite(id, code_mhclg, age_band,sep = " ", remove = FALSE) %>%
              select(-id,id)
  
  # Read in local authority information data and change data types
  df_info <-  read_xlsx('data/la_info.xlsx') %>%
              mutate_at(vars(5:6,8),as.factor)
  
  # Read in spending data
  df_spend <- read_xlsx('data/spending.xlsx') %>%
              filter(name != 'Isles of Scilly') %>% 
              gather(age_band,share,`0_17`:`all`) %>%
              arrange(code_mhclg) %>%
              # Create an ID column
              unite(id, code_mhclg, age_band,sep = " ", remove = FALSE) %>%
              select(-id,id)
  
  # Reactive expression to create data frame of all input values ----  
  
  popdata <- reactive({
    
    data.frame(
      df_pop %>% filter(year %in% input$range) %>%
                  group_by(code_mhclg,age_band) %>%
                  mutate(change_pct = (number - lag(number))/lag(number)) %>%
                  filter(!is.na(change_pct)) %>%
                  select(-year) %>%
                  left_join(select(df_info,code_mhclg,name,admin), by = 'code_mhclg') %>%
                  filter(age_band == 'all', name != 'City of London') %>%
                  arrange(desc(change_pct))
    )
  })
  

  # Show the values in an HTML table ----
  output$values <-  renderTable({
                      popdata()
                    })
  
  output$plot <-  renderPlot({
    ggplot(popdata(), aes(x = admin, y = change_pct, color = admin, alpha = .5)) + 
      geom_jitter(width = .05, size = 5) +
      geom_hline(yintercept = 0)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


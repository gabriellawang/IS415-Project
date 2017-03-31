library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Bus Rider Flow Pattern"),
  
  navbarPage("Bus Rider Flow",
             tabPanel("Interactive Analysis",
                      sidebarLayout(
                        
                        # Sidebar with a slider input
                        sidebarPanel(width = 2,
                                     #dateInput(inputId ="date", label = "Date:", value = as.Date(c("2/17/16"),"%m/%d/%y"), format = "m/d/yy"),
                                     fileInput(inputId="file1", "Choose File"),
                                     selectInput(inputId = "date_1", label = "Date 1", choices = NULL, multiple = FALSE, selectize = TRUE),
                                     selectInput(inputId = "hour_1", label = "Hour 1", choices = NULL, multiple = FALSE, selectize = TRUE),
                                     sliderInput(inputId = "K_1", label ="K 1", min = 5, max = 100, value = 20, step = 5),
                                     selectInput(inputId = "date_2", label = "Date 2", choices = NULL, multiple = FALSE, selectize = TRUE),
                                     selectInput(inputId = "hour_2", label = "Hour 2", choices = NULL, multiple = FALSE, selectize = TRUE),
                                     sliderInput(inputId = "K_2", label ="K 2", min = 5, max = 100, value = 20, step = 5)
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          #leafletOutput("map", height= 600),
                          plotOutput("dominance_plot1",height= 600),
                          plotOutput("dominance_plot2",height= 600),                   
                          column(width = 6, h3("Statistic - 1"), plotOutput("stat1")),
                          column(width = 6, h3("Statistic - 2"), plotOutput("stat2"))
                        )
                      )
             ), 
             
             tabPanel("Flow Matrix Data - 1",
                      dataTableOutput('flow_matrix_table_1')
             ),
             
             tabPanel("Ride Data - 1",
                      dataTableOutput('ride_table_1')
             ),
             tabPanel("Flow Matrix Data - 2",
                      dataTableOutput('flow_matrix_table_2')
             ),
             
             tabPanel("Ride Data - 2",
                      dataTableOutput('ride_table_2')
             )
             
  )
))
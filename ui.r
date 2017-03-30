library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  navbarPage("Bus Rider Flow",
     tabPanel("Interactive map",
              sidebarLayout(
                
                # Sidebar with a slider input
                sidebarPanel(width = 2,
                  #dateInput(inputId ="date", label = "Date:", value = as.Date(c("2/17/16"),"%m/%d/%y"), format = "m/d/yy"),
                  fileInput(inputId="file1", "Choose File")
                  ,numericInput(inputId="hour_1", "Hour 1", value= 8, min = 0, max = 23, step = 1,width = NULL)
                  ,numericInput(inputId="hour_2", "Hour 2", value= 18, min = 0, max = 23, step = 1,width = NULL)
                  #,fileInput(inputId="file2", "Choose File")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  #leafletOutput("map", height= 600),
                  plotOutput("stat1"),
                  plotOutput("stat2"),
                  plotOutput("dominance_plot1"),
                  plotOutput("dominance_plot2"),
                  textOutput("comstat", container = div, inline = FALSE)
                  
                )
              )
     ), 
             
     tabPanel("Matrix Data",
              dataTableOutput('matrix_table')
     ),
     
     tabPanel("Ride Data",
              dataTableOutput('ride_table')
     )

  )
))
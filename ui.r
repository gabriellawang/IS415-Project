#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Bus Rider Flow", titleWidth = 300),
  ## Sidebar content
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      style = "position: fixed;",
      width = 300,
      fileInput(inputId="file1", "Choose File"),
      selectInput(inputId = "date_1", label = "Date 1", choices = NULL, multiple = FALSE, selectize = TRUE),
      selectInput(inputId = "hour_1", label = "Hour 1", choices = NULL, multiple = FALSE, selectize = TRUE),
      sliderInput(inputId = "K_1", label ="K 1", min = 5, max = 100, value = 20, step = 5),
      selectInput(inputId = "date_2", label = "Date 2", choices = NULL, multiple = FALSE, selectize = TRUE),
      selectInput(inputId = "hour_2", label = "Hour 2", choices = NULL, multiple = FALSE, selectize = TRUE),
      sliderInput(inputId = "K_2", label ="K 2", min = 5, max = 100, value = 20, step = 5),
      menuItem("Interactive Analysis - Flows", tabName = "interactive1", 
               icon = icon("bar-chart", lib = "font-awesome")),
      menuItem("Interactive Analysis - Stats", tabName = "interactive2", 
               icon = icon("bar-chart", lib = "font-awesome")),
      menuItem("Flow Matrix Data - 1", tabName = "matrix1", icon = icon("th")),
      menuItem("Ride Data - 1", tabName = "ride1", icon = icon("th")),
      menuItem("Flow Matrix Data - 2", tabName = "matrix2", icon = icon("th")),
      menuItem("Ride Data - 2", tabName = "ride2", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "interactive1", align = "center",
              fluidRow(
                h2("Interactive Analysis"),
                align="center",
                box(
                  title = "Dominance plot - 1", solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  plotOutput("dominance_plot1",height= 600)
                ),
                
                box(
                  title = "Dominance plot - 2", solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  plotOutput("dominance_plot2",height= 600)
                )
              )              
      ),
      
      tabItem(tabName = "interactive2",
              h2("Interactive Analysis"),
              box(
                title = h3("Statistic - 1"), solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("stat1")
              ),
              box(
                title = h3("Statistic - 2"), solidHeader = TRUE,
                collapsible = TRUE,
                plotOutput("stat2")
              )              
      ),
      
      tabItem(tabName = "matrix1", 
              dataTableOutput('flow_matrix_table_1')
      ),
      tabItem(tabName = "ride1",
              dataTableOutput('ride_table_1')
      ),
      tabItem(tabName = "matrix2",
              dataTableOutput('flow_matrix_table_2')
      ),
      tabItem(tabName = "ride2",
              dataTableOutput('ride_table_2')
      )
    )
  )
)

#------------------------------Check Installation------------------------------------------
packages <- c("shiny", "plotly", "leaflet", "shinydashboard", "DT")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#------------------------------Import packages---------------------------------------------
library(shiny)
library(plotly)
library(DT)
library(leaflet)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Bus Rider Flow", titleWidth = 300),
  ## Sidebar content
  dashboardSidebar(
    width = 300,
    div(style="overflow-y: scroll"),
    sidebarMenu(
      width = 300,
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Interactive Analysis - Flows", tabName = "interactive1", 
               icon = icon("bar-chart", lib = "font-awesome")),
      menuItem("Interactive Analysis - Stats", tabName = "interactive2", 
               icon = icon("bar-chart", lib = "font-awesome")),
      menuItem("Flow Matrix Data", tabName = "matrix", icon = icon("th")),
      menuItem("Ride Data", tabName = "ride", icon = icon("th")),
      fileInput(inputId="file1", "Choose File"),
      radioButtons("type", "Aggregation Type",
                   c("Plan Area" = "P_AREA",
                     "Planing Sub Zone" = "Sub_Zone")),
      fluidRow(
        column(width=6,selectInput(inputId = "date_1", label = "Date 1", choices = NULL, multiple = TRUE, selectize = TRUE)),
        column(width=6,selectInput(inputId = "date_2", label = "Date 2", choices = NULL, multiple = TRUE, selectize = TRUE))
      
      ),
      fluidRow(
        column(6,selectInput(inputId = "hour_1", label = "Hour 1", choices = NULL, multiple = FALSE, selectize = TRUE)),
        column(6,selectInput(inputId = "hour_2", label = "Hour 2", choices = NULL, multiple = FALSE, selectize = TRUE))
      ),
      sliderInput(inputId = "K_1", label ="K 1", min = 5, max = 100, value = 20, step = 5),
      sliderInput(inputId = "K_2", label ="K 2", min = 5, max = 100, value = 20, step = 5)
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Area Daily Flow", solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotlyOutput("daily_flow")
                ),
                box(
                  title = "Control",solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 3,
                  selectInput(inputId = "date", label = "Date", choices = NULL, multiple = FALSE, selectize = TRUE),
                  selectInput(inputId = "area", label = "Area", choices = NULL, multiple = FALSE, selectize = TRUE)
                )
              ),
      fluidRow(
              box(
                title = "Bus Stop Flow", solidHeader = TRUE,
                collapsible = TRUE,
                width = 6,
                leafletOutput("area_leaflet")
              ))
      ),
      
      tabItem(tabName = "interactive1", align = "center",
              fluidRow(
                h2("Interactive Analysis"),
                align="center",
                box(
                  title = "Dominant Flow - Map 1", solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  leafletOutput("dominance_leaflet1",height= 600)
                ),
                
                box(
                  title = "Dominant Flow - Map 2", solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  leafletOutput("dominance_leaflet2",height= 600)
                )
              )              
      ),
      
      tabItem(tabName = "interactive2",
              fluidRow(
                
                h2("Interactive Analysis"),
                align="center",
                box(
                  title = h3("Statistic - 1"), solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("stat1"),
                  plotlyOutput("plotCount1", height = 600)
                ),
                box(
                  title = h3("Statistic - 2"), solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("stat2"),
                  plotlyOutput("plotCount2", height = 600)
                )
              )
              
      ),
      
      tabItem(tabName = "matrix", 
              fluidRow(
                tabBox(          
                  width=12,
                  tabPanel("Flow 1", dataTableOutput('flow_matrix_table_1')),
                  tabPanel("Flow 2", dataTableOutput('flow_matrix_table_2'))
                )
              )
      ),
      tabItem(tabName = "ride",
              fluidRow(
                tabBox(          
                  width=12,
                  tabPanel("Ride 1", 
                           div(style = 'overflow-x: scroll', DT::dataTableOutput('ride_table_1'))),
                  tabPanel("Ride 2", 
                           div(style = 'overflow-x: scroll', DT::dataTableOutput('ride_table_2'))
                  )
                )
              )
              
      )
    )
  )
)

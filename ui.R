library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)

load("external1.RData")
load("rf_results.RData")

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "COVID-19 Metrics by Country",
                                    titleWidth = 300),
                    dashboardSidebar(width = 300,
                                     sidebarMenu(
                                       menuItem("Map", tabName = "map", icon = icon("map")
                                                ),
                                       
                                       selectInput(inputId = "vars",
                                                   label = "Select the Metric (for Map)",
                                                   choices = c(colnames(map_ds1)[2:5], colnames(map_ds1)[8]),
                                                   selected =  colnames(map_ds1)[2]),
                                       selectInput(inputId = "selected_country",
                                                   label = "Select the Country (for Time Series)",
                                                   choices = c(levels(res_dataset$country)),
                                                   selected = "USA"),
                                       
                                      menuItem("Time-Series (Trees)", tabName = "ts-rf", icon = icon("chart-line")),
          
                                       menuItem("Time-Series (Linear Methods)", tabName = "ts-lin", icon = icon("chart-line")),
                                       menuItem("Time-Series", tabName = "ts-final", icon = icon("chart-line"))
                                       
                                     )),
                    
                    dashboardBody(
                      
                      tabItems(
                        
                        tabItem(tabName = "map",
                                
                      
                      fluidRow(
                                  column(8, leafletOutput("map1", height = 800)),
                                  column(4, dataTableOutput("table1"))
                      )
                      ),
                      
                    tabItem(tabName = "ts-rf",
                            fluidRow(
                              box(plotlyOutput("time_series_trees"))
                            )),
                    tabItem(tabName = "ts-lin",
                            fluidRow(
                              box(plotlyOutput("time_series_linear"))
                            )),
                    tabItem(tabName = "ts-final",
                            fluidRow(
                              box(plotlyOutput("time_series_final"))
                            ))
                      ))         
                      )
                    

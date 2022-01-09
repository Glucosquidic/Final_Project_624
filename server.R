library(shinydashboard)
library(shiny)
library(leaflet)
library(dplyr)
library(stats)
library(sf)
library(lubridate)
library(plotly)

load("external1.RData")
load("rf_results.RData")



server <- function(input, output) {

  time_series <- reactive({
    filter(res_dataset, country == input$selected_country)
    
  })
  
  reactive_ds <- reactive({
    select(map_ds1, geometry, "selection" = input$vars)
  })
  
  bw <- reactive({2 * IQR(reactive_ds()$selection) / length(reactive_ds()$selection)^(1/3)})
  

  pal1 <- reactive({colorBin("viridis",
                      domain = seq(from = min(range(reactive_ds()$selection)),
                                   to = max(range(reactive_ds()$selection)), by = bw()))})

  pop <- reactive({paste("Country: ", map_ds1$name, "<br/>",
                         "Value: ", round(reactive_ds()$selection, 3))})
              
  
  
  output$map1 <- renderLeaflet({
    
    
    leaflet(reactive_ds()) %>%
      addProviderTiles(providers$CartoDB) %>%
      addPolygons(data = reactive_ds()$geometry , fillColor = pal1()(reactive_ds()$selection),
                  color = "black", popup = pop()) %>%
     addLegend(pal = pal1(), values = reactive_ds()$selection)
  })
  
  
  output$table1 <- renderDataTable({
    reactive_ds() %>% select(-geometry) %>%
      cbind("Country" = map_ds1$name)
  })
  
  
  output$time_series_trees <- renderPlotly({
   
   p <- time_series() %>% plot_ly(x = ~ date, y = ~rf_predicted_cases, name = "Random Forest", type = "scatter", mode = "lines") %>%
     add_trace(y = ~actual_deaths, name = "Actual Deaths", mode = "lines+markers") %>%
     add_trace(y = ~decision_tree_pred, name = "Decision Tree") %>%
     add_trace(y = ~bagged_tree_pred, name = "Bagged Tree") %>%
     add_trace(y = ~boosted_tree_pred, name = "Boosted Tree") %>%
     layout(title = paste0("Actual vs. Predicted Daily Deaths (thousands) for", " ", input$selected_country),
            yaxis = list(title = "Deaths"))
   
   return(p)
   
   
     
  })
  
  output$time_series_linear <- renderPlotly({
  
  p.all <- time_series() %>% plot_ly(x = ~ date, y = ~MLR, name = "MLR (Reduced by p-values)", type = "scatter", mode = "lines") %>%
    add_trace(y = ~actual_deaths, name = "Actual Deaths", mode = "lines+markers") %>%
    add_trace(y = ~ForwardMLR, "MLR (Forward Selection)") %>%
    add_trace(y = ~BackwardMLR, "MLR (Backward Selection)") %>%
    add_trace(y = ~s1, name = "Ridge") %>%
    add_trace(y = ~s1.1, name = "LASSO") %>%
    layout(title = paste0("Actual vs. Predicted Daily Deaths (thousands) for", " ", input$selected_country),
           yaxis = list(title = "Deaths"))
  
  return(p.all)
  })
  
  
  output$time_series_final <- renderPlotly({
  
  p.final <- time_series() %>% plot_ly(x = ~date, y = ~new_deaths_thousand.15.comps, name = "PCR",
                                       type = "scatter", mode = "lines") %>%
    add_trace(y = ~actual_deaths, name = "Actual Deaths", mode = "lines+markers") %>%
    add_trace(y = ~MLR, name = "MLR (Reduced by p-values") %>%
    add_trace(y = ~s1.1, name = "LASSO")  %>%
    layout(title = paste0("Actual vs. Predicted Daily Deaths (thousands) for", " ", input$selected_country),
           yaxis = list(title = "Deaths"))
  
  return(p.final)
    
  })
  
}

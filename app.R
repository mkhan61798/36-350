#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# AUTHOR: MALIK KHAN, CARNEGIE MELLON UNIVERSITY malikk@andrew.cmu.edu


library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggmap)
library(plotly)
library(dygraphs)
library(xts)
library(metricsgraphics)
library(d3treeR)
library(devtools)
library(leaflet)
library(shinythemes)
library(jsonlite)
library(RColorBrewer)
library(htmltools)
library(dplyr)
library(treemap)
library(d3heatmap)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("sandstone"), 
  dashboardPage(
  dashboardHeader(title = "Housing in King Co."),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Page", tabName = "Home"),
      menuItem("Density Map", tabName = "Graph_1"),
      menuItem("Sq ft vs Price", tabName = "Graph_2"),
      menuItem("Condition by Views", tabName = "Graph_3"),
      menuItem("Price over Time", tabName = "Graph_4"),
      menuItem("Price vs Bedrooms", tabName = "Graph_5"),
      menuItem("Heatmap", tabName = "Graph_6"),
      menuItem("Bedrooms vs Floors", tabName = "Graph_7"),
      menuItem("Choropleth of Price", tabName = "Graph_8"))),
  dashboardBody(
    
    tags$head(tags$style(
      HTML('

            /* logo */
            .skin-blue .main-header .logo {
            background-color: #800000;
           }
            

            /* navbar (rest of the header) */
            .skin-blue .main-header .navbar {
            background-color: #800000;
            }


           /* main sidebar */
           .skin-blue .main-sidebar {
           background-color: #800000;
           }
           
           
           body, label, input, button, select { 
           font-family: "Arial";
           }')
  )),
  
    tabItems(
      
      tabItem(tabName = "Home",
              box("Summary: This dataset contains data about approximately 20 thousand house sales in King County, 
                  Washington (the Seattle area). The dataset contains important information like date sold, 
                  square footage, price, number of bedrooms, and condition. It also contains specific 
                  location data like whether the house is on the waterfront and the geographic location. 
                  Our main goals with these visualizations were to determine how location was related to price, 
                  how certain attributes like square footage and number of floors affected price, and
                  how certain attributes like number of bedrooms and floors interact with each other. 
                  The obvious 'output' variable in this dataset is price, so we focused on it, although 
                  we also found interactions between the multiple 'input' variables.",
                   width = 12,
                   column(12, align="center")),
              selectInput(inputId = "dataset",
                          label = "Choose a dataset:",
                          choices = c("Price", "Year built", "# of bedrooms")),
              
              
              numericInput(inputId = "obs",
                           label = "Number of observations to view:",
                           value = 5),
              verbatimTextOutput("summary"),
              tableOutput("view")),
      
      tabItem(tabName = "Graph_1",
              checkboxInput(inputId = "density",
                          label = "Density Estimate",
                          value = TRUE),
              checkboxInput(inputId = "ind_obs",
                            label = "Show Individual Observations",
                            value = FALSE),
              
              plotOutput(outputId = "density_map")),
      tabItem(tabName = "Graph_2",
              checkboxInput(inputId = "trend_line",
                            label = strong("Show Trend Line"),
                            value = FALSE),
              metricsgraphicsOutput("scatter_plot")),
      tabItem(tabName = "Graph_3",
              selectInput(inputId = "n_viewed",
                          label = "Number of Times House was Viewed",
                          choices = c("All", 0, 1, 2, 3, 4)),
              plotlyOutput("bar_graph")),
      tabItem(tabName = "Graph_4",
              dygraphOutput("dygraph")),
      tabItem(tabName = "Graph_5",
              selectInput(inputId = "num_bedrooms",
                          label = "Number of Bedrooms",
                          choices = c("All", 0, 1, 2, 3, 4, 5, "6 or more")),
              plotlyOutput("price_hist")),
      tabItem(tabName = "Graph_6",
              plotlyOutput("heatmap")),
      tabItem(tabName = "Graph_7",
              d3tree2Output("tree")),
      tabItem(tabName = "Graph_8",
              leafletOutput("leaflet"))
        
        )      
      )
    )  
  )  
    
    

server <- function(input, output) {
  
  house <- read.csv("kc_house_data.csv", header = T) 
  house$final_date <- as.Date(house$date, format = "%Y%m%dT000000")
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Price" = house$price,
           "Year built" = house$yr_built,
           "# of bedrooms" = house$bedrooms)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
   
  output$density_map <- renderPlot({
    
   
    map_base_2 <- get_stamenmap(c(-121.9142833 - 1,47.4344217 - .4, 
                                  -121.9142833 + 1, 47.4344217 + .4), 
                                maptype = "toner", zoom = 9)
    
    map_object_2 <- ggmap(map_base_2,
                          extent = "device",
                          ylab = "Latitude",
                          xlab = "Longitude")
    
    
    
    plot1 <- map_object_2 
                              
    
    if (input$density) {
      plot1 <- plot1 + geom_density2d(aes(x = long, y = lat), data = house, col = "red", size = 1.5)
    }
    
    if (input$ind_obs) {
      plot1 <- plot1 + geom_point(aes(x = long, y = lat, size = price), data = head(house, 200), col = "blue") +
               labs(size = "Price of House")
    }
    
    print(plot1)
    

  })
  
  
  output$scatter_plot <- renderMetricsgraphics({

    house_scatter <- subset(house, house$sqft_living < 5000)
    house_scatter <- head(house, 300)
    plot2 <- house_scatter %>%
      mjs_plot(x = sqft_living, y = price, title = "SQFT vs Price of House by Number of Bedrooms") %>% 
      mjs_point(color_accessor = bedrooms, y_rug = T) %>%
      mjs_labs(x="SQFT of House", y="Price of House") %>%
      mjs_axis_x(rug=TRUE) 
    
    
    if (input$trend_line) {
      plot2 <- plot2 %>% mjs_point(least_squares=TRUE)
      
    }
    
    print(plot2)
    
  })
  
  output$bar_graph <- renderPlotly({

    if(input$n_viewed == "All") {
      plot3 <- ggplot(data = house, aes(x = condition, fill = as.factor(view))) + 
        geom_bar(position = "dodge") + labs(
          title = "Number of Times Different Conditions of Houses Were Viewed",
          x = "Condition of Houses (Higher Number - Better Condition)",
          y = "Number of Houses",
          fill = "Views"
        )
    }
    else{
      plot3 <- ggplot(data = filter(house, view == input$n_viewed), aes(x = condition)) + 
        geom_bar(color = "black", fill = "light blue") + labs(
          title = "Number of Times Different Conditions of Houses Were Viewed",
          x = "Condition of Houses (Higher Number - Better Condition)",
          y = "Number of Houses",
          fill = "Views"
        )
    }
    
    
    
    
    plot3 <- ggplotly(plot3)
    
    print(plot3)
  })
  
  output$dygraph <- renderDygraph({
    dates <- as.Date(house$date, format = "%Y%m%dT000000")
    test <- xts(x = house$price, order.by = dates)
    dygraph(test, main = "Price Time Series", ylab = "Lot Sq Ft.") %>%
      dyRangeSelector()
  })
  
  output$price_hist = renderPlotly({
    house <- house %>% mutate(bedrooms = ifelse(bedrooms > 5, "6 or more", bedrooms))
    if (input$num_bedrooms == "All") {
      plot5 <- ggplot(house, aes(x = price, fill = bedrooms)) + geom_histogram() +
        labs(title = "Distribution of Price vs. Number of Bedrooms", x = "Price", y = "Number of Houses", fill = "Bedrooms")
    } else if (input$num_bedrooms == "6 or more") {
      plot5 <- ggplot(filter(house, bedrooms == "6 or more"), aes(x = price)) + geom_histogram() +
        labs(title = "Distribution of Price vs. Number of Bedrooms", x = "Price", y = "Number of Houses")
    } else {
      plot5 <- ggplot(filter(house, bedrooms == input$num_bedrooms), aes(x = price)) + geom_histogram() +
        labs(title = "Distribution of Price vs. Number of Bedrooms", x = "Price", y = "Number of Houses")
    }
    
    ggplotly(plot5)
  })
  
  output$heatmap <- renderPlotly({
    house <- read.csv("kc_house_data.csv", header = T) 
    new_houses <- house[,c(-1,-2,-9,-10,-15,-16,-17)]
    cor_house <- cor(new_houses)
    
    plot_ly(z = cor_house, type = "heatmap", x = 
    c("price", "bedrooms", "bathrooms","sqft_living", "sqft_lot", 
    "floors", "condition", "grade", "sqft_above", "sqft_basement", 
    "lat", "long", "sqft_living15", "sqft_lot15"), y = c("price", "bedrooms", "bathrooms","sqft_living", 
                                                          "sqft_lot", "floors", "condition", "grade", 
                                                          "sqft_above", "sqft_basement", "lat", "long", 
                                                  "sqft_living15", "sqft_lot15")) %>% layout(title = "Correlation Heatmap")
    
    
  })
  
  
  output$tree <- renderD3tree2({
    
    house_tree <- house
    house_tree$bedrooms = as.character(house_tree$bedrooms)
    house_tree$floors = as.character(house_tree$floors)
    house_tree$bedrooms <- paste(house_tree$bedrooms, "bed", sep=" ")
    house_tree$floors <- paste(house_tree$floors, "Fl.", sep=" ")
    
    house_tree_floors <- house_tree %>%
      dplyr::group_by(floors, bedrooms) %>%
      summarize(n = n())
    
    
    tree_house <- treemap(house_tree_floors, index = c("floors", "bedrooms"),
                          vSize = "n",
                          type = "index",
                          fontsize.labels = c(15,12),
                          fontcolor.labels = c("white", "yellow"),
                          fontface.labels = c(2,1),
                          bg.labels = c("transparent"),
                          align.labels = list(
                            c("center", "center"),
                            c("center", "top")
                          ),
                          title = "Treemap of Floors and Bedrooms",
                          overlap.labels = 0.2,
                          inflate.labels = F
    )
    tree_house <- d3tree2(
      tree_house
      , rootname = "# of Bedrooms for X Floors"
    )
    
    print(tree_house)
    
  })
    
  
  output$leaflet <- renderLeaflet({
    house <- read_csv("kc_house_data.csv")
    prices <- house %>% select(price)
    model_kmeans <- kmeans(prices, centers = 5) # use kmeans to group the prices normally
    # Create a new dataframe appending the cluster assignment
    house$cluster <- model_kmeans$cluster
    
    min(house[which(house[, "cluster"] == 1), "price"])
    max(house[which(house[, "cluster"] == 1), "price"])
    min(house[which(house[, "cluster"] == 2), "price"])
    max(house[which(house[, "cluster"] == 2), "price"])
    min(house[which(house[, "cluster"] == 3), "price"])
    max(house[which(house[, "cluster"] == 3), "price"])
    min(house[which(house[, "cluster"] == 4), "price"])
    max(house[which(house[, "cluster"] == 4), "price"])
    min(house[which(house[, "cluster"] == 5), "price"])
    max(house[which(house[, "cluster"] == 5), "price"])
    
    # Now assign indexes, so I can refer to them when deciding the colors 
    house[which(house[, "cluster"] == 1), "index"] = '409,900 - 673,200'
    house[which(house[, "cluster"] == 2), "index"] = '674,000 - 1,137,500'
    house[which(house[, "cluster"] == 3), "index"] = '2,147,500 - 7,700,000'
    house[which(house[, "cluster"] == 4), "index"] = '75,000 - 409,500'
    house[which(house[, "cluster"] == 5), "index"] = '1,138,990 - 2,140,000'
    
    
    #price_map <- price_index(price_cluster)
    house$index <- as.factor(house$index)
    house1 <- filter(house, cluster == 1)
    house2 <- filter(house, cluster == 2)
    house3 <- filter(house, cluster == 3)
    house4 <- filter(house, cluster == 4)
    house5 <- filter(house, cluster == 5)
    
    color_palette <- colorFactor(palette = c("blue", "red", "green", "pink", "orange"), 
                                 levels = c('409,900 - 673,200', '1,138,990 - 2,140,000', '2,147,500 - 7,700,000', '674,000 - 1,137,500', 
                                            '75,000 - 409,500'))
    
    # make map
    leaflet(options = leafletOptions(minZoom = 9, dragging = TRUE)) %>% 
      addCircleMarkers(data = house1, radius = 1, 
                       popup = ~paste0("<b>", 'USD $', price),
                       color = "blue",  group = '409,900 - 673,200') %>%
      addCircleMarkers(data = house5, radius = 1, 
                       popup = ~paste0("<b>", 'USD $', price),
                       color = "red",  group = '1,138,990 - 2,140,000') %>%
      addCircleMarkers(data = house3, radius = 1, 
                       popup = ~paste0("<b>", 'USD $', price),
                       color = "green",  group = '2,147,500 - 7,700,000') %>%
      addCircleMarkers(data = house2, radius = 1, 
                       popup = ~paste0("<b>", 'USD $', price),
                       color = "pink",  group = '674,000 - 1,137,500') %>%
      addCircleMarkers(data = house4, radius = 1, 
                       popup = ~paste0("<b>", 'USD $', price),
                       color = "orange",  group = '75,000 - 409,500') %>%
      setView(lng = -122.001008, lat = 47.474443, zoom = 9) %>% # select Kings County
      addLegend(pal = color_palette, 
                values = c('75,000 - 409,500', '409,900 - 673,200', '674,000 - 1,137,500', 
                           '1,138,990 - 2,140,000', '2,147,500 - 7,700,000'),
                opacity = 0.5, title = "Price Range", position = "bottomright") %>%
      addLayersControl(overlayGroups = c('75,000 - 409,500', '409,501 - 673,500', '673,501 - 1,137,500', '
                                    1,137,501 - 2,140,500', '2,140,501 - 7,700,000'), position = "bottomleft") 
    
  })

  
}  

# Run the application 
shinyApp(ui = ui, server = server)


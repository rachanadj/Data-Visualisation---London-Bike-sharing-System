# importing libraries

library(shiny)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(tidyr)
library(dplyr)

# importing the dataset for the month of August
setwd("C:/Users/rachd/Desktop/FIT5147/Visualisation")
month_dataset <- read.csv("./month_dataset.csv", header=TRUE)
head(month_dataset)
summary(month_dataset)

# cleaning the dataset:

# 1.1 changing the data type of "Date" column of the dataset:
month_dataset$Date <- dmy(as.character(month_dataset$Date))
summary(month_dataset)


#ui

ui <- fluidPage(
  headerPanel("London Bike Sharing System Data for the Month of August"),
  
  # slider input the select range of dates:
  sliderInput(
    inputId = "date",
    label = "Select the range of dates",
    min = min(month_dataset$Date),
    max = max(month_dataset$Date),
    value = range(month_dataset$Date)
  ),
  
  # select a suburb to view
  selectInput(
    inputId = "suburb",
    label = "Select a suburb",
    choices = c("All", unique(month_dataset$Suburb)), selected = " Liverpool Street"
  ),
  
  
  mainPanel(
    wellPanel(
      tabsetPanel(
        tabPanel(
          title = "Home Page",
          p("This project is created as a part of FIT5147 - Data Exploration & Visualisation Assignment."),
          h3("Introduction"),
          p("The bike sharing system is a popular mode of transport
          for the residents of the city of London over the recent years.
          It has proven to be an environment friendly way to move around the city.
          Not only that, it is also a flexible transportation scheme in the metropolitan area.
          Due to its benefits on the reduction of greenhouse gas emmissions, many governments have recognized this 
          as an innovative strategy with multiple benefits. It has also reduce the 
          traffic congestions in city centres. Reports have shown that 77% of Londoners prefer cycling over short distances.
           This webpage provides an overview, through multiple visualisations, of the journeys recorded through the bike 
          sharing system."),
          h3("About the Datasets"),
          p("The complete dataset is collected from www.kaggle.com.
          It was then cleaned to get the journey records for the month of August,2017. This consists of 300732 records.
          As the data takes time to load, the user is provided with an option  to 
          view the data for selected range of dates over August, 2017, thus reducing the computing time with the corresponding dates."),
          p("Regardless, as a warning, the data does take some time to load."),
          h3("Webpage Layout:"),
          p("The webpage consists of three tabs:"),
          p("1. Map Analysis: This page maps out the stations and provides bar chart with a count of 
          the corresponding to the displayed stations. These can be filtered by the suburbs"),
          p("2. Popularity Analysis: This page provides with bar graphs displaying the 
          most popular suburbs and stations over the month of August."),
          p("3. Distance Analysis: This page displays the distribution of journey distances 
          over the selected data time period and suburbs."),
          h4("References: "),
          p("www.kaggle.com: London Bike Sharing System, By Eden Au")
        ),
        tabPanel(
          title = "Map Analysis",
          h3("Station Map"),
          p("The map below shows the station locations. The radius of the markers is proportional to the number of times the station appears in the recorded
           journeys for the selected time frame.
            The map can be filtered by viewing one suburb for the selected time frame to get a more detailed idea about the stations 
            in each suburb."),
          leafletOutput("m_leaflet"),
          h3("Bar Chart"),
          p("The bar chart reflects upon the filters selected, namely the dates and suburbs. Based on that,
            the bar chart shows the count of journeys covered at the station in the selected time frame. If all suburb are selected, the bar chart shows the most common stations in the all the suburbs."),
          plotOutput("station_chart")
        ),
        tabPanel(
          title = "Popularity Analysis",
          p("This bar chart depict the top 15 busiest suburbs and stations in the month of August,2017. 
            This is a static bar chart and is a means to provide an overview of the areas that should be looked at
            in order to decide on the resource allocation, and whether or not more bikes are required in any of these locations.
            As it is a static visualisation, it also works as a substitute for the user in case the complete data for the month takes a long time to 
            render for the filtered visualisations."),
          p("As per these charts, the most popular suburb is Hyde Park with almost 39,000 recorded journeys in August. The most
             popular station is in the same suburb as above and is the Hyde Park Corner with almost 10,000 recorded journeys
             in August. After the most busiest suburb, the suburb of Marylebone has almost 10,000 journeys less than that for Hyde Park."),
          h3("Bar Chart - Suburb"),
          plotlyOutput("pop_suburb"),
          h3("Bar Chart - Station"),
          plotlyOutput("pop_station")
        ),
        tabPanel(
          title = "Distance Analysis",
          h3("Distance Distribution"),
          p("The following plot depicts the histogram analysis of the distances covered by the recorded journeys.
            The plot can be filtered to see the distance distribution of a particular and how it changes over the entire month of August."),
          p("Note: The distance is measured in kms."),
          plotlyOutput("distance_count")
        ),
        tabPanel(
          title = "Insights",
          p("Some of the questions that can be answered using this visualisation 
            is as follows:"),
          p(" "),
          p(" "),
          h4("Question:"),
          p("Which are the most busiest suburbs and stations traveled by in the month of August, 2017?"),
          p(" "),
          p("Answer: "),
          p("The suburb of Hyde Park is the most busiest suburb, followed by the suburb of Marylebone, with a difference of almost 10,000 recorded journeys among them."),
          p("The Hyde Park Corner Station, Triangle Car Park, and Albert Gate stations in Hyde Park, as well as the Belgrove Street in King's Cross are some of the most popular stations for the month of August, 2017."),
          p(" "),
          p(" "),
          h4("Question:"),
          p("What does the distribution of the distances covered by the journeys look like for the month of August, 2017?"),
          p(" "),
          p("Answer:"),
          p("For the entire month of August and all suburbs combined, the distances of the recorded journeys follow a normal distribution, with
             the peak distance of 1.18 km. However, it does not follow the same pattern within a selected suburb."),
          p(" "),
          p(" "),
          p("By: Rachana Jobanputra"),
          p("Student ID: 29555248")
        )
      )
    )
  )
)

#server
server <- function(input, output){
  # creating the filtered dataset for the selected inputs
  filtered_data <- reactive({
    data <- subset(
      month_dataset,
      Date >= input$date[1] & Date <= input$date[2]
    )
    if (input$suburb != "All"){
      data <- subset(
        data,
        Suburb == input$suburb
      )
    }
    else{
      data <- data
    }
  }) 
  
  #1. creating the leaflet map
  output$m_leaflet<- renderLeaflet({
    a <- filtered_data()
    # isolating the count of stations in the filtered dataset
    b <- a %>% group_by(Station) %>% summarise(station_count = n())  
    # final dataset to be used for the map
    plot_data <- merge(x= b, y = a[,c("Station","Latitude", "Longitude")], by = "Station")
    
    if(nrow(plot_data) == 0){
      showNotification("No such Data with given input", type="error", duration=20)
    }
   
    # creating color pallete for distinct stations
    pal <- colorFactor(c("green", "red","navy", "yellow", "purple"), domain = unique(plot_data$Station))
    # creating the leaflet map
    leaflet(data = plot_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(~Longitude, ~Latitude, col = ~pal(Station), 
                       popup = paste("Station: ", plot_data$Station, "<br>",
                                     "Count: ", plot_data$station_count),
                       stroke = FALSE, radius = ~sqrt(plot_data$station_count), fillOpacity = 0.5)
  })
  # 2. Station bar chart
  output$station_chart <- renderPlot({
    
    station_plot <- filtered_data() 
    bar_chart <- station_plot %>% 
      group_by(Station) %>% 
      summarise(station_count = n()) %>% 
      arrange(desc(station_count)) %>% head(10)
    ggplot(bar_chart, aes(reorder(Station, station_count), station_count, fill = Station)) + 
      geom_bar(stat = "identity") + theme(legend.position = "none") + 
      labs(x = "Station", y = "Count", title = "Selected Station Counts") + coord_flip()
  })
  # 3. Popularity Analysis - Suburb
  output$pop_suburb <- renderPlotly({
      su_data <- month_dataset %>% 
        group_by(Suburb) %>%
        summarise(suburb_count = n()) %>% 
        arrange(desc(suburb_count)) %>% head(15)
      ggplot(su_data, aes(reorder(Suburb, suburb_count), suburb_count, fill = Suburb)) + 
        geom_bar(stat = "identity") + theme(legend.position = "none") + 
        labs(x = "Suburb", y = "Count", title = "Top 15 suburbs over the month of August") + coord_flip()
  })
  # 4. Popularity Analysis - Station
  output$pop_station <- renderPlotly({
    st_data <- month_dataset %>%
      group_by(Station) %>%
      summarise(station_count = n()) %>%
      arrange(desc(station_count)) %>% head(15)
    ggplot(st_data, aes(reorder(Station, station_count), station_count, fill = Station)) +
      geom_bar(stat = "identity") + theme(legend.position = "none") + 
      labs(x = "Station", y = "Count", title = "Top 15 stations over the month of August") + coord_flip()
    })
  # 5. Histogram of Journey Distances
  output$distance_count <- renderPlotly({
    distance_data <- filtered_data()
    if(nrow(distance_data) == 0){showNotification("No such Data with given input", type="error", duration=20)}
    distance_plot <- distance_data[c("Distance")]
    
    ggplot(distance_plot, aes(Distance))+
      geom_histogram(color = "white", fill = "navy") +
      scale_x_continuous()
  })
}

shinyApp(ui, server)

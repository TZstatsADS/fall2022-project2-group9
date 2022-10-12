
library(shiny)
library(plotly)
library(lubridate)
library(DT)
library(shinydashboard)
library(leaflet)
library(shinyjs)

nycboroughs <- sf::read_sf("https://raw.githubusercontent.com/fedhere/PUI2015_EC/master/mam1612_EC/nyc-zip-code-tabulation-areas-polygons.geojson")
res_data <- read.csv("../data/NYCHA_Residential_Addresses.csv", header = TRUE)
water_data<-read.csv('../data/avg_water.csv', header = TRUE)
heat_data<-read.csv('../data/avg_heat.csv',header = TRUE)
Electric_Data<-read.csv('../data/avg_electric.csv',header = TRUE)

final <- merge(x=nycboroughs, y=Electric_Data, by.x=c("postalCode"), by.y=c("Zip.Code"), all.y=TRUE)

res<- read.csv("../data/union.csv")[-1] 

res<-res %>% na.omit()

res$Revenue.Month2 <- ym(res$Revenue.Month)
res$Revenue.Month3 = res$Revenue.Month2
res$Borough2 = res$Borough
res$type2 = res$type


ui<- dashboardPage(
  skin='yellow',
  dashboardHeader(title = "NYCHA Utility Data",
                  titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      
      menuItem(text = "About", tabName = "about", icon=icon("clipboard")),
      
      menuItem("Pie and Line Chart",tabName="piechart",icon=icon("chart-pie")),
      
      menuItem("Utilities Map",tabName="HEATMAP",icon=icon("globe")),  
      
      menuItem("Data Table",tabName="table",icon=icon("table"))
               
      )),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "about", 
              p("Introduction", style = "font-size:100px"),
              p("We created an R Shiny app to help the NYCHA visualize the utility consumption across its residencies. The app consistents of a heatmap, pie charts, a line chart and a data table to allow the user to track utility usage over time.", style = "font-size:36px"), 
              p("The data we use is from NYCHA (New York City Housing Authority). The Consumption and cost data of Electric, Water, Heat and Cooking Gas are being plotted in this dashboard. Our data range from January 2010 to January 2022.", style = "font-size:36px"),
              
      ),
      
      tabItem(tabName = "table", 
              mainPanel( DT::dataTableOutput('table'))),
      
      tabItem(tabName = "piechart",
              sidebarLayout(
                position = "left",
                sidebarPanel(
                  width = 4,
                  selectInput("Borough","Borough, for 'Borough Difference'",choices = c("BRONX","BROOKLYN","MANHATTAN","QUEENS","STATEN ISLAND")),
                  
                  selectInput("type","Utility Type, for 'Utility Difference'",choices = c("water", "electric", "cooking_gas")),
                  
                  selectInput("Value","Display Cost or Consumption",choices = c("Cost", "Consumption")),
                  
                  dateRangeInput("dateRange", "Select Date Range", format = "yyyy-mm", min=min(res$Revenue.Month2),max=max(res$Revenue.Month2))
                ),
                
                mainPanel(
                  fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"), 
                                hight = c(10000, 10000),
                                plotlyOutput("graph1"), 
                                plotlyOutput("graph2"))),
                  fluidRow(width = 9,plotlyOutput("line1"))
                )
              )
      ),
      
      tabItem(tabName ="HEATMAP" ,
              # Use a fluid Bootstrap layout
              fluidPage(
                # Give the page a title
                titlePanel("NYC Utilities Consumption"),
                mainPanel(tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"), leafletOutput("map"), selectInput("select", label = h3("Select Utility"), 
                                                            choices = list("Electric" = 1, "Water" = 2, "Heat" = 3),  selected = 1),
                          hr(),
                          fluidRow(column(3, verbatimTextOutput("value"))))      
                
              ))
      
    ))
)

server <- function(input, output, session){
  
  output$line1 = renderPlotly({
    
    if (input$Value == 'Cost'){
      return(
        res %>%
          filter(Borough == input$Borough, 
                 Revenue.Month2 >= as.Date(input$dateRange[1]), 
                 Revenue.Month2 <= as.Date(input$dateRange[2]),
                 type == input$type) %>%
          group_by(Revenue.Month2) %>%
          summarise(DailyCost = sum(DailyCharge)) %>%
          plot_ly(x = ~Revenue.Month2, y = ~DailyCost, type= 'scatter', mode = "line") %>%
          layout(title = 'Line Plot' ,xaxis=list(title='Revenue Month'))
      )
    } else {
      return(
        res %>%
          filter(Borough == input$Borough, 
                 Revenue.Month2 >= as.Date(input$dateRange[1]), 
                 Revenue.Month2 <= as.Date(input$dateRange[2]),
                 type == input$type) %>%
          group_by(Revenue.Month2) %>%
          summarise(DailyConsumption = sum(DailyConsumption)) %>%
          plot_ly(x = ~Revenue.Month2, y = ~DailyConsumption, type= 'scatter', mode = "line") %>%
          layout(title = 'Line Plot' ,xaxis=list(title='Revenue Month') )
      )
    }
  })
  
  output$graph1 = renderPlotly({
    
  return(
    res %>%
      filter(Borough == input$Borough,
             Revenue.Month2 >= as.Date(input$dateRange[1]), 
             Revenue.Month2 <= as.Date(input$dateRange[2])
      ) %>%
      group_by(type) %>%
      summarise(DailyCost = sum(DailyCharge))  %>%
      plot_ly(labels = ~type, values = ~DailyCost, type = "pie")%>%
      layout(title = 'Borough Difference' 
             ,legend = list(orientation = "h" , font = list(size = 7)))
  )

  })
  
  output$graph2 = renderPlotly({
    if (input$Value == 'Cost'){
      return(
        res %>% 
          filter(type == input$type ,
                 Revenue.Month2 >= as.Date(input$dateRange[1]), 
                 Revenue.Month2 <= as.Date(input$dateRange[2])
          ) %>%
          group_by(Borough) %>%
          summarise(DailyCharge = sum(DailyCharge))  %>%
          filter(Borough %in% c("MANHATTAN", "QUEENS", "BROOKLYN", "BRONX", "STATEN ISLAND")) %>%
          plot_ly(labels = ~Borough, values = ~DailyCharge, type = "pie") %>%
          layout(title = 'Type Difference' 
                 ,legend = list(orientation = "h" , font = list(size = 7)))
      )
    } else {
      return(
        res %>% 
          filter(type == input$type ,
                 Revenue.Month2 >= as.Date(input$dateRange[1]), 
                 Revenue.Month2 <= as.Date(input$dateRange[2])
          ) %>%
          group_by(Borough) %>%
          summarise(DailyConsumption = sum(DailyConsumption))  %>%
          filter(Borough %in% c("MANHATTAN", "QUEENS", "BROOKLYN", "BRONX", "STATEN ISLAND")) %>%
          plot_ly(labels = ~Borough, values = ~DailyConsumption, type = "pie") %>%
          layout(title = 'Type Difference' 
                 ,legend = list(orientation = "h" , font = list(size = 7)))
      )
    }
  })
  
  output$map <-renderLeaflet({
    
    if(input$select == 1)# for Electric Data
    {
      bins <- c(0, 1000,20000,30000,4000,50000, Inf)
      pal <- colorBin("Reds", domain = Electric_Data$Electric.Consumption.Average..KWH., bins = bins)
      
      labels1 <- paste( nycboroughs$postalCode, " - ",nycboroughs$borough,  "<br/>","Consumption in KWH: ",Electric_Data$Electric.Consumption.Average..KWH.,"<br/>",
                        "Charges in $: ",Electric_Data$Electric.Average.Charge,"<br/>")%>% lapply(htmltools::HTML)
      leaflet(nycboroughs) %>%
        addTiles() %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.25,
                    fillColor = ~pal(Electric_Data$Electric.Consumption.Average..KWH.),label=labels1) %>%
        addLegend(pal = pal, values = Electric_Data$Electric.Consumption.Average..KWH., opacity = 1.0,position = "bottomright",
        )
    }
    else if(input$select == 2)#For Water Data
    {
      bins <- c(0, 200,400,600,800,1000, Inf)
      pal <- colorBin("Reds", domain = water_data$Water.Consumption.Average..HCF., bins = bins)
      
      labels1 <- paste( nycboroughs$postalCode, " - ", nycboroughs$borough, "<br/>","Consumption in HCF: ",water_data$Water.Consumption.Average..HCF.,"<br/>",
                        "Charges in $: ",Electric_Data$Electric.Average.Charge,"<br/>")%>% lapply(htmltools::HTML)
      
      leaflet(nycboroughs) %>%
        addTiles() %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.25,
                    fillColor = ~pal(water_data$Water.Consumption.Average..HCF.),label=labels1
        ) %>%
        addLegend(pal = pal, values = water_data$Water.Consumption.Average..HCF., opacity = 1.0,position = "bottomright")
    }
    
    else if(input$select == 3)#For Heat Data
    {
      bins <- c(0, 1000,20000,30000,4000,50000, Inf)
      pal <- colorBin("Reds", domain = heat_data$Heat.Consumption.Average , bins = bins)
      
      labels1 <- paste( nycboroughs$postalCode, " - ",nycboroughs$borough, "<br/>","Consumption in Therms: ",heat_data$Heat.Consumption.Average..Therms,"<br/>",
                        "Charges in $: ",heat_data$Heat.Average.Charge,"<br/>")%>% lapply(htmltools::HTML)
      
      leaflet(nycboroughs) %>%
        addTiles() %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.25,
                    fillColor = ~pal(heat_data$Heat.Consumption.Average),label=labels1
                    
        ) %>%
        addLegend(pal = pal, values = heat_data$Heat.Consumption.Average, opacity = 1.0,position = "bottomright",
                  
        )
    }}) 
  
  output$table <- renderDataTable(DT::datatable({
    data <- res[-2][-7] %>%
      select("Service.Start.Date",  "Service.End.Date", "Borough", "DailyCharge", "DailyConsumption")
    data}))
}

shinyApp(ui, server)

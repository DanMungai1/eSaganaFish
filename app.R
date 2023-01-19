library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(robotoolbox)
library(gitlabr)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = 'Sagana Fisheries'),
    dashboardSidebar(),
    dashboardBody(box(DTOutput("DataSets"), width = 10),
                  box(selectInput("dataset", "Datasets", 
                                  choices = c("Visitors","Fingerlings", "Revenue", "Food_Fish",
                                              "Orders","Official_Coms", "Tilapia_Pairing")),
                      width = 2)
    )
)


server <- function(input, output) {
    kobo_setup(url = "https://kf.kobotoolbox.org/", 
               token = "17d3d9e1633c38dde15ca96cf043b59a14d36bcf")
    data <- kobo_asset_list()$uid[1] |> kobo_asset() |> kobo_data()
    Dataset <- reactive({
        switch(input$dataset,
               Fingerlings = data |> filter(Section == "Fingerlings_Sales") |> 
                   select(Fingerling_Sale_Date:Fingerlings_Total_Revenue),
               
               Revenue = data |> filter(Section == "General_Sales") |> 
                   select(Sales_Date:Receipt_number),
               
               Food_Fish = data |> filter(Section == "Food_Fish_Harvest") |> 
                   select(Harvest_Date:Total_Weight_Harvested),
               
               Visitors = data |> filter(Section == "Visitors") |> 
                   select(Date_of_Visit:Visit_Purpose),
               
               Orders = data |> filter(Section == "Pre-Orders") |> 
                   select(Order_Date:Order_Delivery_Date),
               
               Official_Coms = data |> filter(Section == "Official_Calls_and_Emails") |> 
                   select(Call_Date:Agreed_Delivery_Date),
               
               Tilapia_Pairing = data |> filter(Section == "Pond_Pairing") |> 
                   select(Pairing_Date:Stocking_Pairing_Ratio)
        )
    })
    
    output$DataSets <- renderDT({Dataset()})
    
}

# Run the application 
shinyApp(ui = ui, server = server)



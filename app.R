library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(robotoolbox)
library(ggchicklet)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = 'Sagana Fisheries'),
    dashboardSidebar(
        sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Datasets", tabName = "datasets", icon = icon("dashboard")),
        menuItem("visitors", icon = icon("calendar"), tabName = "Visitors",
                 badgeColor = "green"),
        menuItem("Fingerlings Data", tabName = "fingerlings", icon = icon("file")),
        menuItem("Revenue Collection", tabName = "sales", icon = icon("dollar")),
        menuItem("Food Fish", tabName = "foodfish", icon = icon("fish")))),
    
    dashboardBody(
        tabItems(
            tabItem("datasets",
                    box(DTOutput("DataSets"), width = 10),
                    box(selectInput("dataset", "Datasets", 
                                    choices = c("Visitors","Fingerlings", "Revenue", "Food_Fish",
                                                "Orders","Official_Coms", "Tilapia_Pairing")),
                        width = 2)),
            tabItem("Visitors",
                    fluidPage(
                        fluidRow(
                            column(width = 12,
                                   box(plotOutput("visits"), width = 12))
                        ),
                        fluidRow(
                            column(width = 12,
                                   box(plotOutput("counties"), width = 12))
                        )
                    )
            ),
            tabItem("fingerlings",
                    fluidPage(
                        fluidRow(
                            column(width = 12,
                                   box(plotOutput("fingers"), width = 12))
                        )
                    )
            ),
            tabItem("sales",
                    fluidPage(
                        fluidRow(
                            column(width = 12,
                                   box(plotOutput("revenue"), width = 12))
                        )
                    )
            ),
            tabItem("foodfish",
                    fluidPage(
                        fluidRow()
                    )
        ))
    )
)


server <- function(input, output) {
    kobo_setup(url = "https://kf.kobotoolbox.org/", 
               token = "17d3d9e1633c38dde15ca96cf043b59a14d36bcf")
    data <- kobo_asset_list()$uid[1] |> kobo_asset() |> kobo_data()
    Dataset <- reactive({
        switch(input$dataset,
               Fingerlings = data |> filter(Section == "Fingerlings_Sales") |> 
                   select(Fingerling_Sale_Date, Farmer_Name, Farmer_Contact,
                          Farmer_County,Fingerlings_Sold, Numbers_Sold),
               
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
    output$visits <- renderPlot({
        data |> filter(Section == "Visitors") |> 
            select(Date_of_Visit:Visit_Purpose) |> 
            count(Date_of_Visit, name = "Visitors") |> 
            ggplot(aes(Date_of_Visit, Visitors)) + geom_chicklet()
            
    })
    output$counties <- renderPlot({
        data |> filter(Section == "Visitors") |> 
            select(Date_of_Visit:Visit_Purpose) |> 
            count(Date_of_Visit, Visitor_s_County,name = "Visitors") |> 
            ggplot(aes(Date_of_Visit, Visitors, fill = Visitor_s_County)) + 
            geom_chicklet() +
            theme(legend.position = c(0.9, 0.7))
    })
    output$fingers <- renderPlot({
        data |> filter(Section == "Fingerlings_Sales") |> 
        select(Fingerling_Sale_Date, Farmer_Name, Farmer_Contact,
               Farmer_County,Fingerlings_Sold, Numbers_Sold) |> 
        group_by(Fingerling_Sale_Date, Fingerlings_Sold) |> 
        summarise(Numbers_Sold = sum(Numbers_Sold), .groups = "drop") |> 
        ggplot(aes(Fingerling_Sale_Date, Numbers_Sold, fill = Fingerlings_Sold)) +
        geom_chicklet() +
        theme(legend.position = c(0.8, 0.7)) +
        labs(x = "Date of Sale", y = "Fingerlings Sold")
            
    })
    output$revenue <- renderPlot({
        data |> filter(Section == "General_Sales") |> 
            select(Sales_Date:Receipt_number) |> 
            group_by(Sales_Date, Product_Sold) |> 
            summarise(sales_Total_Revenue = sum(Sales_Total_Revenue), .groups = "drop") |> 
            ggplot(aes(Sales_Date, sales_Total_Revenue, fill = Product_Sold)) +
            geom_chicklet() +
            theme(legend.position = c(0.8,0.7)) +
            labs(x = "Date of Sale", y = "Revenue Generated", fill = "Product Sold")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)



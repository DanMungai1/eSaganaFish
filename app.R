library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(robotoolbox)
library(ggchicklet)
library(gt)
library(lubridate)
library(showtext)
library(writexl)
library(glue)
library(sf)
library(sp)
library(rKenyaCensus)
library(rmapshaper)
library(ggtext)
font_add_google(family = "Noto serif", name = "Noto Serif")
showtext_auto()
showtext_opts(dpi = 130)
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
                        width = 2),
                    box(downloadButton("download", "Download.xlsx"), width = 4)),
            tabItem("Visitors",
                    fluidPage(
                        fluidRow(
                            column(width = 6,
                                   box(plotOutput("counties"), width = 12)),
                            column(width = 6,
                                   box(plotOutput("map"), width = 12))
                        ),
                        fluidRow(
                            column(width = 12,
                                   box(gt_output("Visits"), width = 6),
                                   box(gt_output("monthly_visits"), width = 6))
                        )
                    )
            ),
            tabItem("fingerlings",
                    fluidPage(
                        fluidRow(
                            column(width = 12,
                                   box(plotOutput("fingers"), width = 12))),
                        fluidRow(
                            column(width = 6,
                                   box(gt_output("weekly"), width = 12)),
                            column(width = 6,
                                   box(gt_output("monthly"), width = 12))
                        )
                    )
            ),
            tabItem("sales",
                    fluidPage(
                        fluidRow(
                            column(width = 12,
                                   box(plotOutput("revenue"), width = 12))
                        ),
                        fluidRow(
                            column(width = 6,
                                   box(gt_output("weeklysales"), width = 12)),
                            column(width = 6,
                                   box(gt_output("monthlysales"), width = 12))
                        )
                    )
            ),
            tabItem("foodfish",
                    fluidPage(
                        fluidRow(
                            column(width = 12,
                                   box(plotOutput("food"), width = 12))
                        ),
                        fluidRow(
                            column(width = 12,
                                   box(gt_output("WeeklyFoodFish"), width = 6),
                                   box(gt_output("monthlyfoodfish"), width = 6))
                        )
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
                   select(Date_of_Visit, Visitor_s_name:Visitor_Time_Out),
               
               Orders = data |> filter(Section == "Pre-Orders") |> 
                   select(Order_Date:Order_Delivery_Date),
               
               Official_Coms = data |> filter(Section == "Official_Calls_and_Emails") |> 
                   select(Call_Date:Agreed_Delivery_Date),
               
               Tilapia_Pairing = data |> filter(Section == "Pond_Pairing") |> 
                   select(Pairing_Date:Stocking_Pairing_Ratio)
        )
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste0(input$dataset, ".xlsx")
        },
        content = function(file) {
            write_xlsx(Dataset(), file)
        }
    )
    output$DataSets <- renderDT({Dataset()})
    SHP <- KenyaCounties_SHP |> 
      st_as_sf()
    output$counties <- renderPlot({
        data |> filter(Section == "Visitors") |> 
            select(Date_of_Visit:Visit_Purpose) |> 
            mutate(Week = week(Date_of_Visit)) |> 
            count(Week, Visitor_s_County,name = "Visitors") |>
            ggplot(aes(as_factor(Week), Visitors, fill = Visitor_s_County)) + 
            geom_chicklet() +
            theme(legend.position = c(0.9, 0.85), text = element_text(family = "Noto serif")) +
            labs(x = "Week of Visit", fill = "County of Visitor")
    })
    output$map <- renderPlot({
      SHP |> left_join(data |> filter(Section == "Visitors") |> 
                         select(County = Visitor_s_County) |> 
                         count(County, sort = T, name = "Visitors") |> 
                         mutate(County = str_to_upper(County)))|> 
        ggplot(aes(fill = Visitors)) +
        geom_sf(show.legend = F) +
        geom_sf_label(aes(label = Visitors), col = "yellow", size = 3,
                      show.legend = F) +
        theme_void()
    })
    output$fingers <- renderPlot({
        data |> filter(Section == "Fingerlings_Sales") |> 
        select(Fingerling_Sale_Date, Farmer_Name, Farmer_Contact,
               Farmer_County,Fingerlings_Sold, Numbers_Sold) |> 
        group_by(Fingerling_Sale_Date, Fingerlings_Sold) |> 
        summarise(Numbers_Sold = sum(Numbers_Sold), .groups = "drop") |> 
        ggplot(aes(Fingerling_Sale_Date, Numbers_Sold, fill = Fingerlings_Sold)) +
        geom_chicklet() +
        theme(legend.position = c(0.8, 0.7),
              text = element_text(family = "Noto serif")) +
        labs(x = "Date of Sale", y = "Fingerlings Sold")
            
    })
    output$revenue <- renderPlot({
        data |> filter(Section == "General_Sales") |> 
            select(Sales_Date:Receipt_number) |> 
            group_by(Sales_Date, Product_Sold) |> 
            summarise(sales_Total_Revenue = sum(Sales_Total_Revenue), .groups = "drop") |> 
            ggplot(aes(Sales_Date, sales_Total_Revenue, fill = Product_Sold)) +
            geom_chicklet() +
            theme(legend.position = c(0.8,0.7),
                  text = element_text(family = "Noto serif")) +
            labs(x = "Date of Sale", y = "Revenue Generated", fill = "Product Sold")
    })
    output$food <- renderPlot({
        data |> filter(Section == "Food_Fish_Harvest") |> 
            select(Harvest_Date:Total_Weight_Harvested) |> 
            group_by(Harvest_Date,Species_Harvested) |> 
            summarise(Harvested_Weight = sum(Total_Weight_Harvested), .groups = "drop") |> 
            ggplot(aes(Harvest_Date, Harvested_Weight, fill = Species_Harvested)) +
            geom_chicklet() +
            theme(legend.position = c(0.5, 0.8),
                  text = element_text(family = "Noto serif")) +
            labs(x = "Harvest Date", y = "Harvested Weight")
    })
    output$weekly <- render_gt({
        data |> filter(Section == "Fingerlings_Sales") |> 
            select(Fingerling_Sale_Date, Farmer_Name, Farmer_Contact,
                   Farmer_County,Fingerlings_Sold, Numbers_Sold) |> 
            mutate(Month = month(Fingerling_Sale_Date, label = T),
                   Week = week(Fingerling_Sale_Date),
                   Day = wday(Fingerling_Sale_Date, label = T),
                   Week = as.factor(Week)) |> 
            select(Fingerlings_Sold, Numbers_Sold, Week) |> 
            group_by(Fingerlings_Sold, Week) |> 
            summarise(Numbers_Sold = sum(Numbers_Sold), .groups = "drop") |> 
            pivot_wider(names_from = Week, values_from = Numbers_Sold) |> 
            rename_at(vars(-1),~glue("Week {.}")) |> rename(`Species Sold` = `Fingerlings_Sold`) |> 
            rowwise(`Species Sold`) |> 
            mutate(Total = sum(c_across(starts_with("Week")), na.rm = T))  |> 
            arrange(desc(Total)) |> ungroup() |>
            relocate("Week 6", .after = "Week 5") |> 
            gt(rowname_col = "Species Sold") |>
            sub_missing(missing_text = "-") |> 
            summary_rows(fns = list("Total" = ~sum(., na.rm = T)),
                         formatter = fmt_number, decimals = 0,
                         missing_text = "-") |> 
            tab_options(
                data_row.padding = px(2),
                summary_row.padding = px(3), # A bit more padding for summaries
                row_group.padding = px(4)    # And even more for our groups
            ) |> 
            opt_stylize(style = 6, color = 'gray')
    })
   output$monthly <- render_gt({
       data |> filter(Section == "Fingerlings_Sales") |> 
           select(Fingerling_Sale_Date, Farmer_Name, Farmer_Contact,
                  Farmer_County,Fingerlings_Sold, Numbers_Sold) |> 
           mutate(Month = month(Fingerling_Sale_Date, label = T)) |>
           select(Fingerlings_Sold, Numbers_Sold, Month) |> 
           group_by(Fingerlings_Sold, Month) |> 
           summarise(Numbers_Sold = sum(Numbers_Sold)) |> 
           mutate(Total = sum(Numbers_Sold)) |> ungroup() |> 
           pivot_wider(names_from = Month, values_from = Numbers_Sold) |>
           arrange(desc(Total)) |> relocate(Total, .after = Feb) |> 
           gt(rowname_col = "Fingerlings_Sold") |>
           sub_missing(missing_text = "-") |> 
           summary_rows(fns = list("Total" = ~sum(., na.rm = T)),
                        formatter = fmt_number, decimals = 0) |> 
           tab_options(
               data_row.padding = px(2),
               summary_row.padding = px(3), # A bit more padding for summaries
               row_group.padding = px(4)    # And even more for our groups
           ) |> 
           opt_stylize(style = 6, color = 'gray')
   }) 
   output$weeklysales <- render_gt({
       data |> filter(Section == "General_Sales") |> 
           select(Sales_Date:Receipt_number) |> 
           mutate(Week = week(Sales_Date)) |> 
           group_by(Week, Product_Sold) |> 
           summarise(Revenue = sum(Sales_Total_Revenue)) |>
           pivot_wider(names_from = Week, values_from = Revenue) |> 
           rename_at(vars(-1),~glue("Week {.}")) |> rename(`Products Sold` = `Product_Sold`) |> 
           rowwise(`Products Sold`) |> 
           mutate(Total = sum(c_across(starts_with("Week")),na.rm =T))  |> 
           arrange(desc(Total)) |> ungroup() |> 
           gt(rowname_col = "Products Sold") |> 
           sub_missing(missing_text = "-") |> 
           summary_rows(fns = list("Total" = ~sum(., na.rm = T)),
                        formatter = fmt_number, decimals = 0) |> 
           tab_options(
               data_row.padding = px(2),
               summary_row.padding = px(3), # A bit more padding for summaries
               row_group.padding = px(4)    # And even more for our groups
           ) |> 
           opt_stylize(style = 6, color = 'gray')
           
   })
   output$monthlysales <- render_gt({
       data |> filter(Section == "General_Sales") |> 
           select(Sales_Date:Receipt_number) |> 
           mutate(Month = month(Sales_Date, label =T)) |> 
           group_by(Month, Product_Sold) |> 
           summarise(Sales_Total_Revenue = sum(Sales_Total_Revenue)) |> 
           group_by(Product_Sold) |> 
           mutate(Total = sum(Sales_Total_Revenue)) |> ungroup() |>  
           pivot_wider(names_from = Month,
                       values_from = Sales_Total_Revenue)|> 
           arrange(desc(Total)) |> relocate(Total, .after = last_col()) |> 
           gt(rowname_col = "Product_Sold") |> 
           sub_missing(missing_text = "-") |> 
           summary_rows(fns = list("Total" = ~sum(., na.rm = T)),
                        formatter = fmt_number, decimals = 0) |> 
           tab_options(
               data_row.padding = px(2),
               summary_row.padding = px(3), # A bit more padding for summaries
               row_group.padding = px(4)    # And even more for our groups
           ) |> 
           opt_stylize(style = 6, color = 'gray')
   })
   output$WeeklyFoodFish <- render_gt({
       data |> filter(Section == "Food_Fish_Harvest") |> 
           select(Harvest_Date:Total_Weight_Harvested) |> 
           mutate(Week = week(Harvest_Date)) |> 
           group_by(Species_Harvested, Week) |> 
           summarise(Total_Weight_Harvested = sum(Total_Weight_Harvested), .groups = "drop") |> 
           pivot_wider(names_from = Week, values_from = Total_Weight_Harvested) |> 
           rename_at(vars(-1),~glue("Week {.}")) |> 
           rename(`Species Harvested` = `Species_Harvested`) |>
           rowwise(`Species Harvested`) |> 
           mutate(Total = sum(c_across(starts_with("Week")), na.rm = T))  |> 
           arrange(desc(Total)) |> ungroup() |> 
           gt(rowname_col = "Species Harvested") |> 
           sub_missing(missing_text = "-") |> 
           summary_rows(fns = list("Total" = ~sum(., na.rm = T)),
                        formatter = fmt_number, decimals = 1) |> 
           tab_options(
               data_row.padding = px(2),
               summary_row.padding = px(3), # A bit more padding for summaries
               row_group.padding = px(4)    # And even more for our groups
           ) |> 
           opt_stylize(style = 6, color = 'gray')
   })
   output$monthlyfoodfish <- render_gt({
       data |> filter(Section == "Food_Fish_Harvest") |> 
           select(Harvest_Date:Total_Weight_Harvested) |> 
           mutate(Month = month(Harvest_Date, label = TRUE)) |> 
           group_by(Species_Harvested, Month) |> 
           summarise(Total_Weight_Harvested = sum(Total_Weight_Harvested), .groups = "drop") |> 
           pivot_wider(names_from = Month, values_from = Total_Weight_Harvested) |> 
           rename(`Species Harvested`=Species_Harvested) |> 
           rowwise(`Species Harvested`) |> 
           mutate(Total = sum(c_across(starts_with("Jan"))))  |> 
           arrange(desc(Total)) |> ungroup() |> 
           gt(rowname_col = "Species Harvested") |> 
           sub_missing(missing_text = "-") |> 
           summary_rows(fns = list("Total" = ~sum(., na.rm = T)),
                        formatter = fmt_number, decimals = 1) |> 
           tab_options(
               data_row.padding = px(2),
               summary_row.padding = px(3), # A bit more padding for summaries
               row_group.padding = px(4)    # And even more for our groups
           ) |> 
           opt_stylize(style = 6, color = 'gray')
   })
   output$Visits <- render_gt({
       data |> filter(Section == "Visitors") |> 
           select(Date_of_Visit:Visit_Purpose) |> 
           mutate(Week = week(Date_of_Visit)) |> 
           count(Week, Visitor_s_County, sort = T) |> 
           pivot_wider(names_from = Week, values_from = n) |> 
           relocate(`1`, .after = 1) |> 
           rename_with(~glue("Week {.}")) |> 
           rename(`Visitor's County` = `Week Visitor_s_County`) |> 
           rowwise(`Visitor's County`) |> 
           mutate(Total = sum(c_across(starts_with("Week")), na.rm = T))  |> 
           arrange(desc(Total)) |> ungroup() |> 
           gt(rowname_col = "Visitor's County") |> 
           sub_missing(missing_text = "-") |> 
           summary_rows(fns = list("Total" = ~sum(., na.rm = T)),
                        formatter = fmt_number, decimals = 0) |> 
           tab_options(
               data_row.padding = px(2),
               summary_row.padding = px(3), # A bit more padding for summaries
               row_group.padding = px(4)    # And even more for our groups
           ) |> 
           opt_stylize(style = 6, color = 'gray')
   })
   output$monthly_visits <- render_gt({
       data |> filter(Section == "Visitors") |> 
           select(Date_of_Visit:Visit_Purpose) |> 
           mutate(Month = month(Date_of_Visit, label =TRUE)) |> 
           count(Month, Visitor_s_County, sort = T) |> 
           pivot_wider(names_from = Month, values_from = n) |> 
           rename(`Visitor's County`=Visitor_s_County) |> 
           rowwise(`Visitor's County`) |> 
           mutate(Total = sum(c_across(1:2), na.rm = TRUE))  |> 
           arrange(desc(Total)) |> ungroup() |> 
           gt(rowname_col = "Visitor's County") |> 
           sub_missing(missing_text = "-") |> 
           summary_rows(fns = list("Total" = ~sum(., na.rm = TRUE)),
                        formatter = fmt_number, decimals = 0) |> 
           tab_options(
                            data_row.padding = px(2),
                            summary_row.padding = px(3), # A bit more padding for summaries
                            row_group.padding = px(4)    # And even more for our groups
                        ) |> 
           opt_stylize(style = 6, color = 'gray')
   })
}


# Run the application 
shinyApp(ui = ui, server = server)



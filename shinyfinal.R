library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)



# Load data


data <- read.csv("DataCoSupplyChainDataset.csv")
data <- subset(data, select = -c(Customer.Email, Customer.Password, Order.Item.Cardprod.Id, Order.Zipcode, Product.Card.Id, Product.Description, Product.Image))

data <- na.omit(data)

# Convert order date to date format
data$order.date..DateOrders. <- as.Date(data$order.date..DateOrders., format = "%m/%d/%Y %H:%M")

# Create data for year dropdown menu
years <- data %>%
  group_by(year = lubridate::year(order.date..DateOrders.)) %>%
  summarise(total_sales = sum(Sales), total_profit = sum(Order.Profit.Per.Order))

# Create data for month dropdown menu
months <- data %>%
  group_by(month = lubridate::month(order.date..DateOrders., label = TRUE),
           year = lubridate::year(order.date..DateOrders.)) %>%
  summarise(total_sales = sum(Sales), total_profit = sum(Order.Profit.Per.Order))

# Create data for quarter dropdown menu
quarters <- data %>%
  group_by(quarter = quarters(order.date..DateOrders.),
           year = lubridate::year(order.date..DateOrders.)) %>%
  summarise(total_sales = sum(Sales), total_profit = sum(Order.Profit.Per.Order))


# UI
ui <- navbarPage("Shiny app for supply chain management data",
                 
                 #page 1 - Data Visualization 
                 
                 tabPanel(
                   "Data Visualization",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(
                         "visualization",
                         "List of  visualization",
                         choices = c(
                           "sales_histogram",
                           "delivery_status_bar_chart",
                           "top_10_categories",
                           "top_10_countries",
                           "category_name_vs_sales",
                           "shipping_mode_vs_late_delivery_risk",
                           "total_sales_by_department",
                           "top_10_order_countries",
                           "order_regions_by_count_of_orders_from_customers",
                           "top_20_customers"
                         )
                       )
                     ),
                     mainPanel(
                       plotOutput("sales_histogram"),
                       plotOutput("delivery_status_bar_chart"),
                       plotOutput("top_10_categories"),
                       plotOutput("top_10_countries"),
                       plotOutput("category_name_vs_sales"),
                       plotOutput("shipping_mode_vs_late_delivery_risk"),
                       plotOutput("total_sales_by_department"),
                       plotOutput("top_10_order_countries"),
                       plotOutput("order_regions_by_count_of_orders_from_customers"),
                       plotOutput("top_20_customers")
                     )
                   )
                 ),
                 
                 # Page-2
                 tabPanel("Sales and Profit Analysis",
                          # Sales and profit dropdown menu
                          selectInput("sales_profit_input", "Select sales or profit", choices = c("Sales", "Profit")),
                          
                          # Year dropdown menu
                          selectInput("year_input", "Select year", choices = years$year),
                          
                          # Month and quarter dropdown menu (hidden by default)
                          conditionalPanel(
                            condition = "input.year_input != ''",
                            selectInput("month_quarter_input", "Select month or quarter", 
                                        choices = c("Month" = "month", "Quarter" = "quarter"))
                          ),
                          
                          # Plot
                          plotOutput("sales_profit_plot")
                 ),
                 
                 # Page 3 - Data Table
                 tabPanel("Data Table",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("column", "Select a Column", choices = c("Days.for.shipping..real.", "Days.for.shipment..scheduled.", "Benefit.per.order", "Sales.per.customer", "Late_delivery_risk", "Customer.Id", "Department.Id", "Latitude", "Longitude", "Order.Customer.Id", "Order.Id", "Order.Item.Discount", "Order.Item.Discount.Rate", "Order.Item.Id", "Order.Item.Product.Price", "Order.Item.Profit.Ratio", "Order.Item.Quantity", "Sales", "Order.Item.Total", "Order.Profit.Per.Order", "Product.Category.Id", "Product.Price")),
                              sliderInput("range", "Select a Range", min = 0, max = 500, value = c(0, 100))
                            ),
                            mainPanel(
                              dataTableOutput("table")
                            )
                          )
                 ),
                 
                 # Page 3 - Plots
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("x_var", "Select X Variable", choices =c("Days.for.shipping..real.", "Days.for.shipment..scheduled.", "Benefit.per.order", "Sales.per.customer", "Late_delivery_risk", "Customer.Id", "Department.Id", "Latitude", "Longitude", "Order.Customer.Id", "Order.Id", "Order.Item.Discount", "Order.Item.Discount.Rate", "Order.Item.Id", "Order.Item.Product.Price", "Order.Item.Profit.Ratio", "Order.Item.Quantity", "Sales", "Order.Item.Total", "Order.Profit.Per.Order", "Product.Category.Id", "Product.Price")),
                              selectInput("y_var", "Select Y Variable", choices = c("Days.for.shipping..real.", "Days.for.shipment..scheduled.", "Benefit.per.order", "Sales.per.customer", "Late_delivery_risk", "Customer.Id", "Department.Id", "Latitude", "Longitude", "Order.Customer.Id", "Order.Id", "Order.Item.Discount", "Order.Item.Discount.Rate", "Order.Item.Id", "Order.Item.Product.Price", "Order.Item.Profit.Ratio", "Order.Item.Quantity", "Sales", "Order.Item.Total", "Order.Profit.Per.Order", "Product.Category.Id", "Product.Price"))
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                          )
                 ),
                 
                 # Page 4 - Maps
                 tabPanel("Map",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("lat", "Select a latitude variable:", choices = c("Latitude")),
                              selectInput("lng", "Select a longitude variable:", choices = c("Longitude")),
                              selectInput("intensity", "Select an intensity variable:", choices = c("Sales", "Benefit.per.order", "Order.Profit.Per.Order"))
                            ),
                            mainPanel(
                              leafletOutput("map")
                            )
                          ))
)

# Server
server <- function(input, output) {
  
  # Page 1 - Data visualization 
  sales_histogram <- function(data) {
    ggplot(data, aes(x = Customer.Id, fill = after_stat(x))) +
      geom_histogram() +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(x = "Sales per customer", y = "Count", title = "Distribution of Sales per Customer")
  }
  
  output$sales_histogram <- renderPlot({
    sales_histogram(data)
  })
  
  create_delivery_status_histogram <- function(data) {
    ggplot(data, aes(x = Delivery.Status, fill = Delivery.Status)) +
      geom_bar() +
      labs(x = "Delivery Status", y = "Count", title = "Delivery Status Distribution") +
      scale_fill_manual(values = c("green", "blue", "red", "orange", "purple")) # Set custom colors
  }
  
  
  output$delivery_status_bar_chart <- renderPlot({
    create_delivery_status_histogram(data)
  })

  late_delivery_category_plot <- function(data) {
    # Filter data based on "Late delivery" status
    late_delivery <- data %>%
      filter(Delivery.Status == "Late delivery")
    
    # Count the number of late deliveries by Category Name
    category_counts <- late_delivery %>%
      group_by(Category.Name) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      top_n(10)
    
    # Plot the top 10 categories by count of late deliveries
    ggplot(category_counts, aes(x = Category.Name, y = count, fill = Category.Name)) +
      geom_col() +
      labs(x = "Category Name", y = "Count of Late Deliveries", title = "Top 10 Categories by Late Delivery Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  output$top_10_categories <- renderPlot({
    late_delivery_category_plot(data)
  })
  
  plot_late_delivery_by_country <- function(data) {
    # Filter data based on "Late delivery" status
    late_delivery <- data %>%
      filter(Delivery.Status == "Late delivery")
    
    # Count the number of late deliveries by Order.Country
    country_counts <- late_delivery %>%
      group_by(Order.Country) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      top_n(10)
    
    # Plot the top 10 countries by count of late deliveries
    ggplot(country_counts, aes(x = Order.Country, y = count, fill = Order.Country)) +
      geom_col() +
      labs(x = "Country", y = "Count of Late Deliveries", title = "Top 10 Countries by Late Delivery Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  output$top_10_countries <- renderPlot({
    plot_late_delivery_by_country(data)
  })
  
  create_sales_boxplot <- function(data) {
    ggplot(data, aes(x = Category.Name, y = Sales, fill = Category.Name)) +
      geom_boxplot() +
      labs(title = "Category Name vs. Sales")
  }
  output$category_name_vs_sales <- renderPlot({
    create_sales_boxplot(data)
  })
  
  create_delivery_boxplot <- function(data) {
    ggplot(data, aes(x = Shipping.Mode, y = Late_delivery_risk, fill = Shipping.Mode)) +
      geom_boxplot() +
      labs(title = "Shipping Mode vs. Late Delivery Risk")
  }
  output$shipping_mode_vs_late_delivery_risk <- renderPlot({
    create_delivery_boxplot(data)
  })
  
  plot_sales_by_department <- function(data) {
    # Aggregate sales by department
    sales_by_department <- data %>%
      group_by(Department.Name) %>%
      summarize(total_sales = sum(Sales))
    
    # Create the plot
    # Create the plot
    ggplot(sales_by_department, aes(x = Department.Name, y = total_sales, fill = Department.Name, 
                                    text = paste("Department: ", Department.Name, "<br>", "Sales: $", 
                                                 scales::comma(total_sales)))) +
      geom_bar(stat = "identity") +
      labs(title = "Total Sales by Department", x = "Department", y = "Sales") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  }
  
  output$total_sales_by_department <- renderPlot({
    plot_sales_by_department(data)
  })
  
  top_10_orders_by_country <- function(data) {
    # calculate the count of orders by Order.Country
    orders_by_country <- data %>%
      group_by(Order.Country) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      slice(1:10)
    
    # create the plot 
    ggplot(orders_by_country, aes(x = Order.Country, y = count, fill = Order.Country, text = paste("Country: ", Order.Country, "<br>", "Orders: ", count))) +
      geom_bar(stat = "identity") +
      labs(title = "Top 10 Order Countries by Customer Orders") +
      xlab("Order Country") +
      ylab("Customer Orders") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  
  output$top_10_order_countries <- renderPlot({
    top_10_orders_by_country(data)
  })
  
  plot_order_region_count <- function(data) {
    
    # create a data frame with the count of orders by order region
    order_region_count <- data %>%
      group_by(Order.Region) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count)) 
    
    # create the plot
    ggplot(order_region_count, aes(x = Order.Region, y = Count, fill = Order.Region, text = paste("Order Region: ", Order.Region, "<br>", "Order Count: ", Count))) +
      geom_bar(stat = "identity") +
      labs(title = "Order Regions by Count of Orders from Customers") +
      xlab("Order Region") +
      ylab("Order Count") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_fill_viridis_d()
    
  }
  output$order_regions_by_count_of_orders_from_customers <- renderPlot({
    plot_order_region_count(data)
  })
  
  top_customers_sales_plot <- function(data) {
    
    # create a data frame with sales per customer
    sales_per_customer <- data %>%
      group_by(Customer.Id) %>%
      summarise(total_sales = sum(Sales)) %>%
      arrange(desc(total_sales))
    
    # get the top 20 customers who did the highest sales.
    top_customers <- head(sales_per_customer, 20)
    
    # create a new column with the combined first and last name of each customer
    top_customers <- top_customers %>%
      left_join(data %>% select(Customer.Id, Customer.Fname, Customer.Lname), by = "Customer.Id") %>%
      mutate(CustomerName = paste(Customer.Fname, Customer.Lname, sep = " "))
    
    # create the plot
    ggplot(top_customers, aes(x = reorder(CustomerName, -total_sales), y = total_sales, fill = CustomerName, text = paste("Customer Name: ", CustomerName, "<br>", "Total Sales: $", total_sales))) +
      geom_bar(stat = "identity") +
      labs(title = "Top_20_Customers") +
      xlab("Customer Name") +
      ylab("Total Sales") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  }
  output$top_20_customers <- renderPlot({
    top_customers_sales_plot(data)
  })
  
  # Filter data based on year selection
  year_data <- reactive({
    data %>%
      filter(lubridate::year(order.date..DateOrders.) == input$year_input)
  })
  
  # Filter data based on month or quarter selection
  filtered_data <- reactive({
    if (input$month_quarter_input == "month") {
      months %>%
        filter(year == input$year_input)
    } else {
      quarters %>%
        filter(year == input$year_input)
    }
  })
  
  # Create sales and profit plot
  output$sales_profit_plot <- renderPlot({
    if (input$sales_profit_input == "Sales") {
      ggplot(filtered_data(), aes(x = get(input$month_quarter_input), y = total_sales, fill = get(input$month_quarter_input))) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Set3") +
        labs(title = paste("Total Sales by", input$month_quarter_input, "in", input$year_input), 
             x = input$month_quarter_input, y = "Sales")
    } else {
      ggplot(filtered_data(), aes(x = get(input$month_quarter_input), y = total_profit, fill = get(input$month_quarter_input))) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Set3") +
        labs(title = paste("Total Profit by", input$month_quarter_input, "in", input$year_input), 
             x = input$month_quarter_input, y = "Profit")
    }
  })
  
  
  # Page 3 - Data Table
  output$table <- renderDataTable({
    data %>%
      filter(.data[[input$column]] >= input$range[1] & .data[[input$column]] <= input$range[2])%>%
      select(input$column) %>%
      filter(.data[[input$column]] >= input$range[1] & .data[[input$column]] <= input$range[2]) # Include the range condition in the select statement
  })
  
  # Page 4 - Plots
  output$plot <- renderPlot({
    ggplot(data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]])) +
      geom_point()
  })
  
  # Page 5 - Maps
  # Map page
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      addHeatmap(lng = ~get(input$lng), lat = ~get(input$lat), intensity = ~get(input$intensity))
  })
}

# Run the app
shinyApp(ui, server)

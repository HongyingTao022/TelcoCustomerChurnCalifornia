library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(tigris)

# Load the dataset
telco_data <- read.csv("data/telco.csv")

# Data cleaning and transformation
telco_data$City <- tolower(telco_data$City)
telco_data$Churn.Category[telco_data$Churn.Category == ""] <- "NoChurn"
telco_data <- telco_data %>%
  mutate(Offer_Status = ifelse(Offer == "None", "No Offer", "Has Offer"))

# Summarize the telco data
telco_summary <- telco_data %>%
  group_by(City) %>%
  summarize(Churn.Score = mean(as.numeric(Churn.Score), na.rm = TRUE),
            CLTV = mean(CLTV, na.rm = TRUE),
            Satisfaction.Score = mean(as.numeric(Satisfaction.Score), na.rm = TRUE),
            Churn_Rate = mean(as.numeric(Churn.Label == "Yes"), na.rm = TRUE),
            Avg_Charges = mean(Total.Charges, na.rm = TRUE))

# Convert Churn_Rate to percentage
telco_summary <- telco_summary %>%
  mutate(Churn_Rate = Churn_Rate * 100)

# Function to load California counties as an sf object
load_california_counties <- function() {
  counties <- counties(state = "CA", cb = TRUE)
  counties <- st_as_sf(counties)
  counties <- counties %>%
    select(NAME, geometry) %>%
    rename(subregion = NAME)
  counties$subregion <- tolower(counties$subregion)
  # Convert the coordinate system to WGS84
  counties <- st_transform(counties, crs = st_crs(4326))
  counties
}

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .note {
        color: #808080;
        font-size: 12px;
      }
    "))
  ),
  titlePanel("Telco Customer Churn in California"),
  tabsetPanel(
    tabPanel("Map",
             sidebarLayout(
               sidebarPanel(
                 selectInput("map_category", "Select Churn Category:",
                             choices = c("All", unique(telco_data$Churn.Category))),
                 selectInput("map_metric", "Select Metric:",
                             choices = c("Churn Score" = "Churn.Score",
                                         "CLTV" = "CLTV",
                                         "Satisfaction Score" = "Satisfaction.Score",
                                         "Churn Rate" = "Churn_Rate")),
                 conditionalPanel(
                   condition = "input.map_metric == 'Churn.Score'",
                   p(class = "note", "Churn Score: A value from 0-100 that is calculated using the predictive tool IBM SPSS Modeler. The model incorporates multiple factors known to cause churn. The higher the score, the more likely the customer will churn.")
                 ),
                 conditionalPanel(
                   condition = "input.map_metric == 'CLTV'",
                   p(class = "note", "CLTV: Customer Lifetime Value. A predicted CLTV is calculated using corporate formulas and existing data. The higher the value, the more valuable the customer. High value customers should be monitored for churn.")
                 ),
                 conditionalPanel(
                   condition = "input.map_metric == 'Satisfaction.Score'",
                   p(class = "note", "Satisfaction Score: A score from 1-5 that measures customer satisfaction. Higher scores indicate higher satisfaction levels.")
                 ),
                 conditionalPanel(
                   condition = "input.map_metric == 'Churn_Rate'",
                   p(class = "note", "Churn Rate: The proportion of customers who have churned. The higher the rate, the more customers have left.")
                 )
               ),
               mainPanel(
                 leafletOutput("californiaMap")
               )
             )
    ),
    tabPanel("Histograms",
             sidebarLayout(
               sidebarPanel(
                 selectInput("hist_variable", "Select Variable:",
                             choices = c("Churn Score" = "Churn.Score",
                                         "CLTV" = "CLTV",
                                         "Total Charges" = "Total.Charges")),
                 sliderInput("age_range", "Select Age Range:",
                             min = min(telco_data$Age, na.rm = TRUE), max = max(telco_data$Age, na.rm = TRUE),
                             value = c(min(telco_data$Age, na.rm = TRUE), max(telco_data$Age, na.rm = TRUE)),
                             step = 1),
                 radioButtons("fill_variable", "Select Fill/Legend Variable:",
                              choices = c("Gender" = "Gender",
                                          "Married" = "Married",
                                          "Lives with any dependents" = "Dependents",
                                          "Referred a Friend" = "Referred.a.Friend",
                                          "Offer Status" = "Offer_Status",
                                          "Satisfaction Score" = "Satisfaction.Score",
                                          "Status at the end of the quarter" = "Customer.Status",
                                          "Churn Category" = "Churn.Category")),
                 sliderInput("bins", "Number of bins:",
                             min = 5, max = 50, value = 15, step = 1)
               ),
               mainPanel(
                 plotOutput("variableHistogram")
               )
             )
    ),
    tabPanel("Additional Information",
             h3("Core Concepts and Insights"),
             p("The primary objective of these visualizations is to provide insights into customer churn, satisfaction, and value for a fictional telecommunications company in California."),
             h4("Map Visualization"),
             p("The map visualization displays average metrics by city, including churn score, CLTV (Customer Lifetime Value), satisfaction score, churn rate, and average charges. Users can select a metric to display on the map, and detailed information appears when hovering over each city. This helps in understanding regional differences in customer behavior and identifying areas with high churn rates or low customer satisfaction."),
             p("By analyzing churn score, companies can identify the likelihood of customers churning and take proactive measures to retain them. CLTV helps in understanding the long-term value of customers, guiding investment in customer retention strategies. Satisfaction scores provide insights into customer contentment, enabling targeted improvements in service quality."),
             h4("Histogram Visualization"),
             p("The histogram visualization allows users to explore the distribution of selected variables such as churn score, CLTV, and total charges. Users can filter the data by age range, gender, marital status, dependents, referrals, offer status, satisfaction score, and customer status. This interactive tool helps users identify patterns and correlations within the data, such as how different demographics or customer segments contribute to overall metrics."),
             p("Histograms reveal the underlying distribution of key metrics, enabling companies to understand customer behavior across different segments. For example, analyzing total charges can highlight spending patterns and potential revenue opportunities. By examining satisfaction scores, businesses can pinpoint areas needing improvement to enhance customer experience."),
             p("These visualizations aim to provide actionable insights for improving customer retention strategies and enhancing customer satisfaction by highlighting key areas of concern and potential opportunities. By leveraging these insights, companies can make data-driven decisions to foster customer loyalty and drive business growth.")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$californiaMap <- renderLeaflet({
    # Filter telco data based on selected Churn Category
    if (input$map_category == "All") {
      filtered_data <- telco_data
    } else {
      filtered_data <- telco_data %>%
        filter(Churn.Category == input$map_category)
    }
    
    # Summarize the filtered telco data
    filtered_summary <- filtered_data %>%
      group_by(City) %>%
      summarize(Churn.Score = mean(as.numeric(Churn.Score), na.rm = TRUE),
                CLTV = mean(CLTV, na.rm = TRUE),
                Satisfaction.Score = mean(as.numeric(Satisfaction.Score), na.rm = TRUE),
                Churn_Rate = mean(as.numeric(Churn.Label == "Yes"), na.rm = TRUE) * 100,
                Avg_Charges = mean(Total.Charges, na.rm = TRUE))
    
    # Load California counties map data
    counties <- load_california_counties()
    
    # Merge telco data with county data
    county_data <- counties %>%
      left_join(filtered_summary, by = c("subregion" = "City"))
    
    # Create a color palette based on the selected metric
    pal <- colorNumeric("viridis", domain = county_data[[input$map_metric]], na.color = "transparent")
    
    # Create the leaflet map
    leaflet(county_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(get(input$map_metric)),
                  weight = 1,
                  opacity = 1,
                  color = 'white',
                  dashArray = '3',
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 2,
                    color = '#666',
                    dashArray = '',
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = ~paste0(
                    "City: ", subregion, "<br>",
                    "Churn Score: ", round(Churn.Score, 2), "<br>",
                    "CLTV: ", round(CLTV, 2), "<br>",
                    "Satisfaction Score: ", round(Satisfaction.Score, 2), "<br>",
                    "Churn Rate: ", round(Churn_Rate, 2), "%<br>",
                    "Avg Charges: $", round(Avg_Charges, 2)
                  ) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"
                  )) %>%
      addLegend(pal = pal, values = county_data[[input$map_metric]], opacity = 0.7,
                title = input$map_metric, position = "bottomright",
                labFormat = labelFormat(suffix = if(input$map_metric == "Churn_Rate") "%" else ""))
  })
  
  output$variableHistogram <- renderPlot({
    # Filter telco data based on selected age range
    filtered_data <- telco_data %>%
      filter(Age >= input$age_range[1] & Age <= input$age_range[2])
    
    # Ensure fill variable is a factor
    filtered_data[[input$fill_variable]] <- as.factor(filtered_data[[input$fill_variable]])
    
    # Calculate bin width based on the number of bins
    binwidth <- (max(filtered_data[[input$hist_variable]], na.rm = TRUE) - 
                   min(filtered_data[[input$hist_variable]], na.rm = TRUE)) / input$bins
    
    # Plot the histogram of the selected variable with fill/legend variable
    ggplot(filtered_data, aes_string(x = input$hist_variable, fill = input$fill_variable)) +
      geom_histogram(binwidth = binwidth, color = "black", position = "stack") +
      labs(title = paste("Distribution of", input$hist_variable, "by", input$fill_variable),
           x = input$hist_variable,
           y = "Count",
           fill = input$fill_variable) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

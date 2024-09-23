library(shiny)
library(ggplot2)

results <- read.csv("") # this lines requires the specific dataset to insert
county_data <- read.csv("") # this lines requires the specific dataset to insert
feature_importance <- read.csv(" ") # this lines requires the specific dataset to insert

fips <- maps :: county.fips
names(fips)[which(names(fips) == "fips")] <- "FIPS" # explain this part
fips$region <- sapply(fips$polyname, function(x) { return(strsplit(x, ",")[[1]][1]) })
fips$subregion <- sapply(fips$polyname, function(x) { return(strsplit(x, ",")[[1]][2]) })

counties <- map_data("county")
counties <- merge(counties, fips, by=c("region", "subregion"))
counties <- counties[order(counties$order),]

features_names <- c("Unemployment_Rate",
  "Median_Household_Income",
  "Mean_Household_Income",
  "Proportion_Poverty",
  "Proportion_Uninsured",
  "Proportion_Home_No_Vehicle",
  "Proportion_Homeowners_35Perc_Income_on_Home",
  "Proportion_Renters_35Perc_Income_on_Rent",
  "Proportion_White",
  "Proportion_Black",
  "Proportion_American_Indian_Alaska_Native",
  "Proportion_Asian",
  "Proportion_Native_Hawaiian_Pacific_Islander",
  "Proportion_Male",
  "Proportion_High_School_Greater",
  "Proportion_Bachelors_Degree_or_Greater",
  "Employee_Capacity",
  "Total_Employees_March_Snapshot",
  "Total_Annual_Payroll",
  "Employee_Capacity_Change",
  "Total_Employees_March_Snapshot_Change",
  "Total_Annual_Payroll_Change",
  "Urbanicity",
  "ORx_per_100",
  "Fentanyl_Total",
  "Deaths",
  "Population"
)

ui <- fluidPage(
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "ML Performance",
      sidebarLayout(
        sidebarPanel(
          selectInput("Year", "Year:", c(2014:2021), selected=2021),
          selectInput("Lag", "Lag:", c(1:3), selected=1),
          selectInput("Window", "Window:", c(1:5), selected=1),
          selectInput("PredictionType", "Prediction Type:", c("Rates", "Counts"), selected="Rates")
        ),
        mainPanel(
          titlePanel("Performance Measures for Machine Learning Models"),
          plotOutput("MAEPlot"),
          plotOutput("RMSEPlot"),
          plotOutput("SpearmanPlot"),
          plotOutput("TopDecilePlot")
        )
      )
    ),
    tabPanel(
      "Feature Investigation",
      sidebarLayout(
        sidebarPanel(
          selectInput("FeatureYear", "Year:", c(2014:2021), selected=2014),
          selectInput("Feature", "Feature:", features_names, selected="Unemployment Rate"),
          selectInput(
            "OutcomeType", 
            "Outcome:", 
            c("Overdose Death Rates", "Overdose Death Counts"), 
            selected="Overdose Death Rates")
        ),
        mainPanel(
          titlePanel("Geographic Distribution of Feature and Outcome Values"),
          plotOutput("FeatureMap"),
          plotOutput("OutcomeMap")
        )
      )
    ),
    tabPanel(
      "Feature Importances",
      sidebarLayout(
        sidebarPanel(
          selectInput("LagImportance","Lag:",c(1:3),selected=1),
          selectInput("WindowImportance","Window:",c(1:5),selected=1),
          selectInput("PredictionTypeImportance", "Type of Prediction:", c("Rates", "Counts"), selected="Counts")
        ),
        mainPanel(
          titlePanel("Importance Rankings of Features for Random Forest Models"),
          plotOutput("FeatureImportanceGrid")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$MAEPlot = renderPlot({
    user_results <- results[results$Year == input$Year & results$Lag == input$Lag & results$Window == input$Window & results$PredictionType == input$PredictionType,]
    user_results$Model <- factor(user_results$Model,
                                 levels = c("tree", "boost", "linear", "neural", "forest", "svm"),
                                 labels = c("Decision\nTree", "Gradient\nBoosting", "Linear\nRegression",
                                            "Neural\nNetwork", "Random\nForest", "Support\nVector\nMachine"))
    
    ggplot(user_results, aes(x = Model, y = MAE, fill = Model)) +
      geom_bar(stat = "identity", fill = c("orange", "chartreuse4", "blue4", "purple", "brown2", "deeppink3"), color = "black") +
      ggtitle("Mean Absolute Error") +
      theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))
  })
  
  output$RMSEPlot = renderPlot({
    user_results <- results[results$Year == input$Year & results$Lag == input$Lag & results$Window == input$Window & results$PredictionType == input$PredictionType,]
    user_results$Model <- factor(user_results$Model,
                                 levels = c("tree", "boost", "linear", "neural", "forest", "svm"),
                                 labels = c("Decision\nTree", "Gradient\nBoosting", "Linear\nRegression",
                                            "Neural\nNetwork", "Random\nForest", "Support\nVector\nMachine"))
    ggplot(user_results, aes(x = Model, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity", fill = c("orange", "chartreuse4", "blue4", "purple", "brown2", "deeppink3"), color = "black") +
      ggtitle("Root Mean Squared Error") +
      theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))
  })
  
  output$SpearmanPlot = renderPlot({
    user_results <- results[results$Year == input$Year & results$Lag == input$Lag & results$Window == input$Window & results$PredictionType == input$PredictionType,]
    user_results$Model <- factor(user_results$Model,
                                 levels = c("tree", "boost", "linear", "neural", "forest", "svm"),
                                 labels = c("Decision\nTree", "Gradient\nBoosting", "Linear\nRegression",
                                            "Neural\nNetwork", "Random\nForest", "Support\nVector\nMachine"))
    ggplot(user_results, aes(x = Model, y = Spearman, fill = Model)) +
      geom_bar(stat = "identity", fill = c("orange", "chartreuse4", "blue4", "purple", "brown2", "deeppink3"), color = "black") +
      ggtitle("Spearman Correlation of County Rankings") +
      theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))
  })
  
  output$TopDecilePlot = renderPlot({
    user_results <- results[results$Year == input$Year & results$Lag == input$Lag & results$Window == input$Window & results$PredictionType == input$PredictionType,]
    user_results$Model <- factor(user_results$Model,
                                 levels = c("tree", "boost", "linear", "neural", "forest", "svm"),
                                 labels = c("Decision\nTree", "Gradient\nBoosting", "Linear\nRegression",
                                            "Neural\nNetwork", "Random\nForest", "Support\nVector\nMachine"))
    ggplot(user_results, aes(x = Model, y = TopDecile, fill = Model)) +
      geom_bar(stat = "identity", fill = c("orange", "chartreuse4", "blue4", "purple", "brown2", "deeppink3"), color = "black") +
      ggtitle("Percentage of Top Decile Agreement") +
      theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))
  })
  
  
  output$FeatureMap = renderPlot({
    year_data <- county_data[county_data$Year == input$FeatureYear,]
    
    # combine the county information with our data
    data_for_maps <- merge(year_data, counties, by=c("FIPS"))
    data_for_maps <- data_for_maps[order(data_for_maps$order),]
    
    # get the data column chosen by the user
    feature_column <- switch(feature_name, 
                             "Unemployment Rate" = data_for_maps$Unemployment_Rate,
                             "Opioid Perscriptions (per 100k)" = data_for_maps$ORx_per_100,
                             "Median Household Income" = data_for_maps$Median_Household_Income,
                             "Mean Household Income" = data_for_maps$Mean_Household_Income,
                             "Proportion Poverty" = data_for_maps$Proportion_Poverty,
                             "Proportion Uninsured" = data_for_maps$ Proportion_Uninsured,
                             "Proportion of Homes with No Vehicle" = data_for_maps$Proportion_Homes_No_Vehicle,
                             "Proportion of Homeowners 35 Percent Income on Home" = data_for_maps$Proportion_Homeowners_35Perc_Income_on_Home,
                             "Proportion of Renters 35 Perc Income on Rent" = data_for_maps$Proportion_Renters_35Perc_Income_on_Rent,
                             "Proportion who are White" = data_for_maps$Proportion_White,
                             "Proportion who are Black" = data_for_maps$Proportion_Black,
                             "Proportion of American-Indian-Alaska Native" = data_for_maps$Proportion_American_Indian_Alaska_Native,
                             "Proportion who are Asian" = data_for_maps$Proportion_Asian,
                             "Proportion of Native Hawaiian-Pacific Islander" = data_for_maps$Proportion_Native_Hawaiian_Pacific_Islander,
                             "Proportion of Male"= data_for_maps$Proportion_Male,
                             "Proportion High School or Greater" = data_for_maps$Proportion_High_School_or_Greater,
                             "Proportion Bachelors Degree or Greater" = data_for_maps$Proportion_Bachelors_Degree_or_Greater,
                             "Employee Capacity" = data_for_maps$Employee_Capacity,
                             "Total Employees March Snapshot" = data_for_maps$Total_Employees_March_Snapshot,
                             "Total Annual Payroll" = data_for_maps$Total_Annual_Payroll_Change,
                             "Employee Capacity Change" = data_for_maps$Employee_Capacity_Change,
                             "Total Employees March Snapshot Change" = data_for_maps$Total_Employees_March_Snapshot_Change,
                             "Total Annual Payroll Change" = data_for_maps$Total_Annual_Payroll_Change,
                             "Urbanicity" = data_for_maps$Urbanicity,
                             "Fentanyl Total" = data_for_maps$Fentanyl_Total,
                             "Population" = data_for_maps$Population,
                             "overdose gravity add" = data_for_maps$overdose_gravity_add
    )
    
    # a map plotting the feature chosen by the user
    ggplot(data_for_maps, aes(long, lat, group = group, fill=log(feature_column))) +
      scale_fill_continuous(name=input$Feature, low="blue", high="green", na.value="grey50") +
      geom_polygon(colour = "black") + 
      coord_quickmap() +
      ggtitle(paste0(input$Feature, " in ", input$FeatureYear))
    # theme(text = element_text(size=28))
  })
  
  output$OutcomeMap = renderPlot({
    year_data <- county_data[county_data$Year == input$FeatureYear,]
    
    # combine the county information with our data
    data_for_maps <- merge(year_data, counties, by=c("FIPS"))
    data_for_maps <- data_for_maps[order(data_for_maps$order),]
    
    # get the data column chosen by the user
    outcome_column <- switch(input$OutcomeType, 
                             "Overdose Death Rates" = data_for_maps$overdose_death_rate,
                             "Overdose Death Counts" = data_for_maps$Deaths
    )
    
    # a map plotting the feature chosen by the user
    ggplot(data_for_maps, aes(long, lat, group = group, fill=log(outcome_column))) +
      scale_fill_continuous(name=input$OutcomeType, low="blue", high="green", na.value="grey50") +
      geom_polygon(colour = "black") + 
      coord_quickmap() +
      ggtitle(paste0(input$OutcomeType, " in ", input$FeatureYear))
    # theme(text = element_text(size=28))
  })
  
  output$FeatureImportanceGrid = renderPlot({
    importance <- feature_importance[feature_importance$Lag == input$LagImportance & 
                                       feature_importance$Window == input$WindowImportance &
                                       feature_importance$PredictionType == input$PredictionTypeImportance,]
    importance$Feature <- factor(importance$Feature, levels=sort(unique(importance$Feature), decreasing=TRUE))
    importance$Year <- factor(importance$Year, levels=sort(unique(importance$Year)))
    
    ggplot(data=importance, aes(x = Year, y = Feature, fill = Rank)) +
      geom_tile(color = "black") +
      scale_fill_gradient(low = "red", high = "white") #+
      #coord_fixed()
  }, height=1200)
}


# Run the application 
shinyApp(ui = ui, server = server)

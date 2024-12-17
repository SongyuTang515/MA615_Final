#Import package
library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(plotly)
library(maps)
library(forecast)

#Import data
economy_data <- read.csv("table/gdp.csv")
cpi_data <- read.csv("table/cpi.csv")
comparison_data <- list(
  social = data.frame(
    Indicator = c("Population (2023)", "Population Growth (%)", "Life Expectancy (Years)", 
                  "Population Density (per km²)", "Human Development Index (HDI)", "Sex Ratio (M/F)"),
    `Sri Lanka` = c("21.8 Million", "0.5%", "77.2", "345", "0.782", "95.1"),
    `Maldives` = c("0.5 Million", "1.2%", "79.9", "1700", "0.747", "101.6"),
    `Indonesia` = c("277 Million", "0.7%", "71.7", "151", "0.718", "101.3"),
    `Madagascar` = c("29.6 Million", "2.4%", "66.3", "48", "0.501", "99.3")
  ),
  economic = data.frame(
    Indicator = c("GDP (2023, USD Billion)", "GDP Growth Rate (%)", "Inflation Rate (%)", 
                  "Unemployment Rate (%)", "Tourism Contribution to GDP (%)"),
    `Sri Lanka` = c("84.5", "3.1%", "6.3%", "5.4%", "12.6%"),
    `Maldives` = c("6.1", "6.5%", "3.8%", "5.1%", "28.5%"),
    `Indonesia` = c("1,390", "5.2%", "3.3%", "6.5%", "5.8%"),
    `Madagascar` = c("15.1", "4.4%", "8.2%", "6.1%", "6.9%")
  )
)


# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  tags$head(tags$style(HTML("
    body {
      background-color: #ADD8E6 !important; /* Light blue background */
    }
    .header {
      background-color: #87CEEB; /* Sky blue header */
      color: black;
      padding: 0 15px;
      height: 60px;
      display: flex;
      align-items: center;
      justify-content: space-between;
      border-bottom: 3px solid #003366;
    }
    .header h1 {
      font-size: 36px;
      font-weight: bold;
      margin: 0;
      line-height: 60px;
    }
    .sidebar {
      background-color: transparent !important;
      color: white !important;
      height: 100vh;
      padding: 0;
    }
    .sidebar .nav-title {
      background-color: transparent;
      color: #003366;
      font-size: 18px;
      font-weight: bold;
      text-align: center;
      padding: 12px 0;
      margin: 0;
    }
    .content-section {
      background-color: white;
      border-radius: 10px;
      padding: 20px;
      margin-top: 10px;
      box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
    }
    h3 {
      font-size: 22px;
      color: #003366;
      border-bottom: 2px solid #00509E;
      padding-bottom: 10px;
      margin-top: 0;
    }
  "))),
  
  div(class = "header",
      h1("Maldives"),
      tags$img(
        src = "https://upload.wikimedia.org/wikipedia/commons/0/0f/Flag_of_Maldives.svg",
        height = "55px", style = "margin-right: 10px;")
  ),
  
  fluidRow(
    column(
      2,
      div(class = "sidebar",
          div(class = "nav-title", "Navigation"),
          navlistPanel(
            id = "nav",
            tabPanel("Overview"),
            tabPanel("Prediction"),
            tabPanel("Comparison"),
            tabPanel("SWOT Analysis"),
            tabPanel("Reference"),
            widths = c(12, 10)
          )
      )
    ),
    
    column(
      10,
      div(class = "content-section", uiOutput("main_content"))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Render UI for Overview
  output$main_content <- renderUI({
    if (input$nav == "Overview") {
      fluidPage(
        h3("Maldives Overview"),
        tabsetPanel(
          tabPanel("Country Map", 
                   tags$div(
                     h4("Map Description:"),
                     p("This map provides a detailed view of the Maldives, showing three kinds of maps. 
                      Users can select a kind of map from the dropdown menu to zoom in and explore more specific geographical details."),
                     leafletOutput("country_map", height = "500px")
                   )
          ),
          tabPanel("World Position", 
                   tags$div(
                     h4("Map Description:"),
                     p("This detailed map provides a clear visualization of the geographical positions of Maldives on the global landscape. 
                      The map is designed with an interactive feature, allowing users to zoom in for a closer look and uncover more intricate geographical details."),
                     leafletOutput("world_map", height = "500px")
                   )
          ),
          tabPanel("Economy", 
                   tags$div(
                     h4("Economy Overview"),
                     p("This section provides insights into Maldives' GDP, Growth Rate, CPI, and Inflation Rate."),
                     
                     
                     radioButtons("economy_plot_choice", "Select a Chart to Display:",
                                  choices = c("GDP & Growth Rate" = "gdp", 
                                              "CPI & Inflation Rate" = "cpi"),
                                  selected = "gdp", inline = TRUE),
                     
                     
                     conditionalPanel(
                       condition = "input.economy_plot_choice == 'gdp'",
                       sliderInput("gdp_slider", "Select Year Range for GDP:",
                                   min = 2003, max = 2023, value = c(2003, 2023), step = 1)
                     ),
                     conditionalPanel(
                       condition = "input.economy_plot_choice == 'cpi'",
                       sliderInput("cpi_slider", "Select Year Range for CPI:",
                                   min = 1985, max = 2023, value = c(1985, 2023), step = 1)
                     ),
                     
                     
                     plotlyOutput("economy_plot", height = "500px")
                   )
          )
          ,
          tabPanel("Ethnity", 
                   p("The largest ethnic group is Dhivehin, native to the historic region of the Maldive Islands comprising today's Republic of Maldives and the island of Minicoy in Union territory of Lakshadweep, India. They share the same culture and speak the Dhivehi language. They are principally an Indo-Aryan people, closely related to the Sinhalese and having traces of Middle Eastern, South Asian, Austronesian and African genes in the population.

In the past there was also a small Tamil population known as the Giraavaru people. This group have now been almost completely absorbed into the larger Maldivian society but were once native to the island of Giraavaru (Kaafu Atoll). This island was evacuated in 1968 due to heavy erosion of the island.

Filipinos in the Maldives numbering 3,000 in 2018."),
                   tags$img(src = "images/demographic.jpg", height = "500px", width = "100%", 
                            style = "display: block; margin: 10px auto; border-radius: 10px;")
                   ),
          tabPanel("Geography", 
                   p("Maldives is an island country in the Indian Ocean, South Asia, south-southwest of India. It has a total land size of 298 km2 (115 sq mi) which makes it the smallest country in Asia. It consists of approximately 1,190 coral islands grouped in a double chain of 26 atolls, spread over roughly 90,000 square kilometers, making this one of the most geographically dispersed countries in the world. It has the 31st largest exclusive economic zone of 923,322 km2 (356,497 sq mi).[citation needed] Composed of live coral reefs and sand bars, the atolls are situated atop a submarine ridge, 960 km (600 mi) long that rises abruptly from the depths of the Indian Ocean and runs from north to south. Only near the southern end of this natural coral barricade do two open passages permit safe ship navigation from one side of the Indian Ocean to the other through the territorial waters of Maldives. For administrative purposes the Maldives government organized these atolls into twenty-one administrative divisions."),
                   tags$img(src = "images/geograph.jpg", height = "500px", width = "100%", 
                            style = "display: block; margin: 10px auto; border-radius: 10px;")),
          tabPanel("History", 
                   p("The history of the Maldives is intertwined with the history of the broader Indian subcontinent and the surrounding regions, comprising the areas of South Asia and the Indian Ocean. The modern nation is formed of 26 natural atolls, comprising 1194 islands. Historically, the Maldives has held strategic importance due to its location on the major marine routes of the Indian Ocean. The Maldives's nearest neighbors are the British Indian Ocean Territory, Sri Lanka and India. The United Kingdom, Sri Lanka, and some Indian kingdoms have had cultural and economic ties with the Maldives for centuries. In addition to these countries, Maldivians also traded with Aceh and many other kingdoms in what is today Indonesia and Malaysia. The Maldives provided the primary source of cowrie shells, which were then used as currency throughout Asia and parts of the East African coast. Most likely, the Maldives were influenced by the Kalingas of ancient India. The Kalingas were the earliest region of India to trade with Sri Lanka and the Maldives and were responsible for the spread of Buddhism. Stashes of Chinese crockery found buried in various locations in the Maldives also show that there was direct or indirect trade contact between China and the Maldives. In 1411 and 1430, the Chinese admiral Zheng He visited the Maldives. The Chinese also became the first country to establish a diplomatic office in the Maldives when the Chinese nationalist government based in Taipei opened an embassy in Malé in 1966. The Embassy of the People's Republic of China has since replaced this office.

After the 16th century, when colonial powers took over much of the trade in the Indian Ocean, politics in the Maldives were interfered with by first the Portuguese, then the Dutch, and the French. However, this interference ended when the Maldives became a British Protectorate in the 19th century. The Maldivian monarchs were granted a measure of self-governance.

The Maldives gained total independence from the British on 26 July 1965. However, the British continued to maintain an air base on the island of Gan in the southernmost atoll until 1976. The British departure in 1976 at the height of the Cold War almost immediately triggered foreign speculation about the future of the air base. The Soviet Union requested the use of the base, but the Maldives refused.
"),
                   )
        )
      )
    }
    
    # Render UI for Prediction
    else if(input$nav == "Prediction") {
      tabPanel("Prediction", 
               tags$div(
                 h4("GDP and Growth Rate Prediction"),
                 p("This section presents predictions for GDP and Growth Rate in Maldives using Linear Regression (LM) and Time Series (TS) models."),
                 
                 # Existing Plots
                 
                 h5("GDP Prediction: LM vs Time Series"),
                 plotlyOutput("gdp_combined_plot", height = "400px"),
                 
                 h5("Growth Rate Prediction: LM vs Time Series"),
                 plotlyOutput("growth_combined_plot", height = "400px"),
                 
                 tags$hr(),
                 
                 # New Combined Growth Rate Comparison Plot
                 h5("Growth Rate Comparison: Calculated vs Predicted"),
                 p("This chart compares growth rates derived from predicted GDP values (LM and TS) with directly predicted growth rates."),
                 plotlyOutput("combined_growthrate_plot", height = "500px"),
                 
                 tags$ul(
                   tags$li("1. **Different Modeling Approaches**:"),
                   tags$p("   - The calculated Growth Rate is derived indirectly from predicted GDP values using the formula:"),
                   tags$code("Growth Rate = (GDP_current - GDP_previous) / GDP_previous * 100"),
                   tags$p("   - In contrast, directly predicted Growth Rate is modeled independently based on its historical data, without relying on GDP predictions."),
                   
                   tags$li("2. **Error Accumulation in Calculated Growth Rates**:"),
                   tags$p("   - Calculated Growth Rates are sensitive to errors in the predicted GDP values. If the GDP model (LM or TS) has any inaccuracies or deviations, these errors will propagate and magnify in the calculated Growth Rate."),
                   tags$p("   - Directly predicted Growth Rates avoid this issue, as they are forecasted separately and are not influenced by GDP predictions."),
                   
                   tags$li("3. **Data Characteristics and Trend Differences**:"),
                   tags$p("   - GDP data often follows a smoother, more stable upward trend, making it easier for models like linear regression (LM) to predict."),
                   tags$p("   - Growth Rate data, however, tends to exhibit more variability, fluctuations, or even cycles, which time series models like ARIMA may capture better."),
                   
                   tags$li("4. **Assumptions of the Models**:"),
                   tags$p("   - Linear regression assumes a linear relationship between variables and predicts values by extending this trend into the future."),
                   tags$p("   - Time series models like ARIMA or ETS are designed to capture patterns such as trends, seasonality, and short-term fluctuations present in historical data."),
                   
                   tags$li("5. **Impact of Historical Data Fluctuations**:"),
                   tags$p("   - If historical GDP data contains anomalies, such as sudden economic shocks or rapid growth periods, these will influence the calculated Growth Rates."),
                   tags$p("   - Conversely, the directly predicted Growth Rate models may smooth out these anomalies depending on how well they capture the underlying patterns."),
                   
                   tags$li("6. **Timing and Smoothing Effects**:"),
                   tags$p("   - Calculating Growth Rates from predicted GDP values introduces a lag, as the growth is computed between consecutive years."),
                   tags$p("   - Directly predicting Growth Rates allows the model to focus on smoothing or forecasting trends without the dependency on GDP values.")
                 ),
                 
                 p("In summary, the two methods differ because the calculated Growth Rate depends on predicted GDP values and is affected by their accuracy, whereas the directly predicted Growth Rate is independently modeled and reflects its own historical trends.")
               )
               )
    }
    
    # Render UI for Comparison
    else if (input$nav == "Comparison"){
      tabPanel("Comparison", 
               tags$div(
                 h4("Comparison Between Regional Island Nations"),
                 p("This section provides a comparative analysis of key social and economic indicators between Sri Lanka, Maldives, Indonesia, and Madagascar."),
                 
                 # Strengths and Weaknesses Summary for Maldives
                 tags$div(
                   style = "margin-bottom: 20px; padding: 10px; background-color: #f2f2f2; border: 1px solid #ccc; border-radius: 5px;",
                   h5("Strengths and Weaknesses of Maldives Compared to Other Nations"),
                   tags$p(
                     "The Maldives stands out in several areas compared to Sri Lanka, Indonesia, and Madagascar. "
                   ),
                   tags$ul(
                     tags$li(tags$b("Strengths:"),
                             " The Maldives has the highest population density (1700 people per km²), reflecting its small size and urban concentration."),
                     tags$li("It boasts the highest ", tags$b("Life Expectancy"), " at 79.9 years, indicating strong healthcare relative to the region."),
                     tags$li("Tourism contributes a significant portion (28.5%) to its GDP, demonstrating its global appeal as a tourist destination."),
                     tags$li(tags$b("Economic Growth:"), " Maldives shows robust GDP growth rates at 6.5%, surpassing other nations in recent years.")
                   ),
                   tags$ul(
                     tags$li(tags$b("Weaknesses:"),
                             " Despite its strengths, the Maldives has limited land area and natural resources, making it highly dependent on tourism."),
                     tags$li("Its ", tags$b("small population"), " (0.5 million) and narrow economic base pose challenges for diversification and resilience."),
                     tags$li("Climate change poses a major threat due to rising sea levels, which endanger its low-lying islands.")
                   )
                 ),
                 
                 # Dropdown menu for selecting comparison indicators
                 selectInput("comparison_type", "Select Indicator Type for Comparison:",
                             choices = c("Social Indicators" = "social", 
                                         "Economic Indicators" = "economic"),
                             selected = "social"),
                 
                 # Comparison Table
                 tableOutput("comparison_table"),
                 
                 tags$hr(),
                 
                 # Flags Section Below Table
                 tags$div(
                   style = "display: flex; justify-content: center; gap: 50px; margin-top: 20px;",
                   
                   # Sri Lanka Flag
                   tags$div(
                     style = "text-align: center;",
                     tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/1/11/Flag_of_Sri_Lanka.svg", 
                              height = "100px", style = "border: 3px solid black;"),
                     tags$p("Sri Lanka")
                   ),
                   
                   # Maldives Flag
                   tags$div(
                     style = "text-align: center;",
                     tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/0/0f/Flag_of_Maldives.svg", 
                              height = "100px", style = "border: 3px solid black;"),
                     tags$p("Maldives")
                   ),
                   
                   # Indonesia Flag
                   tags$div(
                     style = "text-align: center;",
                     tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/9/9f/Flag_of_Indonesia.svg", 
                              height = "100px", style = "border: 3px solid black;"),
                     tags$p("Indonesia")
                   ),
                   
                   # Madagascar Flag
                   tags$div(
                     style = "text-align: center;",
                     tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/b/bc/Flag_of_Madagascar.svg", 
                              height = "100px", style = "border: 3px solid black;"),
                     tags$p("Madagascar")
                   )
                 )
               )
      )
    }
    
    # Render UI for SWOT Analysis
    else if (input$nav == "SWOT Analysis") {
      tabPanel("SWOT Analysis", 
               tags$div(
                 h3("SWOT Analysis of Maldives"),
                 p("This section highlights the Strengths, Weaknesses, Opportunities, and Threats related to the Maldives."),
                 tabsetPanel(
                   tabPanel("Strengths",
                            tags$div(
                              h4("Strengths"),
                              p("1. The Maldives is a world-renowned destination with stunning coral reefs, white sandy beaches, and clear waters, attracting high-end tourists and divers."),
                              p("2. Located in the Indian Ocean, it has a strategic position for tourism from Asia, Europe, and the Middle East."),
                              p("3. The country's recent political stability has made it more appealing for international investment and tourism growth."),
                              p("4. Its unique blend of Islamic culture and oceanic traditions offers a distinctive cultural experience."),
                              tags$img(src = "images/strengths.jpg", height = "500px", width = "100%", 
                                       style = "display: block; margin: 10px auto; border-radius: 10px;")
                            )
                   ),
                   tabPanel("Weaknesses",
                            tags$div(
                              h4("Weaknesses"),
                              p("1.Limited land and natural resources make the Maldives heavily reliant on imports for basic needs."),
                              p("2. The economy depends heavily on tourism, making it vulnerable to global travel disruptions and economic downturns."),
                              p("3. Many islands lack developed infrastructure, limiting their potential for economic diversification."),
                              tags$img(src = "images/weaknesses.jpg", height = "500px", width = "100%", 
                                       style = "display: block; margin: 10px auto; border-radius: 10px;")
                            )
                   ),
                   tabPanel("Opportunities",
                            tags$div(
                              h4("Opportunities"),
                              p("1. Sustainable tourism and eco-friendly practices offer opportunities to attract environmentally conscious travelers."),
                              p("2. Diversifying into renewable energy, fishing, and digital services can reduce reliance on tourism."),
                              p("3. International partnerships can help develop infrastructure like airports, ports, and energy systems."),
                              p("4. Smart tourism technology such as virtual reality and digital platforms can enhance visitor experiences."),
                              tags$img(src = "images/opportunities.jpg", height = "500px", width = "100%", 
                                       style = "display: block; margin: 10px auto; border-radius: 10px;")
                            )
                   ),
                   tabPanel("Threats",
                            tags$div(
                              h4("Threats"),
                              p("1. Rising sea levels and natural disasters like flooding threaten both infrastructure and livelihoods."),
                              p("2. Global economic shifts and competition from other tropical destinations like Seychelles and Mauritius pose challenges."),
                              p("3. Political and social risks, though relatively low, could harm the Maldives' international reputation and investments."),
                              tags$img(src = "images/threats.jpg", height = "500px", width = "100%", 
                                       style = "display: block; margin: 10px auto; border-radius: 10px;")
                            )
                   )
                 )
               )
      )
    }
    
    # Render UI for Reference
    else if (input$nav == "Reference"){
      tabPanel("Reference", 
               tags$div(
                 h4("References"),
                 p("Below is the list of references used for data, images, and information in this app:"),
                 tags$ul(
                   tags$li(tags$a(href = "https://en.wikipedia.org/wiki/Maldives", "Maldives - Wikipedia", target = "_blank")),
                   tags$li(tags$a(href = "https://en.wikipedia.org/wiki/Madagascar", "Madagascar - Wikipedia", target = "_blank")),
                   tags$li(tags$a(href = "https://en.wikipedia.org/wiki/Sri_Lanka", "Sri Lanka - Wikipedia", target = "_blank")),
                   tags$li(tags$a(href = "https://en.wikipedia.org/wiki/Indonesia", "Indonesia - Wikipedia", target = "_blank")),
                   tags$li(tags$a(href = "https://mastering-shiny.org/index.html", "Mastering Shiny", target = "_blank")),
                   tags$li(tags$a(href = "https://shiny.posit.co/r/getstarted/shiny-basics/lesson7/", 
                                  "Shiny Basics Tutorial - Posit", target = "_blank")),
                   tags$li(tags$a(href = "https://statisticsmaldives.gov.mv/yearbook/2024/#", 
                                  "Maldives Statistics Yearbook 2024", target = "_blank")),
                   tags$li(tags$a(href = "https://r4ds.hadley.nz/", "R for Data Science", target = "_blank")),
                   tags$li(tags$a(href = "https://www.pexels.com/", "Pexels - Free Stock Photos", target = "_blank")),
                   tags$li(tags$a(href = "https://documents1.worldbank.org/curated/en/613221468282288419/pdf/402820MV0ICA0P09247201PUBLIC1.pdf", 
                                  "World Bank Maldives Economic Report", target = "_blank"))
                 ),
                 p("These references have been used to collect data, generate insights, and build this interactive Shiny application.")
               )
      )
    }
  })
  
  # Leaflet Map for Maldives and Major Cities/Islands with Layers Control
  output$country_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Map") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Map") %>%
      setView(lng = 73.543, lat = 1.977, zoom = 6) %>%
      addMarkers(lng = 73.5089, lat = 4.1755, popup = "Malé (Capital City)") %>%
      addMarkers(lng = 72.8381, lat = 5.1573, popup = "Baa Atoll") %>%
      addMarkers(lng = 73.152, lat = 0.308, popup = "Addu City") %>%
      addLayersControl(
        baseGroups = c("Standard Map", "Satellite Map", "Topo Map"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Leaflet Map for World Position with Layers Control
  output$world_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard Map") %>%
      setView(lng = 0, lat = 0, zoom = 2) %>%
      addMarkers(lng = 73.543, lat = 1.977, popup = "Maldives")
  })
  
  # Economy Plot
  output$economy_plot <- renderPlotly({
    if (input$economy_plot_choice == "gdp") {
      req(economy_data)
      
      filtered_gdp <- subset(economy_data, Year >= input$gdp_slider[1] & Year <= input$gdp_slider[2])
      
      
      plot_ly(filtered_gdp, x = ~Year) %>%
        add_bars(y = ~GDP, name = "GDP", marker = list(color = "steelblue")) %>%
        add_lines(y = ~Growth_Rate, name = "Growth Rate (%)", 
                  line = list(color = "red", width = 3), yaxis = "y2") %>%
        layout(title = "Maldives GDP and Growth Rate",
               xaxis = list(title = "Year"),
               yaxis = list(title = "GDP (in millions MVR)", side = "left"),
               yaxis2 = list(title = "Growth Rate (%)", side = "right", overlaying = "y"),
               barmode = "group")
    } else if (input$economy_plot_choice == "cpi") {
      req(cpi_data)
      
      filtered_cpi <- subset(cpi_data, Year >= input$cpi_slider[1] & Year <= input$cpi_slider[2])
      
      
      plot_ly(filtered_cpi, x = ~Year) %>%
        add_bars(y = ~CPI, name = "CPI", marker = list(color = "green")) %>%
        add_lines(y = ~INFLATION, name = "Inflation Rate (%)", 
                  line = list(color = "orange", width = 3), yaxis = "y2") %>%
        layout(title = "Maldives CPI and Inflation Rate",
               xaxis = list(title = "Year"),
               yaxis = list(title = "CPI Values", side = "left"),
               yaxis2 = list(title = "Inflation Rate (%)", side = "right", overlaying = "y"),
               barmode = "group")
    }
  })

  output$gdp_combined_plot <- renderPlotly({
    req(economy_data)
    
    # Linear Regression Model for GDP
    gdp_lm_model <- lm(GDP ~ Year, data = economy_data)
    future_years <- data.frame(Year = seq(2024, 2030, by = 1))
    gdp_lm_forecast <- predict(gdp_lm_model, newdata = future_years)
    
    # Time Series Forecast for GDP
    gdp_ts <- ts(economy_data$GDP, start = min(economy_data$Year), frequency = 1)
    gdp_forecast <- forecast(auto.arima(gdp_ts), h = 7)
    
    # Combine Data
    gdp_combined <- data.frame(
      Year = c(economy_data$Year, future_years$Year),
      Actual_GDP = c(economy_data$GDP, rep(NA, length(future_years$Year))),
      LM_Prediction = c(rep(NA, nrow(economy_data)), gdp_lm_forecast),
      TS_Prediction = c(rep(NA, nrow(economy_data)), gdp_forecast$mean)
    )
    
    # Plot Combined Results
    plot_ly(gdp_combined, x = ~Year) %>%
      add_lines(y = ~Actual_GDP, name = "Actual GDP", line = list(color = "black")) %>%
      add_lines(y = ~LM_Prediction, name = "LM Prediction", line = list(color = "blue", dash = "dot")) %>%
      add_lines(y = ~TS_Prediction, name = "TS Prediction", line = list(color = "red", dash = "solid")) %>%
      layout(title = "GDP Predictions: LM vs Time Series",
             xaxis = list(title = "Year"),
             yaxis = list(title = "GDP (in millions MVR)"))
  })
  
  # Growth Rate Predictions: Combined LM and TS
  output$growth_combined_plot <- renderPlotly({
    req(economy_data)
    
    # Linear Regression Model for Growth Rate
    growth_lm_model <- lm(Growth_Rate ~ Year, data = economy_data)
    future_years <- data.frame(Year = seq(2024, 2030, by = 1))
    growth_lm_forecast <- predict(growth_lm_model, newdata = future_years)
    
    # Time Series Forecast for Growth Rate
    growth_ts <- ts(economy_data$Growth_Rate, start = min(economy_data$Year), frequency = 1)
    growth_forecast <- forecast(auto.arima(growth_ts), h = 7)
    
    # Combine Data
    growth_combined <- data.frame(
      Year = c(economy_data$Year, future_years$Year),
      Actual_Growth = c(economy_data$Growth_Rate, rep(NA, length(future_years$Year))),
      LM_Prediction = c(rep(NA, nrow(economy_data)), growth_lm_forecast),
      TS_Prediction = c(rep(NA, nrow(economy_data)), growth_forecast$mean)
    )
    
    # Plot Combined Results
    plot_ly(growth_combined, x = ~Year) %>%
      add_lines(y = ~Actual_Growth, name = "Actual Growth Rate", line = list(color = "black")) %>%
      add_lines(y = ~LM_Prediction, name = "LM Prediction", line = list(color = "blue", dash = "dot")) %>%
      add_lines(y = ~TS_Prediction, name = "TS Prediction", line = list(color = "red", dash = "solid")) %>%
      layout(title = "Growth Rate Predictions: LM vs Time Series",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Growth Rate (%)"))
  })
  
  # Combined Growth Rate Plot: LM and TS Predictions vs Calculated Growth Rate
  output$combined_growthrate_plot <- renderPlotly({
    req(economy_data)
    
    # 1. LM Model for GDP Prediction
    gdp_lm_model <- lm(GDP ~ Year, data = economy_data)
    future_years <- data.frame(Year = seq(2024, 2030, by = 1))
    gdp_lm_forecast <- predict(gdp_lm_model, newdata = future_years)
    
    # 2. Time Series Model for GDP Prediction
    gdp_ts <- ts(economy_data$GDP, start = min(economy_data$Year), frequency = 1)
    gdp_ts_forecast <- forecast(auto.arima(gdp_ts), h = 7)$mean
    
    # 3. Calculate Growth Rate from LM-predicted GDP
    gdp_lm_combined <- c(tail(economy_data$GDP, 1), gdp_lm_forecast)
    lm_growth_calculated <- diff(gdp_lm_combined) / head(gdp_lm_combined, -1) * 100
    
    # 4. Calculate Growth Rate from TS-predicted GDP
    gdp_ts_combined <- c(tail(economy_data$GDP, 1), gdp_ts_forecast)
    ts_growth_calculated <- diff(gdp_ts_combined) / head(gdp_ts_combined, -1) * 100
    
    # 5. Directly Predicted Growth Rates
    growth_lm_model <- lm(Growth_Rate ~ Year, data = economy_data)
    growth_lm_forecast <- predict(growth_lm_model, newdata = future_years)
    
    growth_ts <- ts(economy_data$Growth_Rate, start = min(economy_data$Year), frequency = 1)
    growth_ts_forecast <- forecast(auto.arima(growth_ts), h = 7)$mean
    
    # 6. Combine All Results
    combined_growth <- data.frame(
      Year = future_years$Year,
      LM_Growth_Calculated = lm_growth_calculated,
      TS_Growth_Calculated = ts_growth_calculated,
      LM_Growth_Predicted = growth_lm_forecast,
      TS_Growth_Predicted = growth_ts_forecast
    )
    
    # 7. Plot All Four Growth Rates
    plot_ly(combined_growth, x = ~Year) %>%
      add_lines(y = ~LM_Growth_Calculated, name = "LM Growth (from GDP)", 
                line = list(color = "blue", dash = "solid")) %>%
      add_lines(y = ~TS_Growth_Calculated, name = "TS Growth (from GDP)", 
                line = list(color = "red", dash = "solid")) %>%
      add_lines(y = ~LM_Growth_Predicted, name = "LM Predicted Growth", 
                line = list(color = "blue", dash = "dot")) %>%
      add_lines(y = ~TS_Growth_Predicted, name = "TS Predicted Growth", 
                line = list(color = "red", dash = "dot")) %>%
      layout(title = "Growth Rate Comparison: Calculated vs Predicted",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Growth Rate (%)"))
  })
  
  # Render comparison table
  output$comparison_table <- renderTable({
    req(input$comparison_type)
    comparison_data[[input$comparison_type]]
  }, striped = TRUE, bordered = TRUE, hover = TRUE, align = "c")
  
  
}

# Run App
shinyApp(ui, server)


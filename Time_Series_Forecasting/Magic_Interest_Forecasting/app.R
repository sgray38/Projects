library(shiny)
library(shinyjs)
library(tsibble)
library(ggplot2)
library(fpp3)
library(plotly)
library(dplyr)
library(zoo)
library(urca)
mtg_ts <- read.csv("mtg_timeline.csv", skip = 1)
names(mtg_ts) <- c("Week", "mtg_interest")
mtg_ts <- mtg_ts |>
  mutate(Week = yearweek(Week)) |>
  tsibble(index = Week)

#Any Gaps?
mtg_ts <- mtg_ts|>
  fill_gaps()
sum(is.na(mtg_ts)) #uh oh

# I think linear interpolation makes the most sense, move smoothly from previous to next value
#Sure
mtg_ts <- mtg_ts |>
  mutate(mtg_interest = replace_na(na.approx(mtg_interest)))

mtg_comps <- mtg_ts |>
  model(STL(mtg_interest)) |>
  components()

ACFs <- ACF(mtg_ts, mtg_interest)

full_sdrift <- mtg_ts |>
  model(SNAIVE(mtg_interest ~ drift()))

full_tslm <- mtg_ts |>
  model(TSLM(mtg_interest ~ trend() + season()))

full_ets <- mtg_ts |>
  model(ETS(mtg_interest))

full_arima <- mtg_ts |>
  model(ARIMA(mtg_interest))


sets <- c(
  'Rivals of Ixalan',
  'Battlebond',
  'Mystery Booster',
  'Dominaria',
  'Guilds of Ravnica',
  'Ravnica Allegiance',
  'War of the Spark',
  'Throne of Eldraine',
  'Theros Beyond Death',
  'Ikoria: Lair of Behemoths',
  'Zendikar Rising',
  'Commander Legends',
  'Kaldheim',
  'Time Spiral Remastered',
  'Strixhaven: School of Mages',
  'Modern Horizons 2',
  'Dungeons & Dragons',
  'Innistrad: Midnight Hunt',
  'Innistrad: Crimson Vow',
  'Kamigawa: Neon Dynasty',
  'Streets of New Capenna',
  'Dominaria United',
  "The Brothers' War",
  'Dominaria Remastered ',
  'Phyrexia: All Will Be One',
  'March of the Machine',
  'March of the Machine: The Aftermath',
  'Commander Masters',
  'The Lord of the Rings: Tales of Middle-earth',
  'Wilds of Eldraine',
  'The Lost Caverns of Ixalan',
  'Murders at Karlov Manor',
  'Ravnica Remastered ',
  'Outlaws of Thunder Junction',
  'Modern Horizons 3',
  'Bloomburrow',
  'Duskmourn: House of Horror',
  'Innistrad Remastered '
)

set_dates <- c(
  'January 13, 2018', #Ixalan
  'June 8, 2018', #Battlebond
  'March 13, 2020', #Mystery Booster
  'April 21, 2018', #Dominaria
  'September 29, 2018', #Guilds
  'January 19, 2019', #Ravnica Allegiance
  'April 27, 2019', #War of the Spark
  'September 28, 2019', #Throne
  'January 17, 2020', #Theros
  'April 17, 2020', #Ikoria
  'September 18, 2020', #Zendikar
  'November 20, 2020', #Commander Legends
  'January 29, 2021', #Kaldheim
  'March 19, 2021', #Time Spiral Remastered
  'April 16, 2021', #Strixhaven
  'June 18, 2021', #Modern Horizons 2
  'July 23, 2021', #DnD
  'September 24, 2021', #Innistrad
  'November 19, 2021', #Innistrad 2
  'February 18, 2022', #Kamigawa
  'April 29, 2022', #Streets
  'September 9, 2022', #Dominaria U
  'November 18, 2022', #Brother's War
  'January 13, 2023', #Dominaria Remastered
  'February 10, 2023', #Phy
  'April 21, 2023', #March
  'May 12, 2023', #March +
  'August 4, 2023', #Com Mast
  'June 23, 2023', #LOTR
  'September 8, 2023', #Wilds
  'November 17, 2023', #Lost Caverns
  'February 9, 2024', #Murders
  'January 12, 2024', #Rav Rem
  'April 19, 2024', #Outlaws
  'June 14, 2024', #MH3
  'August 2, 2024', #Bloomburrow
  'September 27, 2024', #Duskmourn
  'January 24, 2025' #Innistrad Rem
)


announcements <- c(
  'Signature Spellbook: Jace',
  'Dominaria Frame, Template, and Rules Changes',
  'Commander Anthology Vol. II Legends and Decklists',
  'Deathrite/Probe Ban Legacy',
  'State of Design 2018',
  'Guilds of Ravnica Guild Kits',
  'Ravnica Allegiance Guild Kits',
  'No More MSRP',
  'MTG Arena Banned and Restricted Announcement (2019)',
  'Krark-Clan Ban Modern',
  'Gitaxian Probe Pauper Ban',
  'The London Mulligan',
  'Pauper Comes to Paper',
  'Commander Banned List and Philosophy Update',
  'Hogaak Ban',
  'Announcing the Pioneer Format',
  'Field of the Dead Ban Standard',
  'Oko Ban Standard',
  'Oko Ban Pioneer',
  'Oko Ban Modern',
  'Golos, Once upon a Time, Breach bans',
  'Lutri Ban',
  'Lurrus Ban Legacy',
  'Companion Rules Change',
  'Teferi/Cauldron Familiar Ban',
  'Announcement Day',
  'Uro Ban',
  'Omnath Ban',
  "Thassa's Oracle Ban",
  'Middle Earth is Legal',
  'Pricing Update',
  'Spellslingers Announcement',
  '2023 Announcements',
  'Meathook and Yorion Bans',
  'Unfinity and Stickers',
  'MTG 30th Anniversary Debacle',
  'MTG And Marvel',
  'Generative AI Controversy',
  'Miku Secret Lair',
  'Mystery Booster 2',
  'Controversial Commander Ban',
  'Wizards Takes Over Commander',
  'First Marvel Drop',
  'MSRP Returns'
)


announcement_dates <- c(
  'Mar 20, 2018', #Signature Spellbook Jace
  'Mar 21, 2018', #Dominaria Frame, Template, Rules
  'May 8, 2018', #Commander Anthology
  'Jul 2, 2018', #Banned and Restricted 2018
  'Aug 20, 2018', #State of Design
  'Sep 25, 2018', #Guildkits 1
  'Jan 15, 2019', #Guildkits 2
  'Jan 21, 2019', #B&R
  'Feb 14, 2019', #Arena Banned And Restricted
  'Feb 18, 2019', #No More MSRP
  'May 20, 2019', #B&R
  'Jun 3, 2019', #London Mulligan
  'Jun 27, 2019', #Pauper
  'Jul 8, 2019', #Commander ban list
  'Aug 26, 2019', #B&R
  'Oct 21, 2019', #Pioneer
  'Oct 21, 2019', #B&R
  'Nov 18, 2019', #B&R
  'Dec 16, 2019', #Oko Ban, pioneer
  'Jan 13, 2020', #Oko Ban, modern
  'Mar 9, 2020', #Golos, OUAT, Breach ban
  'Apr 13, 2020', #Lutri Ban
  'May 18, 2020', #Lurrus Ban
  'Jun 1, 2020', # Companion rule change
  'Aug 3, 2020', #Teferi/Cauldron Familiar Ban
  'Sep 1, 2020', #Announcement Day
  'September 28, 2020', #Uro Ban
  'Oct 12, 2020', #Omnath Ban
  'May 19, 2021', #Thassa's Oracle Ban
  'Aug 24, 2021', #Middle Earth legal
  'Apr 19, 2022', #Pricing Update
  'Aug 12, 2022', #Spellslingers
  'Aug 18, 2022', #2023 Announcements
  'Oct 4, 2022', #30th Debacle
  'Oct 10, 2022', #Meathook and Yorion Bans
  'Oct 18, 2022', #Unfinity Rules Update
  'Oct 23, 2023', #MTG and Marvel
  'Jan 7, 2024', #Generative AI
  'Apr 29, 2024', #Miku
  'Aug 2, 2024', #Mystery Booster 2
  'Sep 23, 2024', #Controversial Commander Ban
  'Sep 30, 2024', #Wizards take over commander
  'Oct 18, 2024', #First marvel Drop
  'Oct 25, 2024' #MSRP Returns
)
set_df <- data.frame(Sets = sets, Week = set_dates)

announce_df <- data.frame(Announcements = announcements, Week = announcement_dates)

set_df <- set_df |>
  mutate(Week = yearweek(Week))

announce_df <- announce_df |>
  mutate(Week = yearweek(Week))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  
  titlePanel("MTG Time Series Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Introduction"),
      p("This app provides an interactive analysis of Magic: The Gathering interest over time, including visualization, decomposition, and autocorrelation.
       This data comes from https://trends.google.com/trends/explore?date=2018-01-01%202025-01-31&geo=US&q=%2Fm%2F04rhz&hl=en"),
      h4("How to Use"),
      p("Select a visualization type below to explore different aspects of the time series data."),
      selectInput("view_type", "Choose View", choices = c("Full Time Series",
                                                          "Seasonality", 
                                                          "Autocorrelation", 
                                                          "Announcement", 
                                                          "Set",
                                                          "Forecasting!")),
      
      uiOutput("dynamic_dropdown"),
      br(),
      h4("Decomposition Method"),
      p("I used STL for this decomposition, choosing it over classical decomposition because it can accommodate fluctuating seasonality.
       The interest in Magic changes for a multitude of reasons, so while Seasonality is still strong it can't be assumed that it's going to be constant and STL can handle that better than classical decomp.
         Also, since the data isn't very complex, STL won't eat up too many resources."),
      br(),
      h4("Time Series Decomposition"),
      p(
      HTML("Time series decomposition is the process of stripping the data down to the following composites:
        Trend, Seasonality, and Irregularity. Trend is found by taking the moving average of the data, pulling out only the average y value.
        STL uses a smoothing technique called LOESS to calculate the trend, which is more versatile than a simple moving average. The trend for this data appears to drop down until 2020, then begin to elevate again. This makes sense, as the beginning of 2020 was when Covid strangled in-person games and activities. <br><br>
        After removing the trend from the data with simple subtraction, we can take the average (by period) of our detrended data. 
        In this case, our period is 'weekly' so we take the average of Week X for each year, and fill that value in for each week's 'Seasonal' value.
        The LOESS technique is used here to smooth out each year's seasonality and can capture fluctations per year.  There's some strong seasonality in this data, with summer months having a larger interest than Fall or Spring months. This also makes sense because Magic is largely played by young adults, who don't have school in the summer. <br><br>
        After subtracting out both trend and seasonality, whatever is left over is the 'Irregularities' in the data. In this case, irregularities can likely be attributed to announcements or set releases, which I hope to capture in the \"Announcements\" and \"Sets\" sections of this app.
        <br><br>The graph to the right is the full STL decomposition of the data. ")
      )
    ),
    
    mainPanel(
      h3("Interpretation"),
      textOutput("interpretation"),
      plotOutput("main_plot"),
      uiOutput("model_description"),
      uiOutput("star_animation"),
      conditionalPanel(
        condition = "input.view_type == 'Announcement' || input.view_type == 'Set'",
        plotOutput("secondary_plot")
      ),
      h3("Time Series Decomposition"),
      plotOutput("ts_decomp")
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamic UI for announcement or set selection
  output$dynamic_dropdown <- renderUI({
    if(input$view_type == "Announcement") {
      selectInput("announcement", "Select Announcement", choices = unique(announce_df$Announcements))
    } else if(input$view_type == "Set") {
      selectInput("set", "Select Set", choices = unique(set_df$Sets))
    } else if(input$view_type == "Forecasting!") {
      selectInput("forecast", "Select Forecast Type", choice = c("Seasonal Drift", "TSLM", "Exponential Smoothing", "ARIMA"))
    }
  })
  
  # Function to create the main and secondary plots
  render_plots <- reactive({
    main_plot <- ggplot(mtg_ts, aes(x = Week, y = mtg_interest)) +
      geom_line() +
      labs(title = "MTG Interest Over Time")
    header_text <- "Time Series Decomposition"
    secondary_plot <- NULL

    if(input$view_type == "Announcement" && !is.null(input$announcement)) {
      pick_date <- announce_df %>%
        filter(Announcements == input$announcement) %>%
        pull(Week)
      main_plot <- mtg_ts |>
        autoplot() +
        geom_line(aes(pick_date), color = "red") + 
        geom_line(aes(pick_date + 10), color = "green") + 
        geom_line(aes(pick_date - 10), color = "green")
      secondary_plot <- ggplot(mtg_ts %>% filter(Week >= pick_date - 10 & Week <= pick_date + 10), aes(x = Week, y = mtg_interest)) +
        geom_line() +
        geom_line(aes(pick_date), color = "red") + 
        labs(title = paste("Interest Around", input$announcement))
    } else if(input$view_type == "Set" && !is.null(input$set)) {
      pick_date <- set_df %>%
        filter(Sets == input$set) %>%
        pull(Week)
      main_plot <- mtg_ts |>
        autoplot() +
        geom_line(aes(pick_date), color = "red") + 
        geom_line(aes(pick_date + 10), color = "green") + 
        geom_line(aes(pick_date - 10), color = "green")
      secondary_plot <- ggplot(mtg_ts %>% filter(Week >= pick_date - 10 & Week <= pick_date + 10), aes(x = Week, y = mtg_interest)) +
        geom_line() +
        geom_line(aes(pick_date), color = "red") + 
        labs(title = paste("Interest Around", input$set))
    }else if(input$view_type == "Seasonality"){
      main_plot <- mtg_comps |>
        gg_season(season_year) + ggtitle("Seasonality Plot")
    }else if(input$view_type == "Autocorrelation"){
      main_plot <- ACFs |>
        autoplot()
    }else if(input$view_type == "Forecasting!" && !is.null(input$forecast)){
      if(input$forecast == "Seasonal Drift") {
        main_plot <- forecast(full_sdrift, h = 24) |> autoplot(mtg_ts)
      } else if(input$forecast == "TSLM") {
          main_plot <- forecast(full_tslm, h = 24) |> autoplot(mtg_ts)
      } else if(input$forecast == "Exponential Smoothing") {
          main_plot <- forecast(full_ets, h = 24) |> autoplot(mtg_ts)
      } else if(input$forecast == "ARIMA") {
          main_plot <- forecast(full_arima, h = 24) |> autoplot(mtg_ts)
      }
    }
  
    list(main_plot = main_plot, secondary_plot = secondary_plot)
  })
  # Interpretation Text
  output$interpretation <- renderText({
    if (input$view_type == "Full Time Series") {
      "This is the full time series data. It shows overall trend and variations in the interest in Magic: The Gathering over time. 
      This is from a google trend dataset containing the relative interest in Magic: The Gathering from 2018 to early 2025. 
      Since the data is normalized, the minimum value in mtg_interest is 55 and the maximum value is 100. 
      This is relative to the time frame, with 100 representing the maximum interest and 55 representing that the term is half as popular."
      
    } else if (input$view_type == "Seasonality") {
      "The seasonality plot helps identify repeating patterns over time. The fluctuations in seasonality from year to year is the main reason I used STL instead of classical decomposition. Even with this variation though, there is a clear increase in interest in the summer weeks and a lower interest in the winter weeks."
      
    } else if (input$view_type == "Autocorrelation") {
      "Autocorrelation is the relationship the data has with itself in the previous weeks. The largest ACF value is at a lag of 1 Week, meaning the interest each week has the strongest correlation with the previous week's interest. The fact that there is a positive, diminishing pattern of correlation for each lag tells us that Trend dominates Seasonality in this data, but the small humps that can be seen shows that seasonality does exist."
      
    } else if (input$view_type == "Announcement"){
      "This plot grabs the week of your chosen announcement and the 10 weeks leading up to and following the release of that announcement. This allows you to see if the announcement had an effect on the interest in the preceeding and following weeks!"
      
    } else if (input$view_type == "Set"){
      "This plot grabs the week that your chosen set was released and the 10 weeks leading up to and following that release. This allows you to see if the set coming out had an effect on the interest in the preceeding and following weeks!"
    } else if (input$view_type == "Forecasting!"){
      "This is the forecasting plot. It takes the previous data and attempts to predict what the future might hold. The light blue line is the predicted value of interest for the next 24 weeks. The blue cones are the confidence intervals, showing how far off the predicted values could potentially be while staying in the scope of the model."
    }
  })
  
  # Render the main plot
  output$main_plot <- renderPlot({
    render_plots()$main_plot
  })
  
  
  output$model_description <- renderUI({
    req(input$forecast)
    
    description <- switch(input$forecast,
                          "Seasonal Drift" = "This model assumes that seasonal patterns repeat and the trend continues in straight line between the last data point and the next. 
                          While this model seems reasonable, its values go above 100, which is the maximum this data can ever be. It's not ideal.",
                          "TSLM" = "A Time Series Linear Model fits a linear regression model to time series data. 
                          It's using the Trend and the Seasonality as its main predictors. Surprisingly, this model appears to be the best of the four. 
                          It looks like it captures previous trends and seasonality quite well, despite the volatility of interest over time.",
                          "Exponential Smoothing" = "This method gives more weight to recent observations. 
                          Unfortunately, this one is <em>too</em> smooth, and doesn't capture a lot of the spiky nature of the data",
                          "ARIMA" = "AutoRegressive Integrated Moving Average (ARIMA) uses differencing and autoregression to predict.
                          It's a very capable model, but it seems to be conservative in its predictions.",
                          "Choose a forecasting model to view its description."
    )
    
    HTML(paste0("<hr><b>Model Description:</b><br>", description))
  })
  
  
  # Render the secondary plot
  output$secondary_plot <- renderPlot({
    render_plots()$secondary_plot
  })
  output$ts_decomp <- renderPlot({
    mtg_comps |>
      autoplot()
  })
}

shinyApp(ui = ui, server = server)

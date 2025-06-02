library(shiny)
library(bslib)
library(fpp3)
library(tidyverse)
library(plotly)
library(DBI)

mydb <- dbConnect(RSQLite::SQLite(), "mtg.sqlite")
#dbListTables(mydb)
#dbWriteTable(mydb, "price_list", price_list, overwrite = TRUE)
carddb <- dbConnect(RSQLite::SQLite(), 'cards.sqlite')

#dbGetQuery(carddb, "SELECT * FROM card_list")
ui <- fluidPage(
  titlePanel("Card Data"),
  sidebarLayout(
    sidebarPanel(
      navlistPanel(
        id = "sidebar_tabs",
        tabPanel("Top 5 Price Changes"),
        tabPanel("Card Search")
      ),
      conditionalPanel(
        condition = "input.sidebar_tabs === 'Card Search'",
        selectInput("rarity_type", "Choose Rarity", choices = dbGetQuery(carddb, 'SELECT DISTINCT rarity FROM card_list')),
        uiOutput("dynamic_dropdown"),
        uiOutput("card_image")
      )
    ),
    mainPanel(
      uiOutput("main_panel_content")
    )
  )
)


server <- function(input, output) {
  
  # Compute Top 5 Price Changes
  top_price_changes <- reactive({
    names <- dbGetQuery(mydb, 'SELECT DISTINCT name FROM price_list')$name
    
    changes <- data.frame(name = character(), change = numeric(), stringsAsFactors = FALSE)
    
    for (n in names) {
      temp <- dbGetQuery(mydb, 'SELECT price FROM price_list WHERE name = ? ORDER BY date DESC',
                         params = list(n))
      if (nrow(temp) >= 2) {
        diff_price <- temp$price[1] - temp$price[2]
        changes <- rbind(changes, data.frame(name = n, change = diff_price))
      }
    }
    
    # Sort by absolute value of change
    top_5 <- changes[order(-abs(changes$change)), ][1:5, ]
    return(top_5)
  })
  output$top_5_table <- renderTable({
    top_price_changes()
  })
  output$top_5_plot <- renderPlotly({
    df <- top_price_changes()
    
    p <- ggplot(df, aes(x = reorder(name, change), y = change, fill = change > 0)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "forestgreen")) +
      labs(x = "Card Name", y = "Price Change", fill = "Increase?", title = "Top 5 Most Recent Price Changes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  
  output$dynamic_dropdown <- renderUI({
    rarity_type <- input$rarity_type
    
    card_choices <- dbGetQuery(carddb, 'SELECT DISTINCT name FROM card_list WHERE "rarity" == :x',
                               params = list(x = rarity_type)) |>
      arrange(name)
    
    selectInput("set", "Select Card", choices = card_choices)
  })
  
  selected_card <- reactive({
    if (!is.null(input$card_name)) {
      input$card_name
    } else if (!is.null(input$set)) {
      input$set
    } else {
      NULL
    }
  })
  
  output$pricePlot <- renderPlotly({
    name <- selected_card()
    if (is.null(name)) return(NULL)
    
    temp <- dbGetQuery(mydb, 'SELECT * FROM price_list WHERE "name" == :x',
                       params = list(x = name))
    if (nrow(temp) == 0) return(NULL)
    
    temp %>%
      mutate(date = as.Date(date)) |>
      tsibble(index = date) %>%
      autoplot(price) %>%
      ggplotly()
  })
  
  output$card_image <- renderUI({
    name <- selected_card()
    if (is.null(name)) return(NULL)
    
    card_row <- dbGetQuery(carddb, 'SELECT image_url FROM card_list WHERE "name" == :x',
                           params = list(x = name))
    
    div(style = "text-align: center;",
        tags$img(src = card_row$image_url[1], height = "450px")
    )
  })
  
  output$main_panel_content <- renderUI({
    if (input$sidebar_tabs == "Top 5 Price Changes") {
      tagList(
        h3("Top 5 Price Changes"),
        plotlyOutput("top_5_plot"),
        tableOutput("top_5_table")
      )
    } else if (input$sidebar_tabs == "Card Search") {
      plotlyOutput("pricePlot")
    }
  })
  
  
}

shinyApp(ui = ui, server = server)

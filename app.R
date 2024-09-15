# Load required libraries
library(shiny)
library(shinymaterial)
library(highcharter)
library(echarts4r)
library(dplyr)
library(DT)
library(bslib)

# Sample Star Wars data
star_wars_data <- data.frame(
  character = c("Luke Skywalker", "Darth Vader", "Princess Leia", "Han Solo", "Yoda", "Obi-Wan Kenobi"),
  force_power = c(85, 95, 75, 60, 100, 90),
  battles_won = c(10, 15, 8, 12, 20, 18),
  lightsaber_color = c("blue", "red", "blue", "blue", "green", "blue"),
  side = c("Light", "Dark", "Light", "Light", "Light", "Light")
)

# UI
ui <- material_page(
  title = "Star Wars Universe Explorer",
  nav_bar_color = "black",
  background_color = "black",
  tags$head(
    tags$style(HTML("
      body { background-color: #000000; color: #FFD700; }
      .parallax-container { height: 100vh; }
      .content-section { 
        background-color: rgba(0, 0, 0, 0.7);
        padding: 20px;
        margin-top: -100vh;
        min-height: 100vh;
      }
      .card { background-color: rgba(28, 28, 28, 0.8); }
      .card-title { color: #FFD700; }
      .intro-title {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        font-size: 3rem;
        text-align: center;
        color: #FFD700;
        text-shadow: 2px 2px 4px #000000;
      }
    "))
  ),
  
  # Intro Section
  material_parallax(
    image_source = "https://wallpaperaccess.com/full/11801.jpg"
  ),
  div(class = "content-section",
      h1("Embark on Your Galactic Adventure: Explore the Star Wars Universe Like Never Before!", 
         class = "intro-title", 
         style = "color: #FFD700; text-align: center;")
  ),
  
  # Overview Section
  material_parallax(
    image_source = "https://wallpaperaccess.com/full/513448.jpg"
  ),
  div(class = "content-section",
      # Value Boxes
      div(
        class = "row",
        style = "display: flex; justify-content: space-around;",
        div(class = "col-md-4",
            div(
              class = "value-box bg-primary text-light",
              style = "padding: 20px; border-radius: 5px; text-align: center;",
              h3("Total Characters"),
              h2(textOutput("total_characters")),
              icon("users", lib = "font-awesome"),
              p("Total number of characters")
            )
        ),
        div(class = "col-md-4",
            div(
              class = "value-box bg-success text-light",
              style = "padding: 20px; border-radius: 5px; text-align: center;",
              h3("Avg Force Power"),
              h2(textOutput("avg_force_power")),
              icon("bolt", lib = "font-awesome"),
              p("Average force power of characters")
            )
        ),
        div(class = "col-md-4",
            div(
              class = "value-box bg-warning text-dark",
              style = "padding: 20px; border-radius: 5px; text-align: center;",
              h3("Total Battles Won"),
              h2(textOutput("total_battles_won")),
              icon("trophy", lib = "font-awesome"),
              p("Total number of battles won")
            )
        )
      ),
      material_row(
        material_column(
          width = 6,
          material_card(
            title = "Force Power Distribution",
            highchartOutput("force_power_chart")
          )
        ),
        material_column(
          width = 6,
          material_card(
            title = "Battles Won",
            echarts4rOutput("battles_won_chart")
          )
        )
      ),
      material_row(
        material_column(
          width = 12,
          material_card(
            title = "Star Wars Communities",
            highchartOutput("communities_chart")
          )
        )
      )
  ),
  
  # Character Stats Section
  material_parallax(
    image_source = "https://wallpaperaccess.com/full/321094.jpg"
  ),
  div(class = "content-section",
      material_card(
        title = "Character Statistics",
        # reactableOutput("character_table")
        DTOutput("character_table")
        
      )
  )
)
# Server
server <- function(input, output, session) {
  output$total_characters <- renderText({
    nrow(star_wars_data)
  })
  
  output$avg_force_power <- renderText({
    round(mean(star_wars_data$force_power), 1)
  })
  
  output$total_battles_won <- renderText({
    sum(star_wars_data$battles_won)
  })
  
  output$force_power_chart <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column", backgroundColor = "rgba(0, 0, 0, 0)") %>%
      hc_title(text = "Force Power by Character", style = list(color = "#FFD700")) %>%
      hc_xAxis(categories = star_wars_data$character, labels = list(style = list(color = "#FFD700"))) %>%
      hc_yAxis(title = list(text = "Force Power", style = list(color = "#FFD700")), labels = list(style = list(color = "#FFD700"))) %>%
      hc_add_series(name = "Force Power", data = star_wars_data$force_power) %>%
      hc_colors(c("#FFD700")) %>%
      hc_tooltip(pointFormat = "{point.y}") %>%
      hc_plotOptions(column = list(colorByPoint = TRUE))
  })
  
  output$battles_won_chart <- renderEcharts4r({
    star_wars_data %>%
      e_charts(character) %>%
      e_pie(battles_won, radius = c("50%", "70%")) %>%
      e_title("", textStyle = list(color = "#FFD700"), subtextStyle = list(color = "#FFD700"), titleAlign = "center") %>%
      e_theme("dark") %>%
      e_color(c("#FFD700", "#4169E1", "#32CD32", "#FF4500", "#9370DB", "#1E90FF")) %>%
      e_tooltip(trigger = "item", formatter = "{a} <br/>{b}: {c} ({d}%)") %>%
      e_legend(orient = "vertical", left = "left", textStyle = list(color = "#FFD700"))
  })
  
  output$communities_chart <- renderHighchart({
    df <- data.frame(
      stringsAsFactors = FALSE,
      name = c("Jedi Order", "Sith Empire", "Rebel Alliance", "Galactic Empire", "Mandalorians", "Hutts"),
      count = c(150, 100, 200, 180, 80, 50),
      col = c("#00FF00", "#FF0000", "#FFE81F", "#CCCCCC", "#257d98", "#8B4513"), # Jedi green, Sith red, Rebel yellow, Galactic grey, Mandalorian grey, Hutt brown
      abbrv = c("JO", "SE", "RA", "GE", "MN", "HT")
    )
    
    hchart(
      df,
      "item",
      hcaes(
        name = name,
        y = count,
        label = abbrv,
        color = col
      ),
      name = "Members",
      showInLegend = TRUE,
      size = "100%",
      center = list("50%", "75%"),
      startAngle = -100,
      endAngle  = 100
    ) %>%
      hc_chart(backgroundColor = "#000000") %>% # Black background
      hc_title(text = "Star Wars Communities", style = list(color = "#FFD700", fontSize = "20px")) %>% # Gold title
      hc_legend(labelFormat = '{name} <span style="opacity: 0.8">{y}</span>', itemStyle = list(color = "#FFD700")) %>% # Gold legend
      hc_tooltip(pointFormat = "<b>{point.name}</b>: {point.y} members", backgroundColor = "#333333", borderColor = "#FFD700", style = list(color = "#FFD700")) %>% # Dark tooltip with gold text
      hc_plotOptions(
        pie = list(
          dataLabels = list(
            style = list(color = "#000000") # Gold data labels
          ),
          borderColor = NULL, # Remove the border color
          borderWidth = 0 # Remove the border width
        )
      )
  })

  output$character_table <- renderDT({
    datatable(
      star_wars_data,
      options = list(
        dom = 't',  # This shows only the table, removing all other elements
        ordering = FALSE,  # Disables column sorting
        paging = FALSE,  # Disables pagination
        info = FALSE,  # Removes "Showing X to Y of Z entries"
        searching = FALSE,  # Removes the search bar
        autoWidth = TRUE,
        columnDefs = list(
          list(
            targets = 1,
            render = JS("
            function(data, type, row, meta) {
              if (type === 'display') {
                var color = row[3]; // lightsaber color
                var beam = '<div style=\"display: inline-block; width: 100px; height: 20px; background: linear-gradient(to right, ' + color + ' ' + data + '%, transparent ' + data + '%); border-radius: 0 10px 10px 0;\"></div>';
                var hilt = '<div style=\"display: inline-block; width: 20px; height: 20px; background-color: #808080; border-radius: 3px;\"></div>';
                return hilt + beam + ' ' + data;
              }
              return data;
            }
          ")
          ),
          list(
            targets = 3,
            render = JS("
            function(data, type, row, meta) {
              if (type === 'display') {
                return '<div style=\"background-color: ' + data + '; width: 20px; height: 20px; border-radius: 50%;\"></div>';
              }
              return data;
            }
          ")
          )
        )
      ),
      rownames = FALSE,
      colnames = c("Character", "Force Power", "Battles Won", "Lightsaber Color", "Side"),
      style = 'bootstrap',
      class = 'compact',
      selection = 'none'
    ) %>% 
      formatStyle(
        columns = c('character', 'force_power', 'battles_won', 'lightsaber_color', 'side'),
        backgroundColor = 'rgba(28, 28, 28, 0.8)',
        color = '#FFD700'
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
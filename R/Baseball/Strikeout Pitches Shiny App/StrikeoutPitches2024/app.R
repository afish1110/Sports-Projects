#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(readr)
library(ggrepel)

pitcherdata2024 <- read_csv('StatcastPitcherData2024.csv')
masterid <- read_csv('masterid.csv')

##need a filtered data frame that just includes the strikeout pitches
pitcherdata2024 %>%
  filter(events == 'strikeout') -> strikeout_pitches

##adding column that takes does called or swinging
strikeout_pitches %>%
  mutate(swing = case_when(
    description == 'called_strike' ~ FALSE,
    description != 'called_strike' ~ TRUE)) -> strikeout_pitches

##possible pitchers for drop down menu
strikeout_pitches %>%
  group_by(pitcher) %>%
  summarise() %>%
  inner_join(masterid, by = c('pitcher' = 'mlb_id')) %>%
  select(pitcher, mlb_name) %>%
  arrange(mlb_name) -> possiblePitchers

possiblePitchersList <- possiblePitchers$pitcher
names(possiblePitchersList) <- possiblePitchers$mlb_name

##creating strike zone
plate_width <- 17 + (9 / pi)
k_zone_plot <- ggplot(NULL, aes(x = plate_x, plate_z)) +
  geom_rect(xmin = -(plate_width / 2) / 12,
            xmax = (plate_width / 2) / 12,
            ymin = 1.5,
            ymax = 3.6, color = "black", alpha = 0) +
  coord_equal() +
  scale_x_continuous("Horizontal Location (ft.)",
                     limits = c(-2, 2)) +
  scale_y_continuous("Vertical Location (ft.)",
                     limits = c(0, 5))

##defining ui having selections for pitcher and swing
ui <- fluidPage(

    # Application title
    titlePanel('Strikeout Pitches During the 2024 MLB Season'),
    sidebarLayout(
      ##pitcher selection drop down using all pitchers that pitched in data
      sidebarPanel(position = 'left',
                   selectInput(inputId = 'selectPitcher',
                               label = h3("Pitcher"),
                               choices = possiblePitchersList,
                               ##Chris Sale
                               selected = 519242)
      ),
      ##swing check box allows for yes or no
      sidebarPanel(position = 'right',
                   checkboxGroupInput(inputId = 'swings',
                                      label = h3('Swing'),
                                      choices = list('Yes' = TRUE,
                                                     'No' = FALSE),
                                      selected = c(TRUE, FALSE)))
    ),
    mainPanel(
      plotOutput(outputId = 'strikeoutPitchChart')
    )
)


server <- function(input, output) {
  ##filters pitcher selection
  SelectedPitcherReactive <- reactive({
    strikeout_pitches %>% filter(pitcher == input$selectPitcher)
  })
  
  ##filters for swing yes/no selection and creates plot
  
  output$strikeoutPitchChart <- renderPlot({
    SelectedPitcherReactive() %>%
      filter(swing %in% input$swings) -> selectedSwings
    
    ##plots using the strikezone format above, color as the pitch type, and swing as shape creates scatter plot
    k_zone_plot %+% selectedSwings +
      aes(color = pitch_name, shape = swing) +
      geom_point(size = 2.5) + 
      ggtitle('Strikeout Pitches')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(tidyverse)
library(rsconnect)
library(tidyr)

totalStats <- read_delim("Count Us In/Total-Table 1.csv")

demographics <- read_delim("Count Us In/Demographics-Table 1.csv")
newDemographics <- demographics %>% 
  select_if(~ !any(is.na(.)))

yearRange <- newDemographics %>% 
  select(Year) %>% 
  summarise(high = max(Year))


cause <- read_delim("Count Us In/Cause-Table 1.csv")
newCause <- cause %>% 
  select_if(~ !any(is.na(.)))

  
ui <- fluidPage(tabsetPanel(
  tabPanel("About", titlePanel("Homelessness Statistics"),
           p("This app uses data collected on homelessness in ", 
             em("Seattle "), "from the years ", strong("1998 - 2020")),
           br(),
           p("Below is a random sample of the total homeless population data in King County:"),
           tableOutput("random")),
  tabPanel("Plots",
           sidebarLayout(
            sidebarPanel(p("You can select a certain homelessness cause. The number of homeless
                      people made by each cause differs."),
            fluidRow(
            column(6,
               radioButtons("color", "Choose color",
                            choices = c("skyblue", "lawngreen", "orangered",
                                                 "purple", "gold"))
        ),
        column(6,
               uiOutput("checkboxShelter")
        )
      )
    ),
    mainPanel(
      textOutput("years"),
      plotOutput("plot")
    )
  )),
  tabPanel("Table",
           sidebarLayout(
    sidebarPanel(p("The table displays the data on homelessness depending on
                      the demographic selected."),
      fluidRow(
        column(6, uiOutput("checkboxDemo"))
      )
    ),
    mainPanel(textOutput("h"), 
              tableOutput("data_table"))
  ))
  )
)

server <- function(input, output) {
  output$random <- renderTable({
    totalStats %>%  
      sample_n(6)
  })
  output$checkboxShelter <- renderUI({
    radioButtons("userCause", "Choose cause of homelessness",
                       choices = unique(newCause$Cause)
    )
  })
  sample <- reactive({
    newCause %>%
      filter(Cause %in% input$userCause)
  })
  output$plot <- renderPlot({
    p <- sample() %>%
      ggplot(aes(factor(Year), Count, fill = factor(Cause))) +
      geom_col() +
      labs(x = "Year", y = "Count", fill = "Cause") +
      scale_fill_manual(values = input$color)
    if(nrow(sample()) == 0) {
      p <- p + labs(title = "Please select a cause")
    }
    p
  })
  output$checkboxDemo <- renderUI({
    radioButtons("Demo", "Choose a demographic",
                 choices = unique(newDemographics$Demographic))
  })
  
  output$years <- renderText({
    years <- sample() %>% pull(Year) %>% unique()
    if (length(years) == 1) {
      paste("This data is from", years)
    } else {
      paste("The data ranges from", min(years), "to", max(years))
    }
  })
  
  
  table_sample <- reactive({
    s2 <- newDemographics %>% 
      filter(!is.na()) %>% 
      filter(Demographic %in% input$Demo)
    s2
  })
  
  output$data_table <- renderTable({
    newDemographics %>% 
      filter(Demographic == input$Demo)
  })
  
  output$h <- renderText({
    selected_data <- newDemographics %>% 
      filter(Demographic == input$Demo)
    
    n <- sum(!is.na(selected_data$Count))
    output_text <- paste("Selected demographic contains", n, "observations.")
    
  })
}

shinyApp(ui = ui, server = server)

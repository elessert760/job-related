# # ############################
# demo shiny app with reactives
# # ############################

library(shiny)
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(here)

setwd(here("R/apps/benchmarking/"))

Choices <- read_csv("joined.csv")
Choices <- lapply(Choices$`Owner Name` %>% unique, "[")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Benchmarking"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    selectizeInput("choice", "Select a Client", Choices, selected = "")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel(title = "Table",
               DT::dataTableOutput("joined")),
      tabPanel("Barchart",
               plotOutput("barchart", height = "800px")),
      tabPanel("Boxplot",
               plotOutput("boxplot"), height = "100px")
    )
  )))

server <- function(input, output, session) {
  dat <- read_csv("joined.csv")
  joined <- reactive({
    sub_dat <-
      dat[dat$`Industry Group` == dat$`Industry Group`[which(dat$`Owner Name` == input$choice)[1]],]
    
    ind <- sub_dat$`Industry Group` %>% unique()
    
    ind <- ind[1]
    
    sub_dat %<>% group_by(`Owner Name`, completed) %>%
      summarise(`Number Completed` = n()) %>%
      mutate(`Proportion Completed` = round(`Number Completed` / sum(`Number Completed`) *
                                              100, 1)) %>%
      filter(completed == T) %>%
      # ungroup() %>%
      # group_by(`Owner Name`) %>%
      # summarise(`Total Suppliers` = n()) %>%
      select(-completed) %>%
      mutate(`Industry Group` = ind)
  })
  
  prop_summary <- reactive({
    dat[dat$`Industry Group` == dat$`Industry Group`[which(dat$`Owner Name` == input$choice)[1]],] %>% 
      group_by(`Industry Group`, `Owner Name`, completed) %>%
      summarise(Count = n()) %>%
      ungroup %>%
      group_by(`Owner Name`) %>%
      mutate(Total = sum(Count)) %>%
      ungroup %>%
      filter(completed == T) %>%
      mutate(Prop = round(Count / Total, 2) * 100) %>%
      ungroup() %>%
      arrange(desc(Prop))
  })
  
  mycolors <- reactive({
    mycolors <- NA
    mycolors[which(prop_summary()$`Owner Name` == input$choice)] <-
      "#E62525"
    mycolors[which(prop_summary()$`Owner Name` != input$choice)] <-
      "steelblue"
    return(mycolors)
  })
  
  mylabel <- reactive({
    mylabel <- NA
    mylabel[which(prop_summary()$`Owner Name` == input$choice)] <-
      "black"
    mylabel[which(prop_summary()$`Owner Name` != input$choice)] <-
      "transparent"
    # mylabel <- as.factor(mylabel)
  })
  
  barchart <- reactive({
    # plotly::ggplotly(
    qplot(
      y = Prop,
      x = reorder(`Owner Name`,-Prop),
      data = prop_summary(),
      label = Prop,
      geom = "blank"
    ) +
      geom_bar(stat = "identity", fill = mycolors()) +
      coord_flip() +
      geom_hline(aes(yintercept = mean(Prop))) +
      xlab("") +
      ylab("Completion Proportion") +
      theme_light() +
      theme(axis.text.y = element_text(color = mylabel())) +
      # scale_y_discrete(labels = mylabel()) +
      ylab(NULL) +
      geom_label()
    # )
    
  })
  
  boxplot <- reactive({
    # plotly::ggplotly(
    qplot(
      y = Prop,
      x = 1,
      data = prop_summary(),
      geom = "blank"
    ) + geom_boxplot(fill = "grey80",
                     colour = "#3366FF") + geom_jitter(
                       aes(alpha = 0.7,
                           size = 3),
                       colour = mycolors(),
                       width = 0.25,
                       size = 4
                     ) +
      theme(
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        axis.title = element_text(size = 10),
        legend.position = "none"
      ) +
      xlab("") + coord_flip() + ylab("Completion Proportion")
    # )
  })
  
  output$barchart <- renderPlot(barchart())
  output$joined <- DT::renderDataTable(expr = datatable(joined()))
  output$boxplot <- renderPlot(boxplot())
}


shinyApp(
  ui = ui,
  server = server,
  options = list(host = "127.0.0.1", port = 3798)
)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(RTTanalyse)

# raw_dataset = read_csv('../../data/RTT_dataV2.csv') %>%
#   setNames(nm = sapply(X = names(.), FUN = function(xx) gsub(' ','_',xx)))
#
# dataset =  raw_dataset %>%
#   mutate(unique_sp_id = mapply(paste, Order, Notes_b, Family, Genus, Species) %>% gsub('NA', '', .)) %>%
#   gather(Time, n_alive, `0h`:R96h) %>%
#   mutate(Time = sapply(FUN = function(x) {gsub('h','',x) %>% gsub('R','',.)} , Time)) %>%
#   mutate(Time = as.numeric(Time)) %>%
#   mutate(Concentration = sapply(FUN = function(x) {gsub('ug/L','',x) %>% gsub('R','',.)} , Concentration)) %>%
#   mutate(Concentration = sapply(FUN = function(x) {gsub('Control','0',x) %>% gsub('R','',.)} , Concentration)) %>%
#   mutate(Concentration = as.numeric(Concentration)) %>%
#   RTTanalyse:::rename_dataset(concentration_col_name = 'Concentration',
#                                                time_col_name = 'Time',
#                                                n_alive_col_name = 'n_alive',
#                                                species_identifier_col_name = 'Identifier',
#                                                test_id_col_name = 'Test_number')
#
# pesticide_list = dataset %>% .$Pesticide %>% unique()
# species_list = dataset %>% .$species %>% unique()

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel(h2("Load dataset and check consistency")),

   ############################ Load and get a first glimpse of the dataset

   fluidRow(
     column(4,
            fileInput("file1", "Choose CSV File containing the dataset",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
            )
     ),
     column(4,
     textOutput("filepath")
     ),
     column(4,
            checkboxInput("header", "Header", TRUE)
     )
   ),

   dataTableOutput("contents"),


   ##################### Select the column

   fluidRow(
     column(3,
            uiOutput('selectcol_conc')
     ),
     column(3,
            uiOutput('selectcol_species')
     ),
     column(3,
            uiOutput('selectcols_times')
     ),
     column(3,
            uiOutput('selectcols_disambiguation')
     )
   )#,
   # ##################### Plot some of the data
   #
   #
   # selectInput("selected_pesticide", label = h3("Select contaminant"),
   #             choices = pesticide_list, selected = pesticide_list[1]),
   #
   # plotOutput("RawDataPlot"),
   #
   # hr(),
   #
   # fluidRow(
   #   column(4,
   #          selectInput("selected_pesticide2", label = h3("Select contaminant"),
   #                                        choices = pesticide_list, selected = pesticide_list[1])
   #   ),
   #   column(4,
   #          selectInput("selected_species", label = h3("Select species identifier"),
   #                                                      choices = species_list, selected = species_list[1])
   #   )
   # ),
# )

   # sidebarLayout(
   #    sidebarPanel(
   #       selectInput("selected_pesticide", label = h3("Select contaminant"),
   #                   choices = pesticide_list, selected = pesticide_list[1]),
   #       selectInput("selected_species", label = h3("Select species identifier"),
   #                                 choices = species_list, selected = species_list[1])
   #    ),
   #
   #    # Show a plot of the generated distribution
   #    mainPanel(
   #       plotOutput("RawDataPlot")#,
   #      # dataTableOutput("RawDataTable")
   #    )#,
   #
   # ),

   # dataTableOutput("RawDataTable")

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  column_names <- reactive({

    if (is.null(input$file1)){
      return(NULL)
    }
    else{
      read_csv(input$file1$datapath, col_names = input$header) %>%
        names
    }
  })


  output$selectcol_conc = renderUI({
    selectInput(inputId = 'selectedcol_conc',
                label = h3("Select the column corresponding to the concentrations"),
                choices = column_names())
  })

  output$selectcol_species = renderUI({
    selectInput(inputId = 'selectedcol_species',
                label = h3("Select species identifier"),
                choices = column_names() %>% setdiff(input$selectedcol_conc))
  })

  output$selectcols_times = renderUI({
    checkboxGroupInput(inputId = 'selectedcols_times',
                label = h3("Select survival columns for times"),
                choices = column_names() %>% setdiff(c(input$selectedcol_conc, input$selectedcol_species)))
  })

  output$selectcols_disambiguation = renderUI({
    selectInput(inputId = 'selectedcols_disambiguation',
                       label = h3("Select a column to identify replicates (optional)"),
                       choices = column_names() %>% setdiff(c(input$selectedcol_conc, input$selectedcol_species, input$selectedcols_times)))
  })


   output$contents <- renderDataTable({
     # input$file1 will be NULL initially. After the user selects
     # and uploads a file, it will be a data frame with 'name',
     # 'size', 'type', and 'datapath' columns. The 'datapath'
     # column will contain the local filenames where the data can
     # be found.
     inFile <- input$file1

     if (is.null(inFile)){
       return(NULL)
       }
     else{
       # read_csv('../../data/RTT_dataV2.csv')
       read_csv(inFile$datapath, col_names = input$header)
     }
     # read_csv(inFile$datapath, header = input$header)

   }, options = list(pageLength = 5))

   observe({

     inFile <- input$file1


     if (is.null(inFile)){
       return(NULL)
     }
     else{
       dataset_ = read_csv(inFile$datapath, col_names = input$header)

       # output$RawDataPlot <- renderPlot({
       #   p = dataset %>%
       #     subset(Pesticide == input$selected_pesticide) %>%
       #     plot_raw_data()
       #   p +
       #     theme(text = element_text(size=15))
       # })
       #
       # output$RawDataTable <- renderDataTable({
       #   raw_dataset %>%
       #     subset(Pesticide == input$selected_pesticide2 & Identifier == input$selected_species)
       # })

       output$filepath = renderPrint({
         inFile <- input$file1

         if (is.null(inFile))
           return(NULL)
         else
           return(inFile$datapath)
       })

     }

   })
}

# Run the application
shinyApp(ui = ui, server = server)


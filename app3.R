#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("shinyWidgets")
library(stringr)


# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Uploading Files"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            actionButton(inputId = "sauve", label = "sauvegarde")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Data file ----
            # tableOutput("contents"),
            verbatimTextOutput("test"),
            sliderInput("slider1", label = h3("Slider"), min = 0, 
                        max = 5, value = 1),
            textInput("text1", label = h3("mot a enlever 1"), value = "NULL"),
            textInput("text2", label = h3("mot a enlever 2"), value = "NULL"),
            textInput("text3", label = h3("mot a enlever 3"), value = "NULL"),
            actionButton(inputId = "sort", label = "tri")
        )
        
    )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
    # df <- 
    #     output$contents <- renderTable({
    #         
    #         # input$file1 will be NULL initially. After the user selects
    #         # and uploads a file, head of that data file by default,
    #         # or all rows if selected, will be shown.
    #         
    #         req(input$file1)
    #         
    #         # when reading semicolon separated files,
    #         # having a comma separator causes `read.csv` to error
    #         tryCatch(
    #             {
    #                 df <- read.csv(input$file1$datapath,
    #                                header = input$header,
    #                                sep = input$sep,
    #                                quote = input$quote)
    #             },
    #             error = function(e) {
    #                 # return a safeError if a parsing error occurs
    #                 stop(safeError(e))
    #             }
    #         )
    #         
    #         if(input$disp == "head") {
    #             return(head(df))
    #         }
    #         else {
    #             return(df)
    #         }
    #         
    #     })
    # 
    data = reactiveValues()
    
    observeEvent(input$sauve, {
        data$table = read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
        sendSweetAlert(
            session = session,
            title = "Done !",
            text = "Le fichier a bien ete enregistre !",
            type = "success"
        )  
        
    })
    output$test <- renderPrint({"attente tri"})
    
    observeEvent(input$sort, {
        if (input$slider1>0) {
            memory <- c(input$text1, input$text2, input$text3)
            for (i in 1:input$slider1) {
                data$table[,i+1] <- str_detect(data$table[,1] ,memory[i])
            }
        }
        
        
    })
    
    observeEvent(input$sort, {
        output$test <-renderPrint({
            data$table[1:5,1:(input$slider1+1)]})
    })
    
    # data_total$mastectomy <- str_detect(data_total$abstract,"mastectom")
    # data_total$chemotherap <- str_detect(data_total$abstract,"chemotherap")
    # data_total$surgery <- str_detect(data_total$abstract,"surger")
    # data_total$mortality <- str_detect(data_total$abstract,"mortalit")
    # data_total$surgical <- str_detect(data_total$abstract,"surgic")
    # data_total$lesion <- str_detect(data_total$abstract,"lesion")
    # output$test <-renderPrint({
    #     
    #     data$table[1:5,1:input$slider]})
    
}
# Create Shiny app ----
shinyApp(ui, server)
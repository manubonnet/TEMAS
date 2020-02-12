#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(backports)
if (!require("devtools"))
  install.packages("devtools")
if (!require("processx"))
    install.packages("processx")
#install.packages("nlme")
library(nlme)
if (!require("scales"))
    install.packages("scales")
library("devtools")
library(shiny)
#devtools::install_github("gschofl/reutils")
set_entrez_key("2f426efbccf334610530e682833b93e33508")
options(reutils.api.key = "2f426efbccf334610530e682833b93e33508")
options(reutils.email = "emmanuelbeaunez@gmail.com")
#rsconnect::appDependencies()

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Global Search"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("text", label = h3("Recherche"), value = "Enter text..."),
            dateInput("datedebut", label = h3("Date debut"), value = "2006-01-01"),
            dateInput("datefin", label = h3("Date fin"), value = "2017-12-31"),
            numericInput("num_max", label = h3("Num max"), value = 2),
            actionButton(inputId = "go", label = "Update"),
            
            verbatimTextOutput("value")
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            h2("Cliquer pour creer la base"),
            actionButton(inputId = "base", label = "creation base"),
            h3("Base initiale"),
            verbatimTextOutput("base"),
            # downloadButton("downloadData", "Download"),
            h3("Base mise en forme"),
            verbatimTextOutput("test"),
            downloadButton("downloadData", "Download"),
            uiOutput("tab")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    library(rentrez)
    library("XML")
    library(reutils)
    set_entrez_key("2f426efbccf334610530e682833b93e33508")
    library(lubridate)
    library(stringr)

    rv <- reactiveValues(data = NULL)
    rv <- reactiveValues(datedebut =  NULL)
    rv <- reactiveValues(datefin =  NULL)
    rv <- reactiveValues(n_max =NULL)
    
    observeEvent(input$go, {rv$data <- input$text })
    observeEvent(input$go, {rv$datedebut <-  str_c(str_sub(input$datedebut,1,4),"/",str_sub(input$datedebut,6,7),"/",str_sub(input$datedebut,9,10))})
    observeEvent(input$go, {rv$datefin <- str_c(str_sub(input$datefin,1,4),"/",str_sub(input$datefin,6,7),"/",str_sub(input$datefin,9,10))})
    observeEvent(input$go, {rv$n_max <- input$num_max })
    
    
    pubmed <- reactiveValues(ids="Attente confirmation")
    output$value <-renderPrint({
        pubmed$ids
    })
    a <- "Resultat"
    observeEvent(input$go,{pubmed$ids <- entrez_search(db = "pubmed", term = rv$data ,mindate=rv$datedebut,maxdate=rv$datefin ,retmax=rv$n_max, api_key = "2f426efbccf334610530e682833b93e33508")$ids 
    #entrez_search(db = "pubmed", term = rv$data ,mindate=rv$datedebut,maxdate=rv$datefin ,retmax="15")}
    output$value <-renderPrint({
        pubmed$ids[1:10]
    })
    })
    
    date_jour <- str_sub(date(),start = 9,end = 10)
    date_mois <- str_sub(date(),start = 5,end = 7)
    date_annee <- str_sub(date(),start = 21,end = 24)
    date_heure <- str_c(str_sub(date(),start = 12,end = 13),"h", str_sub(date(),start = 15,end = 16))
    
    name_id <- str_c("shiny.step1_",date_jour,"_",date_mois, "_" , date_annee,"_" ,date_heure,".csv")
    
    rv <- reactiveValues(title_abstract = "Etape 2 : creation de la base de donnees")
    output$base <-renderPrint({
        rv$title_abstract})
    output$test <-renderPrint({
        
        "Etape 3 : passage en minuscule + supression caracteres speciaux"})
    num <- 1
    observeEvent(input$base, {rv$l <- length(pubmed$ids) })
    observeEvent(input$base, {rv$title_abstract <- NULL })
    
    observeEvent(input$base, {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Creating database", value = 0)
        for (i in 1:rv$l) {
            progress$inc(1/rv$l, detail = paste("Doing part", i))
            article <- efetch(pubmed$ids[i],"pubmed")
          time.sleep(0.34)
            rv$title_abstract <- c(rv$title_abstract,paste(article$xmlValue("//Title"),article$xmlValue("//Abstract")))
            print(i)
        }
        num <- 10
        
        
        
        output$base <-renderPrint({
            
            
            head(rv$title_abstract)})
        
        
        datas2 <- reactive({rv$title_abstract})
        
        
        clean_data <- reactive({str_replace_all(datas2(),"[[:punct:]]"," ")})
        clean_data3 <- reactive({str_replace_all(clean_data(),"[[:cntrl:]]"," ")})
        clean_data4 <- reactive({str_to_lower(clean_data3())})
        clean_data2 <- reactive({cbind(pubmed$ids,clean_data4())})
        
        output$test <-renderPrint({
            
            head(clean_data2())})
  
    
    output$downloadData <- downloadHandler(
        filename = function() {
            name_id
        },
        content = function(file) {
            write.csv(clean_data2(), file, row.names = FALSE)
        }
    )
    
    
      }
    )
    url <- a("step2", href=" https://step3.temas-bonnet.site/TEMAS2") 
    output$tab <- renderUI({ 
        tagList("Link to step2:", url) 
    }) 
}
# Run the application 
shinyApp(ui = ui, server = server)


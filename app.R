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
library(rentrez)
library("XML")
#install.packages("reutils",check_built=T)
# library(reutils)

library(lubridate)
library(stringr)
library("shinyWidgets")
#devtools::install_github("gschofl/reutils")
set_entrez_key("2f426efbccf334610530e682833b93e33508")
Sys.getenv("ENTREZ_KEY")
options("scipen"=100)
options(reutils.api.key = "34ad5abbcddf5a94d9dbfb34ad005be64d0a")
options(reutils.email = "emmanuelbeaunez@gmail.com")
#rsconnect::appDependencies()

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}")),),
  titlePanel("Global Search"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Step 1 : Define search parameters"),
      textInput("text", label = h4("Pubmed Search"), value = "breast_cancer[MeSH Terms] AND risk_factor[MeSH Terms]"),
      # dateInput("datedebut", label = h5("From"), value = "2006-01-01"),
      # dateInput("datefin", label = h5("to"), value = "2017-12-31"),
      dateRangeInput("dates", label = h4("Publication dates"),
                     start = Sys.Date() - 3652, end = Sys.Date(),
                     max = Sys.Date(), weekstart = 1),
      actionButton(inputId = "go", label = "Save search parameters"),
      
      # verbatimTextOutput("value")
      conditionalPanel("input.go",
                       h4("Query Translation:"),
                       textOutput("QueryTranslation"),
                       h4("Number of PubMed answers:"),
                       textOutput("count"),
                       
                       hr(),
                       numericInput("num_max", label = h4("Maximum number of articles to retrieve \n (approx 15s/1000 articles)"), value = 500,min=100,max=50000,step = 100),
                       h3("Step 2 : Create database"),
                       actionButton(inputId = "base", label = "Create database"),
      ),
      conditionalPanel("input.base",
                       hr(),
                       h3("Step 3 : Download database"),
                       downloadButton("downloadData", "Download database"),
                       uiOutput("tab")
      )
    ),
    
    
    
    mainPanel(
      
      # h3("Base initiale"),
      # verbatimTextOutput("base"),
      # downloadButton("downloadData", "Download"),
      conditionalPanel("input.base",
                       h3("Database Preview"),
                       tableOutput("test")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  rv <- reactiveValues(data = NULL)
  rv <- reactiveValues(datedebut =  NULL)
  rv <- reactiveValues(datefin =  NULL)
  rv <- reactiveValues(n_max =NULL)
  rv <- reactiveValues(datedebut1 =NULL)
  rv <- reactiveValues(datefin1 =NULL)
  
  
  observeEvent(input$go, {
    rv$datedebut1 <- input$dates[1]
    rv$datefin1 <- input$dates[2]
    rv$data <- input$text
    rv$datedebut <-  str_c(str_sub(rv$datedebut1 ,1,4),"/",str_sub(rv$datedebut1 ,6,7),"/",str_sub(rv$datedebut1 ,9,10))
    rv$datefin <- str_c(str_sub(rv$datefin1 ,1,4),"/",str_sub(rv$datefin1 ,6,7),"/",str_sub(rv$datefin1 ,9,10))
    
  })
  
  
  
  pubmed <- reactiveValues(ids="Attente confirmation")
  # output$value <-renderPrint({
  #   pubmed$ids
  # })
  a <- "Resultat"
  observeEvent(input$go,{
    sendSweetAlert(
      session = session,
      btn_labels = NA,
      title = "Saving parameters...",
      text = "Please wait until \"Done !\" appears on your screen.",
      closeOnClickOutside = F,
      type = "warning"
    )
    pubmed$total <- entrez_search(db = "pubmed", term = rv$data ,mindate=rv$datedebut,maxdate=rv$datefin,use_history = T) 
    pubmed$ids <- pubmed$total$ids
    print(length(pubmed$ids))
    pubmed$history <- pubmed$total$web_history
    output$QueryTranslation <- renderText(pubmed$total$QueryTranslation)
    output$count <- renderText(pubmed$total$count)
    updateNumericInput(session, "num_max",
                       value = min(pubmed$total$count,10000), min = 100, max = pubmed$total$count)
    
    #entrez_search(db = "pubmed", term = rv$data ,mindate=rv$datedebut,maxdate=rv$datefin ,retmax="15")}
    # output$value <-renderPrint({
    #   pubmed$ids[1:10]
    # })
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "Search parameters saved !",
      type = "success"
    )  
  })
  
  date_jour <- str_sub(date(),start = 9,end = 10)
  date_mois <- str_sub(date(),start = 5,end = 7)
  date_annee <- str_sub(date(),start = 21,end = 24)
  date_heure <- str_c(str_sub(date(),start = 12,end = 13),"h", str_sub(date(),start = 15,end = 16))
  
  name_id <- str_c("shiny.step1_",date_jour,"_",date_mois, "_" , date_annee,"_" ,date_heure,".csv")
  
  rv <- reactiveValues(title_abstract = "Etape 2 : creation de la base de donnees")
  output$base <-renderPrint({
    rv$title_abstract})
  # output$test <-renderPrint({
  #   
  #   "Waiting..."})
  num <- 1
  observeEvent(input$base, {rv$l <- length(pubmed$ids) })
  observeEvent(input$base, {rv$title_abstract <- NULL })
  
  observeEvent(input$base, {
    rv$n_max <- min(input$num_max,pubmed$total$count)
    sendSweetAlert(
      session = session,
      btn_labels = NA,
      title = "Downloading articles...",
      text = "Please wait until \"Done !\" appears on your screen.",
      closeOnClickOutside = F,
      type = "warning"
    )
    pubmed$ids2 <- NULL
    # article <- efetch(pubmed$ids[i],"pubmed")
    # rv$title_abstract <- c(rv$title_abstract,paste(article$xmlValue("//Title"),article$xmlValue("//Abstract")))
    # print(i)
    niter <- floor(rv$n_max/10000)
    for (i in 0:(niter-1)) {
      article <- entrez_fetch(db="pubmed",web_history = pubmed$total$web_history ,rettype ="xml",parsed = T,retmax = 10000, retstart = i*10000)
      b <- getNodeSet(article,"//MedlineCitation")
      print(length(b))
      for (i in 1:length(b)) {
        bbb <- xmlSerializeHook(b[[i]])
        bbb <- xmlDeserializeHook(bbb)
        ttt <- XML::xpathSApply(bbb, "//Abstract", XML::xmlValue)
        aaa <- XML::xpathSApply(bbb, "///MedlineCitation/PMID[@Version='1']", XML::xmlValue)
        if(length(ttt)==0) ttt <- "NA"
        if(length(aaa)==0) aaa <- "NA"
        ttt <- paste((XML::xpathSApply(bbb, "//ArticleTitle", XML::xmlValue)),ttt)
        rv$title_abstract <-c(rv$title_abstract,ttt)
        pubmed$ids2 <-c(pubmed$ids2,aaa)
        print(i)
      }
    }
    article <- entrez_fetch(db="pubmed",web_history = pubmed$total$web_history ,rettype ="xml",parsed = T,retmax = (rv$n_max-10000*niter), retstart = (niter*10000))
    b <- getNodeSet(article,"//MedlineCitation")
    print(length(b))
    for (i in 1:length(b)) {
      bbb <- xmlSerializeHook(b[[i]])
      bbb <- xmlDeserializeHook(bbb)
      ttt <- XML::xpathSApply(bbb, "//Abstract", XML::xmlValue)
      aaa <- XML::xpathSApply(bbb, "///MedlineCitation/PMID[@Version='1']", XML::xmlValue)
      if(length(ttt)==0) ttt <- "NA"
      if(length(aaa)==0) aaa <- "NA"
      ttt <- paste((XML::xpathSApply(bbb, "//ArticleTitle", XML::xmlValue)),ttt)
      rv$title_abstract <-c(rv$title_abstract,ttt)
      pubmed$ids2 <-c(pubmed$ids2,aaa)
      print(i)
    }
    zzz <- 0
    while(length(pubmed$ids2)<rv$n_max & (rv$n_max + zzz < pubmed$total$count)){
      article <- entrez_fetch(db="pubmed",web_history = pubmed$total$web_history ,rettype ="xml",parsed = T,retmax = (rv$n_max-length(pubmed$ids2)),retstart= rv$n_max+zzz )
      zzz <- zzz + (rv$n_max-length(pubmed$ids2))
      b <- getNodeSet(article,"//MedlineCitation")
      print(length(b))
      for (i in 1:length(b)) {
        bbb <- xmlSerializeHook(b[[i]])
        bbb <- xmlDeserializeHook(bbb)
        ttt <- XML::xpathSApply(bbb, "//Abstract", XML::xmlValue)
        aaa <- XML::xpathSApply(bbb, "///MedlineCitation/PMID[@Version='1']", XML::xmlValue)
        if(length(ttt)==0) ttt <- "NA"
        if(length(aaa)==0) aaa <- "NA"
        ttt <- paste((XML::xpathSApply(bbb, "//ArticleTitle", XML::xmlValue)),ttt)
        rv$title_abstract <-c(rv$title_abstract,ttt)
        pubmed$ids2 <-c(pubmed$ids2,aaa)
        print(i)
      }
    }
    num <- 10
    print(length(pubmed$ids2))
    
    
    output$base <-renderPrint({
      
      
      head(rv$title_abstract)})
    
    
    datas2 <- reactive({rv$title_abstract})
    
    
    clean_data <- reactive({str_replace_all(datas2(),"[[:punct:]]"," ")})
    clean_data3 <- reactive({str_replace_all(clean_data(),"[[:cntrl:]]"," ")})
    clean_data4 <- reactive({str_to_lower(clean_data3())})
    clean_data2 <- reactive({cbind(pubmed$ids2,clean_data4())})
    final_data <- clean_data2()
    colnames(final_data)<- c("uid","Title_and_abstract")
    
    output$test <-renderTable({
      
      head(final_data,n=10L,addrownums=F)})
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "Search complete !",
      type = "success"
    )  
    
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


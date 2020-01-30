#module

library(googlesheets4)
library(shiny)
library(phosphorr)
library(shinyFiles)
library(readxl)
library(xfun)
#Modal module UI
Data_Importer_UI <- function(id) {
    ns <- NS(id)
    actionButton(ns("openModalBtn"), "Open Modal")

}

#Modal Module server
Data_importer_server <- function(input, output, session) {
    myModal <- function() {
        ns <- session$ns #sessions$ns explicitly loads the namespace in the modal
        #all componenets of the Modal need to be here
        modalDialog(
            h3("Data Importer"),
            actionButton(ns('add'), 'Data type', icon = icon('plus')),
            phosphorrOutput(ns('pjs'), height = '80vh'),
            actionButton(ns("closeModalBtn"), "Import Data and Close Window")
            )
    }
    output$pjs <- renderPhosphorr(
        phosphorr()
    )
    #opens the modal if button is pushed
    observeEvent(input$openModalBtn,
                 ignoreNULL = TRUE,
                 showModal(myModal())

    )

    observeEvent(input$add, {
        phosphorrProxy(session$ns('pjs')) %>%
            addWidget("widget_datatype",
                      title = "Data Type",
                      ui = fluidPage(
                          h4("Define Data"),
                          radioButtons(
                              session$ns("type"),
                              label = "File Type",
                              choices = c("File", "URL"),
                              #,"Database"),
                              selected = character(0)
                          ),
                          div(id = "placeholder")

                      ))

    })

    observeEvent(input$type, {
        if (input$type == "File") {
            phosphorrProxy(session$ns('pjs')) %>% addWidget(
                "shiny_window",
                title = "Files",
                insertmode = "split-right",
                ui = fluidPage(
                    shinyFilesButton(
                        session$ns('files'),
                        label = 'File select',
                        title = 'Please select a file',
                        multiple = FALSE
                    )
                )
            )
            insertUI(
                selector = "#placeholder",
                where = "beforeEnd",
                ui = tags$div(
                    div(style="display: inline-block;vertical-align:top; width: 100px;",numericInput(session$ns("header"), label ='Header rows', value = 0, step = 1, min = 0)),
                    div(style="display: inline-block;vertical-align:top; width: 100px;",numericInput(session$ns("nrows"), label ='# of Rows', value = 0, step = 1, min = 0))

                )
            )
            shinyFileChoose(input, 'files', root = c(home = '~'))



        } else if (input$type == "URL") {
            #INCLUDE A SELECTOR IN UI... ETC
            insertUI(
                selector = "#placeholder",
                #selector says where to insert
                where = "beforeEnd",
                ui = tags$div(
                    id = session$ns("MASTER_URL_INPUT"),
                    textInput(
                        inputId = session$ns("my_URL"),
                        label = "Insert URL",
                        value = ""
                    ),
                    verbatimTextOutput(session$ns("my_link")),
                    div(style="display: inline-block;vertical-align:top; width: 100px;",numericInput(session$ns("header"), label ='Header rows', value = 0, step = 1, min = 0)),
                    div(style="display: inline-block;vertical-align:top; width: 100px;",numericInput(session$ns("nrows"), label ='# of Rows', value = 0, step = 1, min = 0))
                    
                    
                    #consider including a header option here? as well as a tail?
                    #need to be able to specify parameters for the table to be input correctly
                )
            )
          }
        })

    observeEvent({
        input$my_URL
        input$files
        1
    },
    {
        if (isTruthy(input$my_URL)) {
            #browser()
            phosphorrProxy(session$ns('pjs')) %>% addWidget(
                session$ns("URL_Stuff"),
                title = "URL...",
                insertmode = "split-bottom",
                refwidgetID = "widget_datatype", #split the right one
                relsize = 0.25,
                ui = fluidPage(tableOutput(session$ns("sheet_data")))
            )
        }

        output$filepaths <-
            renderPrint({
                parseFilePaths(roots = c(home = '~'), input$files)
            })

        if (!is.null(inFile()) && (length(inFile()$datapath) > 0)) {
            #if ((input$inFile() == "") && (length(input$inFile()) > 0)) {
            phosphorrProxy(session$ns('pjs')) %>% addWidget(
                session$ns("file_Stuff"),
                title = "file...",
                insertmode = "split-bottom",
                refwidgetID = "main_window",
                #split the right one
                ui = fluidPage(tags$div(
                    tableOutput(session$ns("file_data"))

                )
                )

            )

        }

    })

    inFile <- reactive({
        parseFilePaths(roots = c(home = '~'), input$files)
    })

    shiny_data <- reactive({
        if (tolower(tools::file_ext(inFile()$datapath)) == "xlsx") {
            read_excel(inFile()$datapath, sheet = 1, skip = input$header, n_max = input$nrows)

        } else if (tolower(tools::file_ext(inFile()$datapath)) == "csv") {
            read.csv(inFile()$datapath, header=TRUE, skip = input$header, nrows = input$nrows) #can specify skip = and nrows =

        } else if (tolower(tools::file_ext(inFile()$datapath)) == "txt" | tolower(tools::file_ext(inFile()$datapath)) == "tsv" ) {
            read.table(inFile()$datapath, header = TRUE, nrows = input$nrows) #weird, when you put 0 it reads in all rows, any other number works -- maybe 0 is invalid input 

        }

    })

    output$file_data <- renderTable(
        shiny_data()
    )

    output$my_link <- renderText({input$my_URL})

    output$sheet_data <- renderTable({
        read_sheet(input$my_URL, skip = input$header, n_max = input$nrows)
    })

    #this closes the modal based on the button in modal
        #seems like we can have server commands run on modal UI
    
    observeEvent(input$closeModalBtn, {
        removeModal()
        return(shiny_data)
        
    })

}



ui <- fluidPage(
    Data_Importer_UI("foo"),
    tableOutput("testing")
)

server <- function(input, output, session) {
    callModule(Data_importer_server, "foo")
    if (exists('shiny_data')){
    output$testing <- renderTable(shiny_data())
    }
}


shinyApp(ui, server)

#' readGUI
#' 
#' GUI for reading data into R
#' 
#' @param ... Params to pass to \code{runApp()}
#' @param verbose Verbose output mode
#' @import shiny
#' @import xtable
#' @export
#' 

readGUI <- function(...,verbose=FALSE) {
   runApp(
      list(
         
         ## UI -----------------------------------------------------------------
         ui = bootstrapPage(
            ## HEAD
#             includeCSS(system.file("css/bootstrap.css",package="datareadR")),
#             includeCSS(system.file("css/styles.css",package="datareadR")),
         
            includeCSS("inst/www/bootstrap.css"),
            includeCSS("inst/www/styles.css"),
            
            ## Empty first row
            div(class='row',tags$p(tags$br(),tags$br())),
            div(
               class = "row",
               div(
                  class="span4", tags$p(tags$br())
               ),
               div(
                  class="span8", h1("datareadR - en applikation för dataimport i R")
               )
            ),
            
            tags$hr(),
            
            ## Content row
            div(
               class="row",
               div(
                  class="span4", tags$p(tags$br())
               ),
               div(
                  class="span4",
                  h4("Välj data"),
                  fileInput("fil1","Fil att importera:", accept=c("text/csv",'text/comma-separated-values,text/plain')),
                  tags$p(tags$br()),
                  actionButton2("go","Importera data","btn action-button btn-primary")
               ),
               div(
                  class="span4",
                  h4("Variabelinnehåll i fil"),
                  tableOutput("contents")
               )
            ),
            tags$hr(),
            div(
               class="row",
               div(
                  class="span4", tags$p(tags$br())
               ),
               div(
                  class="span4 options-container",
                  h4("Kör iPlot"),
                  uiOutput("slice"),
                  tags$p(tags$br()),
                  actionButton2("run_iPlot","Kör iPlot","btn action-button btn-danger")
               ),
               div(
                  class="span4 options-container",
                  h4("Exportera data till RAM"),
                  textInput("dataname", "Namnge dataset",value="min_data"),
                  tags$p(tags$br()),
                  actionButton2("quit","Avsluta applikationen","btn action-button btn-info")
               )
            )
         ),
         
         ## SERVER -------------------------------------------------------------
         server = function(input,output,session) {
            options(shiny.maxRequestSize=100*1024^2)
            
            ## Read data from file
            observe({
               if(input$go == 0) return()
               input$go
               
               path <- isolate(input$fil1$datapath)
               path <- str_replace_all(path,"/","\\\\")
               
               localData <<- iPlot:::iData(readData(isolate(path),filetype="csv"))
               
            })
            
            ## Quit button
            observe({
               if(input$quit == 0) return()
               input$quit
               
               assign(isolate(input$dataname),localData$data,envir=globalenv())
#                stopApp(returnValue = isolate(get(input$dataname)))
               stopApp(returnValue = FALSE)
            })
            
            ## "Launch iPlot" button
            observe({
               if(input$run_iPlot == 0) return()
               input$run_iPlot
               
               for(i in c("numerics","categories")) {
                  localData[[i]] <- input[[i]]
               }
               
               stopApp(returnValue = localData)
            })            

            ## File contents table
            output$contents <- renderTable({
               if(input$go == 0) return()
               
               dataNames <- data.frame(names = names(localData$data), class = as.character(sapply(localData$data, class)))
               return(xtable(dataNames))
               
            })
            
            ## Slicing menus
            output$slice <- renderUI({
               if(input$go == 0) return()
               
               liveSearchLimit <- 5
               
               liveSearch <- if (length(localData$categories) >= liveSearchLimit) T else F
               
               tagList(
                  bootstrapSelectInput(
                     "numerics","Kontinuerliga",
                     choices=localData[["numerics"]],
                     selected=min(localData[["numerics"]]),
                     multiple=TRUE,
                     liveSearch = liveSearch
#                      subtext = rep("categorical", length(localData$categories))
                  ),
                  tags$br(),
                  bootstrapSelectInput(
                     "categories","Kategori",
                     choices=localData[["categories"]],
                     selected=min(localData[["categories"]]),
                     multiple=TRUE,
                     liveSearch = liveSearch
#                      subtext = rep("categorical", length(localData$categories))
                  )
               )
            })
         }
      ),...
   )
}
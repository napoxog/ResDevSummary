#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(DT)
# INIT: lists ####
parNames = list("Накопленная добыча газа",
                    "Накопленная добыча воды",
                    "Пластовое давление",
                    "Устьевое давление")
names(parNames) = c ( "WGPT","WWPT","WBP9","WTHP" )

dayNames = c(1:31)
monNames = c(1:12)
names(monNames) = date_names_lang("ru")$mon_ab

input_wid = 500
spacer_wid = 30

myReactives = reactiveValues(wells = NULL, platforms = NULL, data = NULL, pars = NULL)

# Define UI ####
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tabsetPanel(id = 'input', 
                    tabPanel(title = "Данные", id = 'data',
                             #actionButton()
                             fileInput(
                               "openDataFile",
                               "Загрузить накопленную добычу:",
                               accept = '.txt',
                               buttonLabel = "Открыть..."
                             ),
                             "Начальная дата:",
                             #div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
                             #div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(input_wid/4),"px;"),selectInput(inputId = 'startDay',label = 'день', choices = dayNames)),
                             selectInput(inputId = 'startDay',label = 'день', choices = dayNames),
                             #div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(input_wid/4),"px;"),selectInput(inputId = 'startMon',label = 'месяц', choices = monNames))
                             selectInput(inputId = 'startMon',label = 'месяц', choices = monNames),
#                             dateInput(inputId = 'startDate',label = 'Начальная дата',
#                                        value = '2000-01-01',format = 'dd M'),
                             #dateRangeInput(inputId = 'startDate',,label = 'Начальная дата',
                             #              format = 'd-MM', start='1-января',end='31-декабря'),
                             #dataTableOutput('wells'),#,title = "Скважины:"),
                             dataTableOutput('pars')#, title = "Параметры:")
                    ),
                    tabPanel(title = "Скважины", id = 'platform',
                             #div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(input_wid/5),"px;"),numericInput(inputId = 'platformIdx',label = 'Номер куста:',1,min = 1,max = 10,step = 1)),
                             numericInput(inputId = 'platformIdx',label = 'Номер куста:',1,min = 1,max = 10,step = 1),
                             #div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
                             #div(style=paste0("display: inline-block;vertical-align:top; margin-top: ",spacer_wid," px; width: ", as.integer(input_wid/3),"px;"),actionButton(label = "Назначить куст",inputId = "assignPlatform")),
                             actionButton(label = "Назначить куст",inputId = "assignPlatform"),
                             dataTableOutput(outputId = 'wells')
                             #actionButton("asd","asd")
                             )
                    )
        
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #plotOutput("distPlot"),
        dataTableOutput(outputId = 'dataOutput'),
         actionButton("asd","asd")
      )
   )
)

getFilteredData <- function(data=NULL,day=1,mon=1,pars = names(parNames)) {
  if(is.null(data)) return(NULL)
  if(is.null(pars) || length(pars)<1) pars = names(parNames)
  scalingPars = c("WGPT","WWPT")
  #dataProduction[1,] = 0
  #browser()
  #DP = data[,-1]
  #browser()
  years = sapply(data$DATE,function(x) as.numeric(format(x, "%Y")))
  days = sapply(data$DATE,function(x) as.numeric(format(x, "%d")))
  mons = sapply(data$DATE,function(x) as.numeric(format(x, "%m")))
  DP = cbind(data,years,mons,days)
  #yearlyData[,!names(yearlyData) %in% c('years','mons','days')]
  procNames = names(DP)[(!names(DP) %in% c('years','mons','days','DATE'))]
  #procNames = names(DP)[(!procNames %in% c('years','mons','days','DATE'))]
  prcnm = list()
  for(i in 1:length(pars))
    prcnm = append (prcnm,grep(pars[i],procNames,value=TRUE))
  procNames = unlist(prcnm)
  #browser()
  reducedData = DP[(DP$days == day & DP$mons == mon),]
  reducedYears = years[(DP$days == day & DP$mons == mon)]
  #reducedData = sapply(reducedData,function (x) x*10^-9)
  # calc yearly diff
  #getParName(procNames[1])
  reducedData = sapply(procNames,function(x) { 
    par = strsplit(x,':')[[1]][1] 
    if(par %in% scalingPars) reducedData[,x]*10^-9 
    else reducedData[,x]
    })
  #browser()
  reducedData = cbind(years = reducedYears,
                      sapply(procNames,function(x) diff(reducedData[,x]) ))
  reducedData = data.frame(reducedData)
  # for (i in 1:length(names(reducedData))) 
  # {
  #   q = diff(as.matrix(reducedData[,i])); 
  #   reducedData[,i] = c(0,q)
  # }
  #browser()
  cnames = reducedData$years[-1]
  rnames = names(reducedData)[-1]
  reducedData = t(reducedData[-1,-1])
  colnames(reducedData) = cnames
  rownames(reducedData) = rnames
  return(reducedData)
}

getPlatformName <- function (x = NULL) {
    str = strsplit(x,'_')[[1]]
    if(length(str)==2) return(str[1])
    else return(NA)
}  
# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$dataOutput <- renderDataTable({
     datatable(data = myReactives$FilteredData,
               height = 30,
               class = "compact",
               rownames = TRUE,
               selection = list (
                 mode = 'multiple',
                 #      selection  = sr,
                 target = 'row'
               ),
               options = list(
                 pagingType = "simple",
                 paging = FALSE,
                 ColumnRender = prettyNum,
                 scrollY = "500px",
                 scrollX = TRUE,#"800px",
                 scrollCollapse = TRUE,
                 #      stateSave = TRUE,
                 pageLength = 30#,
                 # columnDefs = list(
                 #   list(visible = FALSE, targets = c(2:3)),
                 #   list(orderable = FALSE, className = 'details-control', targets = 0)
                 # )
               )#,
               # callback = JS("
               #               table.column(1).nodes().to$().css({cursor: 'pointer'});
               #               var format = function(d) {
               #               return '<div style=\"background-color:#eee; padding: .5em;\"> Loc: (X:' +
               #               Math.round(d[2]) + ' Y:' + Math.round(d[3]) + ')</div>';
               #               };
               #               table.on('click', 'td.details-control', function() {
               #               var td = $(this), row = table.row(td.closest('tr'));
               #               if (row.child.isShown()) {
               #               row.child.hide();
               #               td.html('&oplus;');
               #               } else {
               #               row.child(format(row.data())).show();
               #               td.html('&CircleMinus;');
               #               }
               #               });"
               # 
               # )
     ) #%>% formatDate(table = myReactives$data,columns =  1, method = "toLocaleDateString")
   })
   
   output$wells <-renderDataTable({
     datatable(myReactives$wells,
               height = 15,
               class = "compact",
               rownames = FALSE,
               filter = 'none',
               editable = F,
               options = list(
                 pagingType = "simple",
                 paging = FALSE,
                 ColumnRender = prettyNum,
                 scrollY = "400px",
                 #scrollX = "800px",
                 scrollCollapse = TRUE,
                 #      stateSave = TRUE,
                 pageLength = 30#,
                 # columnDefs = list(
                 #   list(visible = FALSE, targets = c(2:3)),
                 #   list(orderable = FALSE, className = 'details-control', targets = 0)
                 # )
               )#,
     )
     })
   
   output$pars <-renderDataTable({
     #browser()
     datatable(myReactives$pars,
               height = 10,
               class = "compact",
               rownames = FALSE,
               filter = 'none',
               editable = TRUE,
               options = list(
                 pagingType = "simple",
                 paging = FALSE,
                 ColumnRender = prettyNum,
                 scrollY = "400px",
                 #scrollX = "800px",
                 scrollCollapse = TRUE,
                 #      stateSave = TRUE,
                 pageLength = 30#,
                 # columnDefs = list(
                 #   list(visible = FALSE, targets = c(2:3)),
                 #   list(orderable = FALSE, className = 'details-control', targets = 0)
                 # )
               )#,
               
     )
     })
   
   observeEvent(input$assignPlatform, {
     if(is.null(input$wells_rows_selected)) {
       showNotification("Выберите скважины в списке", type = 'warning')
     } else {
       #browser()
       myReactives$wells$platform[input$wells_rows_selected] = input$platformIdx
     }
   })
   
   observeEvent(input$pars_rows_selected, {
     myReactives$FilteredData = getFilteredData(data = myReactives$data ,
                                                day = input$startDay,
                                                mon=input$startMon,
                                                pars = names(parNames[input$pars_rows_selected]))
     
   })
   
   observeEvent(input$openDataFile, {
     dataProduction  <- read_delim(input$openDataFile$datapath, "\t", escape_double = FALSE, 
                                col_types = cols(
                                  .default = col_double(), 
                                  DATE = col_datetime(format = "%d %b %Y"))
                                ,na = "0", trim_ws = TRUE)

     if (is.null(dataProduction)) 
       showNotification(ui = "Ошибка при загрузке данных.
Предполагается следующий формат:
DATE  PAR:WELL  PAR:WELL ...",
                        type = "error")
     else {
       showNotification(ui = paste(length(dataProduction[,1])," точек загружено"),
                        type = "default")
       #dates = as.numeric(format(data$date_of_purchase, "%Y"))
       # dates = sapply(dataProduction$DATE,function(x) list(
       #   year = as.numeric(format(x, "%Y")),
       #   month = as.numeric(format(x, "%M")),
       #   day = as.numeric(format(x, "%dd"))
       #   ))
       colnames = strsplit(x = names(dataProduction[-1]),':')
       pars = unique(sapply(colnames,function(x) x[1]))
       names = sapply(pars,function(x) parNames[[x]])
       wells =  unique(sapply(colnames,function(x) x[2]))
       platforms = sapply(wells,getPlatformName)
       myReactives$wells = data.frame(well = wells,platform = as.numeric(platforms))
       myReactives$pars = data.frame(param = pars,name = names)
       myReactives$data = dataProduction[-1,]
       myReactives$FilteredData <- getFilteredData(data = myReactives$data ,
                                                   day = input$startDay,
                                                   mon=input$startMon,
                                                   pars = names(parNames[input$pars_rows_selected]))
       
       #myReactives$data = reducedData
       #updateDateInput(session = session,
       #                inputId = input$startDate,
       #                value = as.Date(x = myReactives$data[1]$DATE[1],format="%d %b %Y")
       #               )
       #names(dataProduction)
     }
   })
   }

# Run the application 
shinyApp(ui = ui, server = server)


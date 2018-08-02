#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(DT)
library(stats)
library(xlsx)
library(shiny)
# INIT: lists ####
parNames = list("Накопленная добыча газа",
                    "Накопленная добыча воды",
                    "Пластовое давление",
                    "Устьевое давление")
names(parNames) = c ( "WGPT","WWPT","WBP9","WTHP" )

tableTypes = list('yearly','platform')
names(tableTypes) = c('годичная','по кустам')

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
        radioButtons(inputId = 'tableMode','Отобразить:',choices = tableTypes,inline = T),
        dataTableOutput(outputId = 'dataOutput'),
        downloadButton('downloadSummary', 'Сохранить в файл')
      )
   )
)

# Debug information print with call info
dbgmes <- function (message = "message", expr = NULL, depth = 1) {
  #browser()
  calls = sys.calls()
  depth=min(depth,length(calls))
  range = c(length(calls)-depth:length(calls))
  funame = unlist(strsplit(x = as.character(calls[range]),split = "\\("))
  cat(paste0('==>',funame[1],':\n'))
  cat(paste0(message,':',capture.output(expr),'\n'))
}

sumPart <- function (x,y) {
  #dbgmes('y=',y)
  return(sum(x,na.rm = T))
}

getNwells <- function (x) {
  #browser()
  #dbgmes("x=",x)
  return(as.integer(length(x[!is.na(x) & x>0])) )
}

getSum <- function(x) {
  #dbgmes("x=",x)
  if(class(x)=='numeric')
    res = sum(x,na.rm = T)
  else res = NA
  #browser()
  #dbgmes("res=",res)
  return(res)
}

getNA <- function(x) {
  return(NA)
}

getInOut <- function(x,inwell = T) {
  dbgmes("x=",x)
  if(class(x)=='numeric')
    if(inwell) if(x>0) res=x else res = 0
    else res = if(x<0) res=x else res = 0
  else res = NA
  dbgmes("res=",res)
  return(res)
}

getDataSubset <- function(data = NULL, pattern="", FUN = mean) {
  if(is.null(data)) return(NULL)
     
  procCols = colnames(data)[c(-1:-3)]
  id_res = grep(x = data$par,pattern)
  
  prod_data = data[id_res,]
  filterList = list( unlist(prod_data[,3]),unlist(prod_data[,1]))
  prod_res = aggregate(x = prod_data[,procCols],by = filterList, FUN = FUN)[,c(-1:-2)]
  #browser()
  factors = aggregate(x = prod_data[,c(3,1)],by = filterList, FUN = getNA)[,c(1:2)]
  prod_res = cbind(factors,prod_res)
  
  return(prod_res)
}

filterByPlatform <- function (data = NULL) {
  if(is.null(data)) return(NULL)
  scalingPars = c("WGPT","WWPT")
  factNames=colnames(data)[c(1:3)]
  colnames(data)[c(1:3)] = c('par','well','platf')
  #platforms = as.numeric(unique(data$'куст'))
  #dbgmes("platforms=",platforms)
  #procCols = colnames(data)[c(-1:-3)]
  #browser()
  
  prod_gas = getDataSubset(data,pattern = "добыча газа",FUN = getSum)
  prod_wat = getDataSubset(data,pattern = "добыча воды",FUN = getSum)
  pres_avg = getDataSubset(data,pattern = "давление")
  prod_col = getDataSubset(data,pattern = "добыча газа",FUN = getNwells)

  
  # id_prod = grep(x = data$par,pattern = "добыча")
  # id_pres = grep(x = data$par,pattern = "давление")
  # id_strt = grep(x = data$par,pattern = "добыча газа")
  # 
  # prod_data = data[id_prod,]
  # filterList = list( unlist(prod_data[,3]),unlist(prod_data[,1]))
  # prod_sum = aggregate(x = prod_data[,procCols],by = filterList, FUN = getSum)[,c(-1:-2)]
  # factors = aggregate(x = prod_data[,c(3,1)],by = filterList, FUN = getNA)[,c(1:2)]
  # prod_sum = cbind(factors,prod_sum)
  # 
  # prod_data = data[id_pres,]
  # filterList = list( unlist(prod_data[,3]),unlist(prod_data[,1]))
  # pres_avg = aggregate(x = prod_data[,procCols],by = filterList, FUN = mean, na.rm = T)[,c(-1:-2)]
  # factors = aggregate(x = prod_data[,c(3,1)],by = filterList, FUN = getNA)[,c(1:2)]
  # pres_avg = cbind(factors,pres_avg)
  # 
  # prod_data = data[id_strt,]
  # filterList = list( unlist(prod_data[,3]),unlist(prod_data[,1]))
  # prod_in = aggregate(x = prod_data[,procCols],by = filterList, FUN = getNwells)[,c(-1:-2)]
  # factors = aggregate(x = prod_data[,c(3,1)],by = filterList, FUN = getNA)[,c(1:2)]
  
  factors = prod_col[,c(1:2)]
  prod_in = t(diff(rbind(0,t(prod_col[,c(-1:-2)]))))
  prod_out = apply(prod_in,MARGIN = c(1,2),FUN = function(x) min(0,x))
  prod_in = apply(prod_in,MARGIN = c(1,2),FUN = function(x) max(0,x))
  prod_in = cbind(factors,prod_in)
  prod_out = cbind(factors,prod_out)

  #browser()
  prod_in$Group.2 = rep('ввод скважин',times = nrow(prod_in))
  prod_out$Group.2 = rep('выбытие скважин',times = nrow(prod_out))
  prod_gas$Group.2 = gsub('Накопленная д','Д', prod_gas$Group.2) 
  prod_wat$Group.2 = gsub('Накопленная д','Д', prod_wat$Group.2) 
  fdf = rbind(prod_in,prod_out,prod_gas,prod_wat,pres_avg)
  colnames(fdf)[1:2] = c('куст','параметр')
  fdf = fdf[order(fdf[1]),]
  #browser()
  #fdf = fdf[order('куст',)]
  return(fdf)  
}
  
getFilteredData <- function(data=NULL,day=1,mon=1,pars = names(parNames)) {
  if(is.null(data)) return(NULL)
  if(is.null(pars) || length(pars)<1) pars = names(parNames)
  scalingPars = c("WGPT")
  diffPars = c("WGPT","WWPT")
  #dataProduction[1,] = 0
  #browser()
  #DP = data[,-1]
  #browser()
  colnames = strsplit(x = names(data[-1]),':')
  #params =  sapply(colnames,function(x) parNames[[x[1]]])
  params =  sapply(colnames,function(x) parNames[[x[1]]])
  wells =  sapply(colnames,function(x) x[2])
  platforms = sapply(wells,getPlatformName)
  years = sapply(data$DATE,function(x) as.numeric(format(x, "%Y")))
  days = sapply(data$DATE,function(x) as.numeric(format(x, "%d")))
  mons = sapply(data$DATE,function(x) as.numeric(format(x, "%m")))
  DP = cbind(data,years,mons,days)
  #browser()
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
    res = reducedData[,x]
    if(par %in% scalingPars){
      res = res*10^-9
    }
    if(par %in% diffPars){
      res[is.na(res)] = 0
      res = c(res[1],diff(res))
    }
    
    return(res)
    })
  reducedData = cbind(years = reducedYears,reducedData)
  #                    sapply(procNames,function(x) diff(reducedData[,x]) ))
  reducedData = data.frame(reducedData)
  # for (i in 1:length(names(reducedData))) 
  # {
  #   q = diff(as.matrix(reducedData[,i])); 
  #   reducedData[,i] = c(0,q)
  # }
  #browser()
  cnames = reducedData$years[-1]
  rnames = names(reducedData)[-1]
  #params = gsub('Накопленная д','Д', params) 
  reducedData = t(reducedData[-1,-1])
  #rownames(reducedData) = rnames
  reducedData=data.frame(params,wells,platforms,reducedData)
  colnames(reducedData) = c('Параметр','скважина','куст',cnames)
  return(reducedData)
}

getPlatformName <- function (x = NULL) {
    str = strsplit(x,'_')[[1]]
    if(length(str)==2) return(str[1])
    else return(NA)
}  
# Define server logic required to draw a histogram

#options(shiny.encoding = "UTF-8")
#options(encoding = "UTF-8")
options(shiny.host = "0.0.0.0")
options(shiny.port = 8080)

server <- function(input, output,session) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$dataOutput <- renderDataTable({
     widetarget = 0
     if(input$tableMode == 'yearly'){
       data = myReactives$FilteredData
       widetarget = 0
       datacols = colnames(data)[c(-1:-3)]
     }
     else if(input$tableMode == 'platform') {
       data = myReactives$FilteredByPlatform#filterByPlatform(myReactives$FilteredData)
       widetarget = 1
       datacols = colnames(data)[c(-1:-2)]
     }
     
     if(is.null(data) || nrow(data)<2) return(NULL)
     datatable(data = data,
               height = 30,
               class = "compact",
               rownames = FALSE,
               selection = list (
                 mode = 'multiple',
                 #      selection  = sr,
                 target = 'row'
               ),
               options = list(
                 pagingType = "simple",
                 paging = FALSE,
                 #ColumnRender = prettyNum,
                 scrollY = "500px",
                 scrollX = TRUE,#"800px",
                 scrollCollapse = TRUE,
                 #      stateSave = TRUE,
                 pageLength = 30,
                 autoWidth = TRUE,
                 columnDefs = list(list(width = '200px', targets = widetarget))#,
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
     ) #%>%  formatRound(columns = c(4:ncol(data)),digits = 5)#  %>% formatStyle(columns = c(widetarget), width='200px') 
     
     #%>% formatDate(table = myReactives$data,columns =  1, method = "toLocaleDateString")
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
   
   output$platforms <-renderDataTable({
     datatable(myReactives$platforms,
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
       myReactives$FilteredByPlatform = filterByPlatform(myReactives$FilteredData)
       #myReactives$data = reducedData
       #updateDateInput(session = session,
       #                inputId = input$startDate,
       #                value = as.Date(x = myReactives$data[1]$DATE[1],format="%d %b %Y")
       #               )
       #names(dataProduction)
     }
   })
   getSaveFilename <- function() {
     fn = paste0('ProductionSummary_',basename(input$openDataFile$datapath),'-',Sys.Date(), '.xlsx')
     return(fn)
   }
   getSaveContent <- function(fname) {
     if(input$tableMode == 'yearly') 
       data = myReactives$FilteredData
     else if(input$tableMode == 'platform') 
       data = myReactives$FilteredByPlatform#filterByPlatform(myReactives$FilteredData)
     # Create WorkBook with Sheets 
     write.xlsx(sheetName = 'годовая', showNA=FALSE,x = myReactives$FilteredData, file = fname)
     write.xlsx(sheetName = 'по кустам', showNA=FALSE, append=TRUE ,x = myReactives$FilteredByPlatform, file = fname)
   }
   
   output$downloadSummary <- downloadHandler(
     filename = getSaveFilename,
     contentType = '.asc',
     content = getSaveContent
   )
}

# Run the application 
shinyApp(ui = ui, server = server)


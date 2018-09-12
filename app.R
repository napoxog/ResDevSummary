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
parNames = list("Добыча газа",
                "Добыча воды",
                "Пластовое давление (9 точек)",
                "Пластовое давление (5 точек)",
                "Пластовое давление (4 точки)",
                "Пластовое давление (3 точки)",
                "Устьевое давление",
                "Забойное давление")
names(parNames) = c ( "WGPT",
                      "WWPT",
                      "WBP9",
                      "WBP5",
                      "WBP4",
                      "WBP3",
                      "WTHP",
                      "WBHP")

scalingPars = c("WGPT")
diffPars = c("WGPT","WWPT")

tableTypes = list('periodic','platform','total')
names(tableTypes) = c('за период','по кустам','общая')

dayNames = c(1:31)
monNames = c(1:12)
names(monNames) = date_names_lang("ru")$mon_ab

periodNames = c(12,6,3,1)
names(periodNames) = c("год",
                       "пол года",
                       "квартал",
                       "месяц")
periodAbbr = c('.','.','.','.')
names(periodAbbr) = names(periodNames)


input_wid = 500
spacer_wid = 30

myReactives = reactiveValues(wells = NULL, platforms = NULL, data = NULL, pars = NULL)

# Define UI ####
ui <- fluidPage(
   
   # Application title
   titlePanel("Сводная добыча"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tabsetPanel(id = 'input', 
                    tabPanel(title = "Данные", id = 'data',
                             #actionButton()
                             div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(input_wid/2),"px;"),
                                 fileInput(
                                 "openDataFile",
                                 "Загрузить накопленную добычу:",
                                 accept = '.txt',
                                 buttonLabel = "Открыть..."
                             )),
                             #"Начальная дата:",
                             div(style=paste0("display: inline-block;vertical-align:top; width: ", spacer_wid,"px;"),HTML("<br>")),
                             #div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(input_wid/4),"px;"),selectInput(inputId = 'startDay',label = 'день', choices = dayNames)),
                             div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(input_wid/4),"px;"),
                                 selectInput(inputId = 'period',label = 'Период', choices = periodNames)),
                             #div(style=paste0("display: inline-block;vertical-align:top; width: ", as.integer(input_wid/4),"px;"),selectInput(inputId = 'startMon',label = 'месяц', choices = monNames))
                             #selectInput(inputId = 'startMon',label = 'месяц', choices = monNames),
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
        tabsetPanel(id = 'output', 
                    tabPanel(title = "Таблицы", id = 'table',
        radioButtons(inputId = 'tableMode','Отобразить:',choices = tableTypes,inline = T),
        dataTableOutput(outputId = 'dataOutput'),
        downloadButton('downloadSummary', 'Сохранить в файл')
                    ),
        tabPanel(title = "Графики", id = 'plot',
                 plotOutput("distPlot", height = "500px")
        )
        )
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
  if(class(x)=='numeric')
    res = sum(x,na.rm = T)
  else res = NA
  return(res)
}

getNA <- function(x) {
  return(NA)
}

getAvg <- function(x) {
  if(class(x)=='numeric')
    res = mean(x,na.rm = T)
  else res = NA
  return(res)
}

getInOut <- function(x,inwell = T) {
  #dbgmes("x=",x)
  if(class(x)=='numeric')
    if(inwell) if(x>0) res=x else res = 0
    else res = if(x<0) res=x else res = 0
  else res = NA
  #dbgmes("res=",res)
  return(res)
}

getDataSubset <- function(data = NULL, pattern="", FUN = mean, total = FALSE) {
  if(is.null(data)) return(NULL)
     
  procCols = colnames(data)[c(-1:-3)]
  id_res = grep(x = data$par,pattern)
  
  prod_data = data[id_res,procCols]
  if(nrow(prod_data)<1) return(NULL)
  #browser()
  if(total) {
    data[id_res,3] = 1
  }
  filterList = list( unlist(data[id_res,3]),unlist(data[id_res,1]))
  prod_res = aggregate(x = prod_data,by = filterList, FUN = FUN)[,c(-1:-2)]
  factors = aggregate(x = data[id_res,c(3,1)],by = filterList, FUN = getNA)[,c(1:2)]
  prod_res = cbind(factors,prod_res)
  
  return(prod_res)
}

getFilteredByPaltform <- function (data = NULL, total = FALSE) {
  if(is.null(data)) return(NULL)
  scalingPars = c("WGPT","WWPT")
  factNames=colnames(data)[c(1:3)]
  colnames(data)[c(1:3)] = c('par','well','platf')
  #platforms = as.numeric(unique(data$'куст'))
  #dbgmes("platforms=",platforms)
  #procCols = colnames(data)[c(-1:-3)]
  
  #dbgmes("data=",dim(data))
  prod_gas = getDataSubset(data,pattern = "обыча газа",FUN = getSum,total)
  prod_wat = getDataSubset(data,pattern = "обыча воды",FUN = getSum,total)
  pres_avg = getDataSubset(data,pattern = "авление",FUN = getAvg,total)
  prod_col = getDataSubset(data,pattern = "обыча газа",FUN = getNwells,total)
  
  #dbgmes("prod_gas=",(prod_gas[,-2]))
  #dbgmes("prod_wat=",(prod_wat[,-2]))
  #dbgmes("pres_avg=",(pres_avg[,-2]))
  #dbgmes("prod_col=",(prod_col[,-2]))
  
  resList = c()
  
  if(!is.null(prod_col)) {
    factors = prod_col[,c(1:2)]
    prod_in = t(diff(rbind(0,t(prod_col[,c(-1:-2)]))))
    prod_out = apply(prod_in,MARGIN = c(1,2),FUN = function(x) min(0,x))
    prod_in = apply(prod_in,MARGIN = c(1,2),FUN = function(x) max(0,x))
    prod_in = cbind(factors,prod_in)
    prod_out = cbind(factors,prod_out)
    prod_in$Group.2 = rep('ввод скважин',times = nrow(prod_in))
    prod_out$Group.2 = rep('выбытие скважин',times = nrow(prod_out))
    resList = rbind(resList,rbind(prod_in,prod_out))
    ## empty other parameters if there is no wells in production 
    procCols = colnames(data)[c(-1:-3)]
    procPlatf = as.numeric(unique(pres_avg[,1]))
    
    for(ip in procPlatf)
      for(x in procCols) {
        if(prod_col[ip,x]<1) {
          if(!is.null(pres_avg)) pres_avg[ip,x]=NA;
          if(!is.null(prod_gas)) prod_gas[ip,x]=NA;
          if(!is.null(prod_wat)) prod_wat[ip,x]=NA;
        }
        #dbgmes("i,x,pp,pc=",c(ip,x,pres_avg[ip,x],prod_col[ip,x])) 
      }
  }
  if(!is.null(prod_gas)) resList = rbind(resList,prod_gas)
  if(!is.null(prod_wat)) resList = rbind(resList,prod_wat)
  if(!is.null(pres_avg)) resList = rbind(resList,pres_avg)
  

  #browser()
  #fdf = rbind(prod_in,prod_out,prod_gas,prod_wat,pres_avg)
  fdf = data.frame(rbind(resList))
  colnames(fdf)[1:2] = c('куст','параметр')
  colnames(fdf)[c(-1,-2)] = colnames(data)[c(-1,-2,-3)]
  fdf = fdf[order(fdf[1]),]
  return(fdf)  
}
  
getFilteredData <- function(data=NULL,day=1,mon=1,period = 12,pars = parNames) {
  if(is.null(data)) return(NULL)
  #browser()
  #if(is.null(pars) || length(pars)<1) pars = names(parNames)
  #else pars = names(parNames[pars])
  #browser()
  pars = names(pars)
  #dbgmes("filter=",pars)
  #dataProduction[1,] = 0
  #browser()
  #DP = data[,-1]
  #browser()
  years = sapply(data$DATE,function(x) as.numeric(format(x, "%Y")))
  days = sapply(data$DATE,function(x) as.numeric(format(x, "%d")))
  mons = sapply(data$DATE,function(x) as.numeric(format(x, "%m")))
  DP = cbind(years,mons,days,data)
  #browser()
  #yearlyData[,!names(yearlyData) %in% c('years','mons','days')]
  procCols = names(DP)[(!names(DP) %in% c('years','mons','days','DATE'))]
  #browser()
  # dbgmes("dayFilter=",day)
  # dbgmes("dayFilter=",DP$days)
  # dbgmes("monFilter=",mon)
  # dbgmes("monFilter=",DP$mons)
  ### Periodic filter ####
  #reducedData = DP[(DP$days == day & DP$mons == mon),]
  #reducedYears = years[(DP$days == day & DP$mons == mon)]

  # get set of check months of 1st day
  checkDate = (seq(1:floor(12 / period))*period) %% 12 + 1
  reducedData = DP[(DP$days == 1 & (DP$mons %in% checkDate)),]
  reducedYears = years[(DP$days == 1 & (DP$mons %in% checkDate))]
  reducedMonts = mons[(DP$days == 1 & (DP$mons %in% checkDate))]
  dbgmes("checkdate=",checkDate)
  #browser()
  
  sep = periodAbbr[which(periodNames == period)]
  colnames = strsplit(x = names(reducedData[c(-1:-4)]),':')
  params =  unlist(sapply(colnames,function(x) x[1]))
  wells =  sapply(colnames,function(x) x[2])
  platforms = sapply(wells,getPlatformName)
  ## sclae and diff the production
  reducedData = sapply(procCols,function(x) { 
    par = strsplit(x,':')[[1]][1] 
    res = reducedData[,x]
    if(par %in% scalingPars){
      res = res*10^-9
    }
    if(par %in% diffPars){
      res[is.na(res)] = 0
      res = c(diff(res),0)
    }
    return(res)
    })
  reducedData = sapply(procCols,function(x) { 
    return(round(reducedData[,x],5))
  })
  reducedData = cbind(years = reducedYears,reducedData)
  reducedData = data.frame(reducedData)
  
  #browser()
  #combine Pariods
  #paste0(reducedYears,".P",round(reducedMonts/period))
  reducedYears+(reducedMonts / 12)
  #cnames = reducedData$years[-1]
  #cnames = reducedYears+round((reducedMonts) / 100,2)#[-1]
  cnames = paste0(reducedYears,sep,(reducedMonts-1) / period+1)#[-1]
  #dbgmes("years = ",cnames)
  rnames = names(reducedData)#[-1]
  #params = gsub('Накопленная д','Д', params) 
  reducedData = t(reducedData)[-1,]
  #rownames(reducedData) = rnames
  reducedData=data.frame(params,wells,platforms,reducedData)
  #browser()
  reducedData=reducedData[reducedData$params %in% pars,]
  #reducedData$params = sapply(reducedData$params,function(x) parNames[[x]])
  reducedData$params = unlist(lapply(X = as.character(reducedData$params),
                                     FUN = function(x) parNames[x]))
  #browser()
  colnames(reducedData) = c('Параметр','скважина','куст',cnames)
  return(reducedData)
}

getPlatformName <- function (x = NULL) {
    str = strsplit(x,'_')[[1]]
    if(length(str)==2) return(str[1])
    else return(NA)
}  

setPaletteTransp <- function(colors = NULL ,alpha = 0.5) {
  if(is.null(colors)) return(NULL)
  
  colors = apply(sapply(colors, col2rgb)/255, 2, 
                 function(x) rgb(x[1], x[2], x[3], alpha=alpha)) 
  #dbgmes("transpal=",colors)
  return(colors)
}

plotData <- function(data = NULL,pattern=NULL,period = 12) {
  if(is.null(data)) return(NULL)
  
  colnames(data)[1:2] = c('platf','param')
  id_res = grep(x = data$param,pattern)
  
  data = data[id_res,]
  
  dbgmes("data=",dim(data))
  if(dim(data)[1]<1) return(NULL)
  platfs = unique(data$platf)
  rem_cols = c(-1,-2)
  data = cbind(data[,-1*rem_cols],apply(data[,rem_cols],MARGIN = c(1,2), 
        FUN = function(x) if(is.na(x)) return(0) else return(x)))
  plotData=list()
  #browser()
  q=colnames(data)[rem_cols]
  q=apply(cbind(q),1,FUN = function(x) {
    yp=unlist(strsplit(x = x,split = '\\.'))
    p = as.numeric(yp[1]) +  (as.numeric(yp[2]))/period/12
    #browser();
    return (p)
  })
  plotData = data.frame(year = as.numeric(q))
  cols = setPaletteTransp(rainbow(length(unique(data$platf))),alpha = 0.5)
  #dbgmes("YR=",colnames(data)[rem_cols])
  #dbgmes("X=",plotData[[1]])
  i=1
  for(row in rownames(data)) {
    if(i>1) q = plotData[i]
    else q= 0
    #browser()
    #dbgmes("add=",(q + as.numeric(data[row,rem_cols])))
    plotData = data.frame(plotData, row = (q + as.numeric(data[row,rem_cols])))
    color = cols[data[row,1]]
    par(new = TRUE)
#    plot(plotData,bty="n",yaxt = "n",t='b', col = color, xlab = "" ,ylab = "")
#    axis(2,col = color,col.lab = color)
    i=i+1
  }
  xlim = c(min(plotData[[1]]),max(plotData[[1]]))
  ylim = c(0,1.1*max(plotData[2:length((plotData))]))
  dbgmes("limits=",cbind(xlim,ylim))
  #browser()
  plot(plotData$year,t='l', xlab = "годы" ,ylab = "добыча газа, млрд куб.м / год", 
       ylim = ylim,
       xlim = xlim)
  grid()
  #par(new = TRUE)
  xx = c(plotData[[1]],rev(plotData[[1]]))
  #browser()
  for(row in 1:length(rownames(data))) {
    if(row==1) y0 = rep(0,length(plotData[[1]]))
    else y0=plotData[[row]]
    yy = c(y0,rev(plotData[[row+1]]))
    #browser()
    #dbgmes("poly=",cbind(xx,yy))
    polygon(xx,yy,col = cols[row],border = cols[row])
    #legend(xlim[2],ylim[1],data[2,])
    #plot(plotData[row],bty="n",yaxt = "n",t='b', col = cols, xlab = "" ,ylab = "")
  }
}

# Define server logic required to draw a histogram

#options(shiny.encoding = "UTF-8")
#options(encoding = "UTF-8")
options(shiny.host = "0.0.0.0")
options(shiny.port = 31337)

server <- function(input, output,session) {
   # PLOT DATA ####
   output$distPlot <- renderPlot({
     #browser()
     plotData(data = myReactives$FilteredByPlatform,pattern = "Добыча газа", period = as.numeric(input$period))
   })
   #dataOutput <- renderDataTable####
   output$dataOutput <- renderDataTable({
     widetarget = 0
     if(input$tableMode == 'periodic'){
       data = myReactives$FilteredData
       widetarget = 0
       datacols = colnames(data)[c(-1:-3)]
     }
     else if(input$tableMode == 'platform') {
       data = myReactives$FilteredByPlatform#filterByPlatform(myReactives$FilteredData)
       widetarget = 1
       datacols = colnames(data)[c(-1:-2)]
     }
     else if(input$tableMode == 'total') {
       data = myReactives$TotalPeriodic#filterByPlatform(myReactives$FilteredData)
       widetarget = 1
       datacols = colnames(data)[c(-1:-2)]
     }
     # PLOT MAIN TABLE ####
     #browser()
     if(is.null(data) || nrow(data)<2) return(NULL)
     #dbgmes("myReactives$Maindata=",data)
     datatable(data = data,
               height = 30,
               class = "compact",
               rownames = FALSE,
               selection = list (
                 mode = 'none',
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
                 autoWidth = TRUE
                 ,columnDefs = list(list(width = '200px', targets = widetarget)
#                                    ,list(targets = c(4:ncol(data)), render = JS(
# "function (data,type, row, meta) {
#  function decimalAdjust(type, value, exp) {
# if (typeof exp === 'undefined' || +exp === 0) {
#       return Math[type](value);
#     }
# if (!Math.round10) {
#     Math.round10 = function(value, exp) {
#       return decimalAdjust('round', value, exp);
#     };
#   }console.log(Math.round10(data);
# return Math.round10(data,-3);"))
                                   )#,
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
     ) #%>% formatRound(columns = c(4:ncol(data)),digits = 5) #%>% 
       #formatStyle(columns = c(widetarget), '300px') %>% 
       #formatStyle(columns = c(1:ncol(data)), valueColumns = 3,
      #             backgroundColor = styleEqual(c(1:length(unique(data[,3]))), c('gray20', 'gray50','gray80')))
      #%>%  formatRound(columns = c(4:ncol(data)),digits = 5) 
     #%>% formatDate(table = myReactives$data,columns =  1, method = "toLocaleDateString")
   })
   #input$wells ####
   observeEvent(input$wells, {
     dbgmes("wells=",myReactives$wells)
   })
   #myReactives$pars####
   observeEvent(myReactives$pars, {
     dbgmes("myReactives$pars=",myReactives$pars)
   })
   
   #dtParsProxy = dataTableProxy("pars")  
   
   #renderDataTable wells####
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
                 #ColumnRender = prettyNum,
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
   #renderDataTable platforms ####
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
   #renderDataTable pars####
   output$pars <-renderDataTable({
     #browser()
     datatable(myReactives$pars,
               height = 10,
               class = "compact",
               rownames = FALSE,
               filter = 'none',
               editable = FALSE,
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
   #input$assignPlatform, ####
   setPaltf <- function(x="WPAR:1_1",platform=2,wells = NULL) {
     if(is.null(wells)) return(x)
     pfw = unlist(strsplit(x,":"))
     p=pfw[1]
     wf=unlist(strsplit(pfw[2],"_"))
     w=wf[2]
     f=wf[1]
     #browser()
     idx = which(wells$well == pfw[2])
     if( idx>0 & idx %in% input$wells_rows_selected) res = paste0(p,':',platform,'_',w)
     else res = x;
     return(res)
   }
   
   observeEvent(input$assignPlatform, {
     if(is.null(input$wells_rows_selected)) {
       showNotification("Выберите скважины в списке", type = 'warning')
     } else {
       #browser()
       myReactives$wells$platform[input$wells_rows_selected] = input$platformIdx
       q=apply(cbind(names(myReactives$data)[-1]),MARGIN = 1, FUN = setPaltf,platform=input$platformIdx,wells = myReactives$wells)
       names(myReactives$data)[-1] = q
       
       colnames = strsplit(x = names(myReactives$data),':')
       pars = unique(sapply(colnames,function(x) x[1]))
       names = sapply(pars,function(x) parNames[[x]])
       wells =  unique(sapply(colnames,function(x) x[2]))
       platforms = sapply(wells,getPlatformName)
       myReactives$wells = data.frame(well = wells,platform = as.numeric(platforms))
       #myReactives$data$
       rebuildTableData()
     }
   })

   rebuildTableData <- reactive({
     #observeEvent(input$pars, {
     #browser()
     names = myReactives$pars$name
     names(names) = rownames(myReactives$pars)
     if(is.null(input$pars_rows_selected)) sel_pars = names
     else sel_pars = names[input$pars_rows_selected]
     #myReactives$wells = data.frame(well = wells,platform = as.numeric(platforms))
     #myReactives$pars = data.frame(param = pars,name = names)
     myReactives$FilteredData <- getFilteredData(data = myReactives$data ,
                                                 day = 1,#input$startDay,
                                                 mon = 1,#input$startMon,
                                                 period = as.numeric(input$period),
                                                 pars = sel_pars)
     myReactives$FilteredByPlatform <- getFilteredByPaltform(myReactives$FilteredData)
     myReactives$TotalPeriodic <- getFilteredByPaltform(myReactives$FilteredData,total = TRUE)
     
   })
      
   observeEvent(input$period, {
     rebuildTableData()
   })
   #input$pars, {####
   observeEvent(input$pars_rows_selected, {
     rebuildTableData()
   })
   
   # GET DATA ####
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
       #browser()
       if(is.null(input$pars_rows_selected)) sel_pars = names
       else sel_pars = names[input$pars_rows_selected]
       myReactives$FilteredData <- getFilteredData(data = myReactives$data ,
                                                   day = 1,#input$startDay,
                                                   mon = 1,#input$startMon,
                                                   period = as.numeric(input$period),
                                                   pars = sel_pars)
       myReactives$FilteredByPlatform <- getFilteredByPaltform(myReactives$FilteredData)
       myReactives$TotalPeriodic <- getFilteredByPaltform(myReactives$FilteredData,total = TRUE)
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
     if(input$tableMode == 'period') 
       data = myReactives$FilteredData
     else if(input$tableMode == 'platform') 
       data = myReactives$FilteredByPlatform#filterByPlatform(myReactives$FilteredData)
     else if(input$tableMode == 'total') 
       data = myReactives$TotalPeriodic
     # Create WorkBook with Sheets 
     write.xlsx(sheetName = 'годовая', showNA=FALSE,x = myReactives$FilteredData, file = fname)
     write.xlsx(sheetName = 'по кустам', showNA=FALSE, append=TRUE ,x = myReactives$FilteredByPlatform, file = fname)
     write.xlsx(sheetName = 'общая', showNA=FALSE, append=TRUE ,x = myReactives$TotalPeriodic, file = fname)
   }
   
   output$downloadSummary <- downloadHandler(
     filename = getSaveFilename,
     contentType = '.asc',
     content = getSaveContent
   )
}

# Run the application 
shinyApp(ui = ui, server = server)


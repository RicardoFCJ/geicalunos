library(data.table);library(magrittr);library(ggplot2);library(shiny);library(readxl)
library(shinythemes);library(lubridate);library(plotly);library(shinydashboard)
source("/home/ricardo/geicdatamining/coletando.R")
source("/home/ricardo/geicdatamining/functions.R")
source('functions.R')

ui = dashboardPage(
  dashboardHeader(title='GEIC Alunos'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Aluno',tabName='tab_aluno'),
      numericInput('idaluno',"ID do aluno",value=0),
      actionButton('search_aluno','Buscar')
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'tab_aluno',
              box(width=12,status='info',htmlOutput('alunoinfo')),
              box(width=12,title='Filtros',collapsible = T,collapsed = T,
                  column(12,
                         column(5,textInput('st_date','Data inicial',placeholder = 'DD-MM-YYYY')),
                         column(5,textInput('en_date','Data final',placeholder = 'DD-MM-YYYY')),
                         column(2,actionButton('update_data','Atualizar'))),
                  column(12,selectInput('programa','Programa',choices=c()))),
              column(12,plotlyOutput('graph1')),column(12,imageOutput('graph2')),
              column(12,column(6,plotlyOutput('graph3')),column(6,plotlyOutput('graph4')))
              )
    )
  )
)
vars=reactiveValues(dados=NULL)
server=function(input,output,session){
  observeEvent(input$search_aluno,{
    vars$dados=process(takeit.byalunoSQL(input$idaluno))
    vars$dados[,Week:=week(Data)]
    vars$dados[,YWeek:=paste(ANO.INICIO.Sess,Week,sep='-')]
    availProgs = vars$dados[,sort(as.character(unique(PROGRAMA)))]
    progs=sort(dir('programas'))
    progsNam=sapply(progs,
                    function(x){
                      unlist(strsplit(x,'-'))%>%{paste0(unlist(strsplit(.[2],'\\.')[1])[1],' (',.[1],')')}
                    })
    progs=sapply(progs,function(x)unlist(strsplit(x,'-'))[1])
    progs=intersect(availProgs,progs)
    names(progs)=grep(paste(progs,collapse="|"),progsNam,value=T)
    updateSelectInput(session,'programa',choices=progs)
  })
  
  observeEvent(c(vars$dados,input$update_data),{
    if(!is.null(vars$dados)){
      dados=vars$dados
      maxD=as.Date(Sys.Date())
      minD=dados[,min(Data)]
      if(!input$st_date==''){
        stdate = input$st_date%>%{unlist(strsplit(.,'-'))}%>%{paste(.[3],.[2],.[1],sep='-')}
        dados=dados[Data>=as.Date(stdate)]
        updateTextInput(session,'st_date',value='')
        minD=as.Date(stdate)
        }
      if(!input$en_date==''){
        endate = input$en_date%>%{unlist(strsplit(.,'-'))}%>%{paste(.[3],.[2],.[1],sep='-')}
        dados=dados[Data<=as.Date(endate)]
        updateTextInput(session,'en_date',value='')
        maxD=as.Date(endate)
        }
      vals=dados[,.(uniqueN(Data)),by=YWeek]
      dt=data.table(dates=seq(minD,maxD,by=1))
      dt[,ano:=substring(dates,1,4)]
      dt[,week:=week(dates)]
      dt[,ydate:=as.factor(paste(ano,week,sep='-'))]
      dt=dt[,max(dates),by=ydate]
      sdt=data.table(YWeek=dt[!ydate%in%vals,ydate],V1=rep(0,dt[!ydate%in%vals,.N]))
      vals=rbindlist(list(vals,sdt))
      vals[,YWeek:=factor(YWeek,levels=levels(dt$ydate))]
      
      output$alunoinfo = renderUI(HTML(paste(alunoinfo(dados),collapse='<br>')))
      output$graph1=renderPlotly(
        print(ggplotly(
        ggplot(vals,aes(x=YWeek,y=V1))+geom_bar(stat='identity')+labs(x='Semanas do ano',y="Dias frequentados")+
          scale_x_discrete(breaks=dt$ydate,labels=format.Date(dt$V1,'%d-%m-%Y'))+theme_bw()+theme(axis.text.x = element_text(angle=45,hjust=1))+
          coord_cartesian(ylim=c(0,5))
      )))
      output$graph2=renderImage({
        inp.progr=input$programa
        nome.arq=grep(inp.progr,dir('programas'),value=T)
        progr = ifelse(length(nome.arq)!=0,paste0('programas/',nome.arq),0)
        outfile <- tempfile(fileext = ".png")
        width  <- session$clientData$output_graph2_width
        height <- session$clientData$output_graph2_height
        png(outfile, width=width, height=height)
        if(is.numeric(progr)){
          plot(c(0,200),c(0,100),ann=F,xaxt='n',yaxt='n',bty='n',col='white')
          text(100,50,labels='Programa\nindisponível',cex=3)
        }else{
          drawProg(progr,vars$dados[PROGRAMA%in%as.numeric(input$programa)&CANCELADA==0&NROINTERACAO==1])
        }
        # Return a list containing information about the image
        dev.off()
        list(
          src = outfile,
          contentType = "image/png",
          width = width,
          height = height,
          alt='Caminho'
        )
      })
    }
  })
}

shinyApp(ui,server)
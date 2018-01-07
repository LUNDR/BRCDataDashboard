

library(RCurl)
library(shiny)
library(shinythemes)
require(formattable)
require(rowr)
require(ggplot2)
require(dplyr)
require(DT)
require(magrittr)
require(shinyjs)
require(dygraphs)
require(forecast)
require(Quandl)
require(shinydashboard)
#########################################
#colours
BRCnavy=rgb(38,34,98,maxColorValue = 255)
BRCfuschia=rgb(146,39,143,maxColorValue = 255)
BRCgreen=rgb(175,202,11,maxColorValue = 255)
BRCblue=rgb(0,187,206,maxColorValue = 255)
######

scaleFUN <- function(x) sprintf("%.1f", x)

dataset=readRDS("Appdata.RDS")


dataset=dataset %>% mutate_each(funs(round(.,1)), -date)

date=as.Date(dataset$date, format = "%m %y")
dataset$date=date


###
## ui.R ##

shinyApp(
ui <- dashboardPage(skin="purple",
  dashboardHeader(title = "Data Explorer"),
  dashboardSidebar(
 
  ),
  dashboardBody(
      
    
    fluidRow(
      # A static valueBox
      valueBoxOutput("Sales"),
      
      # Dynamic valueBoxes
      valueBoxOutput("Footfall"),
      
      valueBoxOutput("Prices")
    ),
    fluidRow(
      
      
      box(width=8,"Welcome to the Retail Insight Data Explorer!Select the dataseries you wish to view, and the chart will update. You can also select the time period you wish to view.\n If you roll over the chart you wil; be able to see the data values for each point.")
      
    ),
    fluidRow(
      box(width=8,
       dygraphOutput("Chart")),
      
      
      
      box(width=4,
          tags$style(type="text/css", "selectize-input{font-size:20;line-height:20;}.selectize-dropdown { font-size: 14px; line-height: 14px; }"),
          
          selectInput("selectInput", "Select column:",
                  choices = colnames(dataset[,-which(names(dataset)%in%c("date"))])),
      selectInput("selectInput2", "Select column:",
                  choices = colnames(dataset[,-which(names(dataset)%in%c("date"))])),
      checkboxInput("SecondaryAxis1","Plot on secondary axis", TRUE),
      
     selectInput("selectInput3", "Select column:",
                  choices = colnames(dataset[,-which(names(dataset)%in%c("date"))])),
     checkboxInput("SecondaryAxis2","Plot on secondary axis",TRUE),
      
      sliderInput("range_one", "Date Range:",min =as.Date(min(dataset$date)), max =as.Date(max(dataset$date)), timeFormat ="%b-%Y" ,value = c(as.Date(min(dataset$date)),as.Date(max(dataset$date)))),
      downloadButton("downloadData", "Download Data")
     
      
      )
   
    

    ))
    
  
),

server <- function(input, output) {
    
  output$Sales<-renderValueBox({
    
    latestdate<-max(dataset$date[which(!is.na(dataset[,"BRC-KPMG RSM Total Sales (% yoy change)"]))])
    
    valueBox(dataset[which(dataset$date==latestdate),"BRC-KPMG RSM Total Sales (% yoy change)"],paste(format(latestdate,"%b %Y"),"BRC-KPMG RSM Total Sales (% yoy change)",sep=" - "),color="purple",icon=icon("shopping-cart"))
    
  })
  
  output$Footfall<-renderValueBox({
    
    latestdate<-max(dataset$date[which(!is.na(dataset[,"BRC-Springboard Footfall (% yoy change)"]))])
    
    valueBox(dataset[which(dataset$date==latestdate),"BRC-Springboard Footfall (% yoy change)"],paste(format(latestdate,"%b %Y"),"BRC-Springboard Footfall (% yoy change)",sep=" - "),color="orange",icon=icon("paw"))
    
  })
  
  output$Prices<-renderValueBox({
    
    latestdate<-max(dataset$date[which(!is.na(dataset[,"BRC-Nielsen Shop Price Index (% yoy change)"]))])
    
    valueBox(dataset[which(dataset$date==latestdate),"BRC-Nielsen Shop Price Index (% yoy change)"],paste(format(latestdate,"%b %Y"),"BRC-Nielsen Shop Prices (% yoy change)",sep=" - "),color="aqua",icon=icon("money"))
    
  })
  
    datasetInput<-reactive({dataset[(dataset$date>=min(input$range_one) & dataset$date<=max(input$range_one)),c("date",input$selectInput,input$selectInput2,input$selectInput3)]
      
    })
    
    columnsy1<-reactive({if(input$SecondaryAxis1==TRUE & input$SecondaryAxis2==TRUE){
        a=2}
          else{if(input$SecondaryAxis1==TRUE & input$SecondaryAxis2==FALSE){
                        a=c(2,4)}
          else{if(input$SecondaryAxis1==FALSE & input$SecondaryAxis2==TRUE){
            a=c(2,3)}
            else{
              a=2:4
          }}}
          
          
    })
    
    columnsy2<-reactive({
      x=columnsy1()
      a= subset(2:4,!(2:4 %in% x))
      
    })
    
    output$value<-renderText({
      x=columnsy2()
      x})
    
    df<-reactive({
      df=datasetInput()
      df2=xts(df,order.by=df$date)
    })

    minimumy1<-reactive({
      a=datasetInput()
      b=columnsy1()
      min(a[,b],na.rm=T)})
    
    maximumy1<-reactive({
      a=datasetInput()
      b=columnsy1()
      max(a[,b],na.rm=T)})
    
    minimumy2<-reactive({
      a=datasetInput()
      b=columnsy2()
      
      if(length(b)==0){
        x=minimumy1()
      }else{
      min(a[,b],na.rm=T)}
    })

    maximumy2<-reactive({
      a=datasetInput()
      b=columnsy2()
      if(length(b)==0){
        x=maximumy1()
      }else{
        max(a[,b],na.rm=T)}
    })
    
   
    yaxis2=reactive({
      a=columnsy2()
      if(length(a)==0){"y"}else{"y2"}
    })

    series2<-reactive({
      a=columnsy2()
      if(3 %in% a){"y2"}else{"y"}
      
    })
    series3<-reactive({
      a=columnsy2()
      if(4 %in% a){"y2"}else{"y"}
      
    })
    

    
    
  
    output$Chart <- renderDygraph({
     df2=df()
      
       dygraph<-dygraph(df2[,2:4]) %>%
          dyOptions(colors = c(BRCfuschia,BRCblue,BRCnavy))%>%
               dySeries(names(df2)[2],axis="y")%>%
                dySeries(names(df2)[3],axis=series2())%>%
         dySeries(names(df2)[4],axis=series3())%>%
        dyAxis(name="y",valueRange=c(minimumy1()-0.4,maximumy1()+0.4))%>%
        dyAxis(name=yaxis2(),valueRange=c(minimumy2()-0.4,maximumy2()+0.4))%>%
        dyLegend(labelsSeparateLines = T,show="follow")
        
      
    })
    
    
    output$downloadData <- downloadHandler(
      filename = function() { paste('data', '.csv', sep='') 
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )
    
  })


shinyApp(ui, server)
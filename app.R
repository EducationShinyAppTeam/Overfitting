library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(devtools)

ui <- list(
  dashboardPage(#skin="black",
    
    #Title
    dashboardHeader(title="Overfitting",titleWidth=235),
    
    #Sidebar
    dashboardSidebar(
      width = 235,
      sidebarMenu(
        id = "tabs",
        menuItem("Prerequisites",tabName = "pre",icon = icon("book")),
        menuItem("Overview", tabName = "over", icon = icon("dashboard")),
        menuItem("Exploration", tabName = "first", icon = icon("wpexplorer"))
      )),
    
    #Content within the tabs
    dashboardBody(
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "sidebar.css"),
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ffb6c1}")),
        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ffb6c1}")),
        tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #ffb6c1}"))
      ),
      
      tabItems(
        tabItem(tabName = "pre",
                h3(tags$b("Background: Overfitting")),br(),
                h4(strong("Understanding the overfitting effect:")),
                withMathJax(),
                h4(tags$li("A reasearcher looks at many explanatory variables
                                      and picks the one that predicts Y the best.")),
                
                
                h4(tags$li("But if we draw another sample randomly from the 
                                      same model, it will not fit nearly as well.")),
                
                br(),
                
                div(style = "text-align: center",bsButton("goover", "Go to the overview", icon("bolt"), size = "large"))
        ),
        tabItem(tabName = "over",
                tags$a(href='http://stat.psu.edu/',tags$img(src='PS-HOR-RGB-2C.png', align = "left", width = 180)),
                br(),br(),br(),
                h3(tags$b("About:")),
                h4("This app explores how you can become overconfident
                                      when you are choosing the best explanatory variable from many choices."),
                br(),
                h3(tags$b("Instructions:")),
                h4(tags$li("Move the sliders to change the values of the sample size, the true population correlation 
                                      and the number of variables you are choosing from.")),
                h4(tags$li("You need to ",
                           
                           tags$strong("first"), "click the ",
                           tags$strong("plot button"),
                           "and",
                           tags$strong("then"), "click the ",
                           tags$strong("validate button"),".")),
                
                h4(tags$li("If you want to generate a new plot with the same slider values, just click the",
                           tags$strong("plot button"),
                           "again.")),
                
                div(style = "text-align: center",bsButton("explore", "Explore", icon("bolt"), size = "large")),
                br(),
                h3(tags$b("Acknowledgements:")),
                h4("This app was developed and coded by Jinglin Feng. Special thanks to Alex Chen for being my partner in this project.")
                
        ),
        
        #Define the content contained within part 1 ie. tabname "first"
        tabItem(tabName = "first",
                div(style="display: inline-block;vertical-align:top;",
                    tags$a(href='https://shinyapps.science.psu.edu/',tags$img(src='homebut.PNG', width = 15))
                ),
                fluidRow(
                  withMathJax(),
                  column(4,
                         h3("Introduction:"),
                         box(width ="10.5%",background = "maroon",
                             "A researcher is about to look at many explanatory 
                     variables and pick the one X that predicts Y the best. 
                     The sliders below allow you to set the number of explanatory 
                     variables, the sample size, and the population correlation between the 
                     explanatory variables and the response variable. Later, the 
                     researcher will run a validation study with new, independent
                     observations for X."),
                     
                     # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #000000}")),
                     # tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #000000}")),
                     # tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #000000}")),
                     
                     sliderInput("n", "Sample Size:", min = 2, max = 50, value = 5 ,
                                 step = 1),
                     bsPopover("n", "", "Number of Observations", place="right",options = list(container = "body")),
                     
                     sliderInput("p", "True Population Correlation:", min = -0.9, max = 0.9 , value = 0,
                                 step = 0.01),
                     
                     bsPopover("p", "", "Move the slider to change true population correlation", place="right",options = list(container = "body")),
                     
                     sliderInput("k", "The Number of Variables:", min = 1, max = 100 , value = 100 ,
                                 step = 1),
                     bsPopover("k", "", "Move the slider to change the number of explanatory variables you are choosing from", place="right",options = list(container = "body")),
                     
                     actionButton("plot", h5(tags$strong("Click to plot a new dataset"))), 
                     bsPopover("plot", "", "The scatterplot on the left shows the relationship between the best picked X and Y.  The box plot on the right summarizes the distribution of the residuals when you predict Y from the best picked X.", place="right",options = list(container = "body")),
                     br(),
                     br(),
                     conditionalPanel("input.plot != 0",
                                      actionButton("validate", h5(tags$strong("Click here later to Validate"))))),
                  bsPopover("validate", "", "Click to show a scatterplot of Y versus X for the new data used to validate the relationship and a box plot of the distribution of residuals from a validation experiment with new observations of X and Y.", place="right",options = list(container = "body")),
                  
                  h3("Plot:"),
                  column(4,   
                         align="center",
                         plotOutput("scatter", height=360),
                         tableOutput("choose"),
                         bsPopover("choose", "", "Compare the sample best Correlation to the validation set correlation (in absolute terms). ", place="bottom",options = list(container = "body")),
                         plotOutput("scatter2",height = 360)
                         
                  ),
                  
                  column(4,
                         br(),
                         br(),
                         br(),
                         plotOutput("plott"),
                         
                         br(),
                         conditionalPanel("input.validate != 0",
                                          
                                          
                                          h3("Challenge:"),
                                          
                                          
                                          h4("How does the overfitting effect depend on 
                                     sample sizes, true population correlation and 
                                     the number of variables you are choosing from ?")
                                     
                         ))
                  
                  
                )
                
                
                
        )
      )
    )
  ))



server <-(function(input, output,session) {
  #Go to overview Button
  observeEvent(input$goover, {
    updateTabItems(session, "tabs", "over")
  })
  #Explore Button
  observeEvent(input$explore, {
    updateTabItems(session, "tabs", "first")
  })
  
  plotdata<-reactive({
    n=input$n
    p=input$p
    k=input$k
    xmat=matrix(0,n,k)
    
    if(input$plot>0){
      y1<-rnorm(n,0,1)
      if(p>0){
        x1=rnorm(n,y1,1/abs(p))
        x2=rnorm(n,y1,1/abs(p))
      }
      
      else if(p<0){
        x1=-rnorm(n,y1,1/abs(p))
        x2=-rnorm(n,y1,1/abs(p))
      }
      
      else{
        x1=rnorm(n,0,1 )
        x2=rnorm(n,0,1 )
      }
      
      if(p>0){
        for(i in 1:k)
          xmat[,i]=rnorm(n,y1,1/abs(p))
      }
      
      else if(p<0){
        for(i in 1:k)
          xmat[,i]=-rnorm(n,y1,1/abs(p))
      }
      
      else{
        for(i in 1:k)
          xmat[,i]=rnorm(n,0,1)
      }
      
      R2=0
      for(i in 1:k)
        R2[i]=cor( y1,xmat[,i])
      
      data<-list(R2,y1,xmat)
    }
    data
  })
  
  bluedata<-reactive({
    n=input$n
    k=input$k
    mydata<-plotdata()
    y1blue<-bluey()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmatblue<-array(unlist(mydata[3]),dim=c(n,k))
    
    kk2=sample(1:k, 1, replace=TRUE)
    mm2blue<-lm(y1blue~xmatblue[,kk2]) # Randomly Chosen X
    
    data<-list(xmatblue,kk2,y1blue,mm2blue)
    data
  })
  
  bluey<-reactive({
    n=input$n
    if(input$validate>0)
      y<-rnorm(n,0,1)
    y
  })
  
  plot2<-renderPlot({
    n=input$n
    k=input$k
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmat<-array(unlist(mydata[3]),dim=c(n,k))
    kk=which.max(abs(R2)) #best
    
    mm<-lm(y2~xmat[,kk]) # Best Chosen X
    
    d2<-density(y2-mm$fitted.values) # Pick the best X
    plot(range(d2$x), range(d2$y), type = "n", xlab = "Residual",
         ylab = "Density", main="",font.lab=2)
    group <- "Best Chosen X" 
    boxplot(y2-mm$fitted.values,xlab=group, ylim = c(-4, 4), ylab="Residuals",font.lab=2)
    #lines(d2, col="black",lwd=2)
  })
  
  plot1<-renderPlot({
    n=input$n
    k=input$k
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmat<-array(unlist(mydata[3]),dim=c(n,k))
    kk=which.max(abs(R2))
    mm<-lm(y2~xmat[,kk]) # Best Chosen X
    
    d2<-density(y2-mm$fitted.values) # Pick the best X
    
    y1blue<-bluey()
    kkblue=which.max(abs(R2))
    xmatblue=array(unlist(mydata[3]),dim=c(n,k)) 
    mm2blue<-lm(y1blue~xmatblue[, sample(1:k, 1, replace=FALSE)]) # Randomly Chosen X
    d1<-density(y1blue-mm2blue$fitted.values) # Randomly Chosen X)
    plot(range(d1$x,d2$x), range(d1$y,d2$y), type = "n", xlab = "Residual",
         ylab = "Density", main="",font.lab=2)
    groups <- c("Best\nChosen X", "Validation\nData Set") 
    boxplot(y2-mm$fitted.values,y1blue-mm2blue$fitted.values, 
            names=groups, ylab="Residuals",ylim = c(-4, 4),font.lab=2,border=c("black", "blue"))
    #lines(d2, col="black",lwd=2)
    #lines(d1, col="blue",lwd=2)
  })
  
  
  scatterplot<-renderPlot({
    n=input$n
    k=input$k
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmat<-array(unlist(mydata[3]),dim=c(n,k))
    
    kk=which.max(abs(R2))
    mm<-lm(y2~xmat[,kk]) # Best Chosen X
    
    plot(xmat[,kk], y2, xlab="Best Chosen X", ylab="Y",font.lab=2, cex=1.5)
    abline(mm,col="red")
  })
  
  scatterplot2<-renderPlot({
    n=input$n
    k=input$k
    mydata<-bluedata()
    xmatblue<-array(unlist(mydata[1]),dim=c(n,k))
    kk2<-unlist(mydata[2])
    y1blue<-unlist(mydata[3])
    mm2blue<-lm(y1blue~xmatblue[,kk2])
    plot(xmatblue[,kk2], y1blue, xlab="Validation set X", ylab="Y",font.lab=2, col="blue",cex=1.5)
    abline(mm2blue,col="red")
  })
  
  observeEvent(input$plot,output$plott<-plot2)
  observeEvent(input$plot,output$scatter<-scatterplot)
  observeEvent(input$plot,output$choose<-value1)
  observeEvent(input$plot,output$scatter2<-renderPlot({NULL}))
  
  
  observeEvent(input$validate,output$plott<-plot1)
  observeEvent(input$validate,output$choose<-value2)
  observeEvent(input$validate,output$scatter2<-scatterplot2)
  
  
  value11<-reactive({
    n=input$n
    k=input$k
    mydata<-plotdata()
    R2<-unlist(mydata[1])
    y2<-unlist(mydata[2])
    xmat<-array(unlist(mydata[3]),dim=c(n,k))
    kk=which.max(abs(R2)) #best
    best<-cor(y2,xmat[, kk])
    xx=as.data.frame(best)
    colnames(xx)=c("Sample Best Correlation")
    xx
  })
  value1<-renderTable({
    value11()},
    align="c"
  )
  value22<-reactive({
    n=input$n
    k=input$k
    best<-value11()
    mydata<-bluedata()
    xmatblue<-array(unlist(mydata[1]),dim=c(n,k))
    kk2<-unlist(mydata[2])
    y1blue<-unlist(mydata[3])
    random<-cor(y1blue,xmatblue[,kk2])
    xx=cbind(best,random)
    xx=as.data.frame(xx)
    colnames(xx)=c("Sample Best Correlation","Sample Validation Set Correlation")
    xx
  })
  value2<-renderTable({
    value22()},
    align="c"
  )
  
  
  
})


boastUtils::boastApp(ui = ui, server = server)



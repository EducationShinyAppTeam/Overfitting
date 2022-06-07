library(shiny)

shinyServer(function(input, output,session) {
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
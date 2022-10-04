# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(shinyWidgets)


# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "black",
    ## Create the app header ----
    dashboardHeader(
      title = "Overfitting",
      titleWidth = 250, 
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown", 
              boastUtils::surveyLink(name = "Overfitting")),
      tags$li(class = "dropdown", 
              tags$a(href = 'https://shinyapps.science.psu.edu/'
                                         , icon("house")))),
    ## Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview",tabName = "over",icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "pre", icon = icon("book")),
        menuItem("Explore", tabName = "first", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    
    # Create the body ----
    dashboardBody(
      ## Set up the Overview Page ----
      tabItems(
        tabItem(tabName = "over",
                h1("Overfitting"),
                p("This app explores how you can become overconfident
                  when you are choosing the best explanatory variable from 
                  many choices.  This is just one of several different types
                  of overfitting that can occur."),
                h2("Instructions"),
                tags$ol(
                  tags$li("Click on the prerequisites button to review/learn the concepts
                    of overfitting."),
                  tags$li("click the go button to enter the prerequisite page.")),
                div(
                  style = "text-align: center",
                  bsButton(
                    inputId = "explore", 
                    label = "Go!", 
                    icon = icon("bolt"), 
                    size = "large", 
                    style = "default"
                  )
                ),
                br(),
                h2("Acknowledgements"),
                p("This app was developed and coded by Jinglin Feng and modified
                  by Adam Poleski(2021) and Junjie He(2022)."),
                br(),
                br(),
                "Cite this app as:",
                br(),
                boastUtils::citeApp(),
                br(),
                br(),
                div(class = "updated", "Last Update: 9/30/2022 by Junjie He.")
        ),
        
        ## Set up the Prerequisites Page ----
        tabItem(
          tabName = "pre",
          withMathJax(),
          h2("Understanding the Overfitting Effect"),
          p("A researcher looks at many explanatory variables and picks the one 
            that predicts Y the best. But if we draw another sample randomly from the same model, it will not fit nearly as well."),
          p("Overfitting is likely to occur when we have too many parameters to 
            estimate compared to the available sample size. For example, Overfitting 
            may occur"),
          tags$ul(
            tags$li("when a high dimensional polynomial model is being fit;"),
            tags$li("when too many explanatory variables are in the model; or"),
            tags$li("when just one or two variables are in the model 
                          chosen from a large number of possible explanatory 
                          variables (this is the topic of this app)"),),
          p("However, more sample size would prevent overfitting; and too many 
              variables might lead to the overfitting some of variables retained 
              in the model are actually noise variables, the model cannot be validated
              in future dataset."),
          br(),
        ),
        
        ## Set up an Explore Page----
        tabItem(
          tabName = "first",
          h2("Explore Overfitting:"),
          tags$ul(tags$li("A researcher is about to look at many explanatory 
                     variables and pick the one X that predicts Y the best."),
                  tags$li("Use the sliders to adjust sample size, population correlation 
                     between the explanatory variables and the response variable 
                     and the number of explanatory variables to check the plots.")),
          br(),
          fluidRow(
            column(
              width=4,
              wellPanel(
                sliderInput(
                  inputId = "n", 
                  label = "Sample Size:", 
                  min = 2, 
                  max = 50, 
                  value = 2,
                  step = 1
                ),
                br(),
                sliderInput(
                  inputId = "p", 
                  label = "True Population Correlation:", 
                  min = -0.9, 
                  max = 0.9 , 
                  value = -0.9,
                  step = 0.01
                ),
                br(),
                sliderInput(
                  inputId = "k", 
                  label = "The Number of Variables:", 
                  min = 1, 
                  max = 100 ,
                  value = 1 ,
                  step = 1
                ),
                br(),
                bsButton(
                  inputId = "plot", 
                  label = "Show plots for new sample",
                  icon = icon("retweet"),
                  size = "large"
                ), 
                br(),
                br(),
                bsButton(
                  inputId = "validate", 
                  label = "Show plots for validation data",
                  icon = icon("retweet"),
                  size = "large"
                )
              )
            ),
            h3("Plot:"),
            column(
              width=4,
              plotOutput("scatter"),
              plotOutput("scatter2"),
            ),
            column(
              width=4,
              plotOutput("plott"),
            )
          )
        ),
        
        ## Set up the References Page ----
        tabItem(
          tabName = "references",
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
          (v0.61). [R package]. Available from
          https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2020). boastUtils: BOAST Utilities. 
          (v. 0.1.10.2), [R package] Available from 
          https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          
          p(
            class = "hangingindent",
            "Chang, W. and Borges, Ribeiro B. (2017). 
          shinydashboard: Create Dashboards with ‘Shiny’. R package version 0.6.1"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y and Mcpherson, J. (2017). 
          shiny: Web Application Framework for R. R package version 1.0.3"
          ),
          p(
            class = "hangingindent",
            " Perrier, V., Meyer, F., and Granjon, D. (2018). 
            shinyWidgets: Custom Inputs Widgets for Shiny. R package version 0.4.3."
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  ))


# Define server logic ----
server <- (function(input, output,session) {
  
  ##Go Button----
  observeEvent(input$explore, {
    updateTabItems(session, "pages", "pre")
  })
  
  ##data used in plot----
  plotdata <- reactive({
    n = input$n
    p = input$p
    k = input$k
    xmat = matrix(0,n,k)
    if (input$plot > 0) {
      y1 <- rnorm(n,0,1)
      if (p > 0) {
        x1 = rnorm(n,y1,1/abs(p))
        x2 = rnorm(n,y1,1/abs(p))
      }
      else if (p < 0) {
        x1 = -rnorm(n,y1,1/abs(p))
        x2 = -rnorm(n,y1,1/abs(p))
      }
      else{
        x1 = rnorm(n,0,1 )
        x2 = rnorm(n,0,1 )
      }
      if (p > 0) {
        for (i in 1:k)
          xmat[,i] = rnorm(n,y1,1/abs(p))
      }
      else if (p < 0) {
        for (i in 1:k)
          xmat[,i] = -rnorm(n,y1,1/abs(p))
      }
      else{
        for (i in 1:k)
          xmat[,i] = rnorm(n,0,1)
      }
      R2 = 0
      for (i in 1:k)
        R2[i] = cor( y1,xmat[,i])
      data <- list(R2,y1,xmat)
    }
    data
  })
  
  bluedata <- reactive({
    n = input$n
    k = input$k
    mydata <- plotdata()
    y1blue <- bluey()
    R2 <- unlist(mydata[1])
    y2 <- unlist(mydata[2])
    xmatblue <- array(unlist(mydata[3]),dim = c(n,k))
    kk2 = sample(1:k, 1, replace = TRUE)
    mm2blue <- lm(y1blue~xmatblue[,kk2]) # Randomly Chosen X
    data <- list(xmatblue,kk2,y1blue,mm2blue)
    data
  })
  
  bluey <- reactive({
    n = input$n
    if (input$validate > 0)
      y <- rnorm(n,0,1)
    y
  })
  
  plot2 <- renderPlot({
    n = input$n
    k = input$k
    mydata <- plotdata()
    R2 <- unlist(mydata[1])
    y2 <- unlist(mydata[2])
    xmat <- array(unlist(mydata[3]),dim = c(n,k))
    kk = which.max(abs(R2)) #best
    mm <- lm(y2~xmat[,kk]) # Best Chosen X
    d2 <- density(y2 - mm$fitted.values) # Pick the best X
    plot(range(d2$x), range(d2$y), type = "n", xlab = "Residual",
         ylab = "Density", main = "",font.lab = 2)
    group <- "Best Chosen X"
    boxplot(y2 - mm$fitted.values,xlab = group, ylim = c(-4, 4),
            ylab = "Residuals",font.lab = 2)
  })
  
  plot1 <- renderPlot({
    n = input$n
    k = input$k
    mydata <- plotdata()
    R2 <- unlist(mydata[1])
    y2 <- unlist(mydata[2])
    xmat <- array(unlist(mydata[3]),dim = c(n,k))
    kk = which.max(abs(R2))
    mm <- lm(y2~xmat[,kk]) # Best Chosen X
    d2 <- density(y2 - mm$fitted.values) # Pick the best X
    y1blue <- bluey()
    kkblue = which.max(abs(R2))
    xmatblue = array(unlist(mydata[3]),dim = c(n,k))
    mm2blue <- lm(y1blue~xmatblue[, sample(1:k, 1, replace=FALSE)])
    d1 <- density(y1blue - mm2blue$fitted.values) # Randomly Chosen X)
    plot(range(d1$x,d2$x), range(d1$y,d2$y), type = "n", xlab = "Residual",
         ylab = "Density", main = "",font.lab = 2)
    groups <- c("Best\nChosen X", "Validation\nData Set")
    boxplot(y2 - mm$fitted.values,y1blue - mm2blue$fitted.values,
            names = groups, ylab = "Residuals",ylim = c(-4, 4),font.lab = 2,
            border = c("black", "blue"))
  }
  )
  
  scatterplot <- renderPlot({
    n = input$n
    k = input$k
    mydata <- plotdata()
    R2 <- unlist(mydata[1])
    y2 <- unlist(mydata[2])
    xmat <- array(unlist(mydata[3]),dim = c(n,k))

    kk = which.max(abs(R2))
    mm <- lm(y2~xmat[,kk]) # Best Chosen X

    plot(xmat[,kk], y2, xlab = "Best Chosen X", ylab = "Y",font.lab = 2, cex = 1.5)
    abline(mm,col = "red")
  })
  
  scatterplot2 <- renderPlot({
    n = input$n
    k = input$k
    mydata <- bluedata()
    xmatblue <- array(unlist(mydata[1]),dim = c(n,k))
    kk2 <- unlist(mydata[2])
    y1blue <- unlist(mydata[3])
    mm2blue <- lm(y1blue~xmatblue[,kk2])
    plot(xmatblue[,kk2], y1blue, xlab = "Validation set X", ylab = "Y",
         font.lab = 2, col = "blue",cex = 1.5)
    abline(mm2blue,col = "red")
  })
  
  observeEvent(input$plot,output$plott <- plot2)
  observeEvent(input$plot,output$scatter <- scatterplot)
  observeEvent(input$plot,output$scatter2 <- renderPlot({NULL}))
  observeEvent(input$validate,output$plott <- plot1)
  observeEvent(input$validate,output$scatter2 <- scatterplot2)
  
})

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
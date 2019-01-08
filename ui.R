library(shiny)
library(shinydashboard)
library(shinyBS)
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
)

              
                  


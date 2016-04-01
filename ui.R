fluidPage(
  # Application title
  titlePanel("Mining with Hillary"),
  # sidebar description
  br(),
  fluidRow(
    column(11,
           h4("A controversy arose in March 2015, when it became publicly known that Hillary Clinton"),
           h4("exclusively used her family's private email server for her official email communications."),
           br()
    )
  ),
    
  sidebarPanel(
      textInput("text", label = h3("Look up"), 
                value = "enter a keyword..."),
      submitButton("Sumbit")
  ),
      mainPanel(
        tabsetPanel(
          tabPanel("Word Cloud", plotOutput("plot")),
          tabPanel("Frequency Table", dataTableOutput(outputId= "table")),
          tabPanel("Histogram", plotOutput("histogram"))
  
          ))
      
)
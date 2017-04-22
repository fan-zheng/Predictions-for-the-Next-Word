library(shiny)
shinyUI(fluidPage(
       titlePanel("Predictions for the Next Word"),
       sidebarLayout(
              sidebarPanel(
                     helpText("Please input words, select the number of words to be predicted and the discount value for Katz Backoff Method"),
                     textInput("str",label = h3("Enter some text:"),value = ""),
                     numericInput("n",label = h3("Number of words to be predicted:"),min = 1,max = 50,value = 5,step = 1),
                     numericInput("discount",label = h3("Discount value for Katz Backoff Method"),min = 0.1,max = 0.9,value = 0.5,step = 0.1)
                  ),
               mainPanel(
                          h2("Predictions from Kneser-Ney Method:(most likely to least likely)"),
                          h4(textOutput("predict",container = pre)),
                          h2('Predictions from Katz Backoff Method: (most likely to least likely)'),
                          h4(textOutput("backoff_output",container = pre))
                          #verbatimTextOutput("backoff_output")
                                          )
                                                             
                                                             
                                    )
               )
         )
  
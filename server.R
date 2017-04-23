require(shiny)
require(stringr)
load("combined_ngrams.RData")
source("getnextwords.R",local = TRUE)
shinyServer(function(input, output) {
         output$predict <- renderText({
                if(input$str=="")
                       return("You haven't entered anything yet!")
                predictions <- getnextword(input$str,input$n)
                paste0("\n",1:input$n,".",predictions,"\n")
           })
         output$backoff_output <- renderText({
           if(input$str=="")
             return("You haven't entered anything yet!")
           predictions1 <- fun(input$str,input$discount,input$n)
           paste0("\n",1:input$n,".",predictions1,"\n")
           
         })
         
         
         
         
         
         })
server <- function(input, output) {
        output$newIndex <- renderText( { 
                t <- 9/5 * input$tempc + 32
                rh <- input$rhum
                indexf <- round((-42.379 + (2.04901523*t) + (10.14333127*rh) + 
                                         (-.22475541*t*rh) + (-6.83783*10^-3*t^2) +
                                         (-5.481717*10^-2*rh^2) + (1.22874*10^-3*t^2*rh) +
                                         (8.5282*10^-4*t*rh^2) + (-1.99*10^-6*t^2*rh^2)),0)
                indexc <- round(((indexf - 32) * 5/9), 0)
                ifelse(indexc < 60, paste("At ", input$tempc, 
                                          " degrees Centigrade with ", input$rhum, 
                                          " percent relative humidity, the heat index is ", indexc, "."), 
                       paste("At ", input$tempc, 
                             " degrees Centigrade with ", input$rhum, 
                             " percent relative humidity, heat stroke is imminent.
                      Don't waste these last few precious moments of your life
                      worrying about the stupid heat index."))
        })
        output$newRX <- renderText({
                t <- 9/5 * input$tempc + 32
                rh <- input$rhum
                indexf <- round((-42.379 + (2.04901523*t) + (10.14333127*rh) + 
                                         (-.22475541*t*rh) + (-6.83783*10^-3*t^2) +
                                         (-5.481717*10^-2*rh^2) + (1.22874*10^-3*t^2*rh) +
                                         (8.5282*10^-4*t*rh^2) + (-1.99*10^-6*t^2*rh^2)),0)
                indexc <- round(((indexf - 32) * 5/9), 0)
                paste("Recommended Fluid Intake Compensation:")
                ifelse(indexc < 39, rx1, 
                       ifelse(indexc >= 39 & indexc < 50, rx2, 
                              ifelse(indexc >= 50 & indexc < 60 , rx3, rx4)))
        })
}

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(lubridate)
library(shiny)

ui <- fluidPage(
    hr(),
    actionButton('start','Start'),
    actionButton('stop','Stop'),
    actionButton('reset','Reset'),
    numericInput('seconds','Seconds:',value=10,min=0,max=99999,step=1),
    textOutput('timeleft'),
    textInput('text','input'),
    actionButton('btn','ok')
    
)

server <- function(input, output, session) {
    
    # Initialize the timer, 10 seconds, not active.
    timer <- reactiveVal(10)
    active <- reactiveVal(FALSE)
    
    # Output the time left.
    output$timeleft <- renderText({
        paste("Time left: ", seconds_to_period(timer()))
    })
    
    # observer that invalidates every second. If timer is active, decrease by one.
    observe({
        invalidateLater(1000, session)
        isolate({
            if(active())
            {
                timer(timer()-1)
                if(timer()<1)
                {
                    active(FALSE)
                    showModal(modalDialog(
                        title = "Important message",
                        "Countdown completed!"
                    ))
                }
            }
        })
    })
    
    # observers for actionbuttons
    observeEvent(input$start, {active(TRUE)})
    observeEvent(input$stop, {active(FALSE)})
    observeEvent(input$reset, {timer(input$seconds)})
    observeEvent(input$btn,{
        updateTextInput(session,'text',value = paste0(input$text,"A"))
    })
    
}

shinyApp(ui, server)

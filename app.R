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
library(tsdo)

ui <- fluidPage(
    hr(),
    actionButton('start','开始'),
    actionButton('stop','结束'),
    
    numericInput('seconds','超时设置:',value=15,min=0,max=99999,step=1),
    textOutput('timeleft'),
    htmlOutput('timeleft2'),
    textInput('text','自回复内容'),
    actionButton('btn','ok')
    
)

server <- function(input, output, session) {
    
    # Initialize the timer, 10 seconds, not active.
   # intial_sec <- reactive({input$seconds})
    timer <- reactiveVal(15)
    active <- reactiveVal(FALSE)
    
    # Output the time left.
    output$timeleft <- renderText({
        paste("已用时: ", seconds_as_Timetext(input$seconds-timer()),"倒计时: ", seconds_as_Timetext(timer()))
    })
    
   
    
    output$timeleft2 <- renderText({ 
        
        if(timer() >6){
            NULL
        }else{
            paste("最后倒计时","<font color=\"#FF0000\"><b>", seconds_as_Timetext(timer()), "</b></font>") 
            
        }
       
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
                    updateTextInput(session,'text',value = '超时自动回复内容测试')
                    showModal(modalDialog(
                        title = "超时提醒",
                        "时间已到", footer = column(shiny::modalButton('确认'),
                                       
                                       width=12)
                    ))
                }
            }
        })
    })
    
    # observers for actionbuttons
    observeEvent(input$start, {
        timer(input$seconds)
        active(TRUE)
        })
    observeEvent(input$stop, {active(FALSE)})
    # observeEvent(input$reset, {timer(input$seconds)})
    # observeEvent(input$end,{
    #     active(FALSE)
    #     timer(input$seconds)
    #     
    # })
    observeEvent(input$btn,{
        updateTextInput(session,'text',value = paste0(input$text,"A"))
    })
    
}

shinyApp(ui, server)

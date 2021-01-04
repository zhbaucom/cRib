
library(shiny)
library(tidyverse)
library(DT)

# source("functions/cRibFun.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cribbage"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
                selectizeInput("hand", "Choose 6", choices = deck, multiple = TRUE, options = list(maxItems = 6L)),
                selectInput("speed", "Speed or Info?", choices = c("speed", "info")),
                actionButton("calc", "Calculate"),
                br(),
                br(),
                uiOutput("flop1")
                ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("tableOut")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$flop1 <- renderUI({
        selectizeInput("flop", "Flop", selected = NULL, choices = deck[!(deck %in% input$hand)], multiple = T, options = list(maxItems = 1L))
    })
    
    
    CARDS <- eventReactive(input$calc, {
        
        handComb <- combn(input$hand, 4)
        
        if(input$speed == "speed"){
            nh <- Number(input$hand)
            su <- Suit(input$hand)
            unum <- unique(nh)
            d2 <- sapply(unum, function(x){
                us <- unique(su[nh %in% x])
                if(length(suit[!(suit %in% us)]) == 0){
                    NA
                }else{
                    paste(x,suit[!(suit %in% us)][1], sep = "")
                }
            })
            d2 <- d2[!is.na(d2)]
            d3 <- append(d2, paste(value[!(value %in% unum)], "H", sep = ""))
            Avgs <- apply(handComb, 2, function(x){
                Avg <- map_dbl(d3, function(y){
                    cc <- CribCount(kh = x, flop = y, DEALER = FALSE, NOBBINS = FALSE, FLUSH = FALSE)
                    if(any(nh %in% Number(y))){
                        cc * (4- sum(nh == Number(y)))/46
                    }else{
                        cc * 4/46
                    }
                    
                }) %>%
                    sum()
                
                if(any(grepl("J", x))){
                    Avg <- Avg + sum(Suit(deck[!(deck %in% input$hand)]) %in% Suit(x[grepl("J", x)]))/46
                }
                
                if(any(table(Suit(x)) == 4)){
                    Avg <- Avg + 4 + sum(Suit(deck[!(deck %in% input$hand)]) %in% Suit(x[1]))/46
                }
                Avg
            })
            list(
                handComb = combn(input$hand, 4),
                allOut = Avgs
            )
        }else{
            list(
                handComb = combn(input$hand, 4),
                allOut = apply(handComb, 2, function(x){
                    map_dbl(deck[!(deck %in% input$hand)], ~CribCount(kh = x, flop = .x, DEALER = FALSE))
                })
            )
        }
        
        
        


        
        


    })
    
    
    output$tableOut <- renderDataTable({
        if(!is.null(CARDS)){
            HANDS <- data.frame(t(CARDS()$handComb))
            colnames(HANDS) <- paste("Card", 1:4)
            if(input$speed == "speed"){
                HANDS$Avg <- round(CARDS()$allOut, 4)
            }else{
                
    
                    HANDS$Avg <- apply(CARDS()$allOut, 2, mean) %>% round(4)
                    HANDS$Med <- apply(CARDS()$allOut, 2, median)
                    HANDS$Min <- apply(CARDS()$allOut, 2, min)
                    HANDS$Max <- apply(CARDS()$allOut, 2, max)
                    HANDS$`25th Perc` <- apply(CARDS()$allOut, 2, quantile, .25)
                    HANDS$`75th Perc` <- apply(CARDS()$allOut, 2, quantile, .75)
                    
    
            }
            
            if(!is.null(input$flop)){
                HANDS$Hand <- apply(CARDS()$handComb, 2, CribCount, flop = input$flop, DEALER = FALSE)
            }
            
            HANDS %>%
                arrange(desc(Avg)) %>%
                datatable(options = list(pageLength = 15, autoWidth = TRUE), rownames= FALSE)

    }
    })


}

# Run the application 
shinyApp(ui = ui, server = server)

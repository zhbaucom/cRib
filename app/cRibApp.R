
library(shiny)
library(rsconnect)
library(tidyverse)
library(DT)
library(kableExtra)

library(tidyverse)
# source("cRibFun.R")


ui <- fluidPage(
    
    # Application title
    titlePanel("Cribbage"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("hs", "Hand Size", choices = 5:6, selected = 6),
            uiOutput("hs2"),
            selectInput("speed", "Speed or Info?", choices = c("info", "speed")),
            actionButton("calc", "Calculate"),
            br(),
            br(),
            uiOutput("flop1")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Best Hand",
                    tableOutput("tableOut"),
                    verbatimTextOutput("ac")
                ),
                tabPanel("Hand Possiblities",
                    uiOutput("hout"),
                    tableOutput("allP")
                )
            )
            
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hs2 <- renderUI({
        if(input$hs == 6){
            selectizeInput("hand", "Choose 6", choices = deck, multiple = TRUE, options = list(maxItems = 6L))
        }else if(input$hs == 5){
            selectizeInput("hand", "Choose 5", choices = deck, multiple = TRUE, options = list(maxItems = 5L))
        }
    })
    
    output$flop1 <- renderUI({
        selectizeInput("flop", "The Cut", selected = NULL, choices = deck[!(deck %in% input$hand)], multiple = T, options = list(maxItems = 1L))
    })
    
    output$hout <- renderUI({
        if(!is.null(CARDS()))
            selectInput("selHand", "Select Hand", choices = CARDS()$tab1$Hand)
    })
    
    

    
    
    
    
    
    CARDS <- eventReactive(input$calc, {
        req(length(input$hand) == input$hs)
        
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
                    Avg <- Avg + sum(Suit(deck[!(deck %in% input$hand)]) %in% Suit(x[grepl("J", x)]))/(52-input$hs)
                }
                
                if(any(table(Suit(x)) == 4)){
                    Avg <- Avg + 4 + sum(Suit(deck[!(deck %in% input$hand)]) %in% Suit(x[1]))/(52-input$hs)
                }
                Avg
            })
            list(
                handComb = combn(input$hand, 4),
                allOut = Avgs
            )
        }else{

            allOut = apply(handComb, 2, function(x){
                map_dbl(deck[!(deck %in% input$hand)], ~CribCount(kh = x, flop = .x, DEALER = FALSE))
            })  %>%
                as.data.frame()%>%
                cbind(deck[!(deck %in% input$hand)])
            colnames(allOut) <- c(apply(handComb, 2, paste, collapse = " "), "Cut")
            
            tab2 <- allOut %>%
                gather("Hand", "Value", as.symbol(colnames(allOut)[1]):as.symbol(colnames(allOut)[ncol(allOut)-1]))
            tab1 <- tab2 %>%
                group_by(Hand) %>%
                nest() %>%
                mutate(
                    Avg = round(map_dbl(data, ~mean(.x$Value)), 4),
                    Hist = "",
                    Min = map_dbl(data, ~quantile(.x$Value, 0)),
                    `25th` = map_dbl(data, ~quantile(.x$Value, .25)),
                    Med = map_dbl(data, ~quantile(.x$Value, .5)),
                    `75th` = map_dbl(data, ~quantile(.x$Value, .75)),
                    Max = map_dbl(data, ~quantile(.x$Value, 1))
                ) %>%
                arrange(desc(Avg), desc(Max), desc(Min))
            
            
            list(tab1 = tab1, tab2 = tab2, allOut = allOut)
            

        }
        
        
        
        
        
        
        
        
        
    })
    
    chsi <- eventReactive(input$calc, {
        list(SE = input$speed, ac = input$calc)
    })
    
    output$tableOut <- function(){

        validate(
            need(chsi()$SE == input$speed | input$calc == 0, "Must rerun for desired info...")
        )
        validate(
            need(length(input$hand) == input$hs, "Your hand isn't complete...")
        )
        if(!is.null(CARDS)){
            
            if(input$speed == "speed"){
                HANDS <- data.frame(Hand = apply(CARDS()$handComb, 2, paste, collapse = " "))
                HANDS$Avg <- round(CARDS()$allOut, 4)
                HANDS <- HANDS %>%
                    arrange(desc(Avg))
                if(!is.null(input$flop)){
                    HANDS$Points <- apply(CARDS()$handComb, 2, CribCount, flop = input$flop, DEALER = FALSE)
                }
                HANDS <-  HANDS %>%
                    kable("html") %>%
                    kable_styling("striped", full_width = T)
            }else{
                if(!is.null(input$flop)){
                    HANDS <- CARDS()$tab1 %>%
                        mutate(cut = map_dbl(data, function(x){
                            x$Value[x$cut == input$flop]
                        }))
                }else HANDS <- CARDS()$tab1

                HANDS <- HANDS %>%
                    select(-data) %>%
                    kable("html") %>%
                    kable_styling("striped", full_width = T)%>%
                    column_spec(3, image = spec_hist(map(HANDS$data, ~.x$Value), res = 200))



            }



            HANDS

        }
    }
    
    
    
    output$allP <- function(){
        
        if(input$speed == "info"){
            CARDS()$tab2 %>%
                filter(Hand %in% input$selHand) %>%
                mutate(garb = TRUE, 
                       cutyn = ifelse(garb == is.null(input$flop), FALSE, Cut == input$flop)) %>%
                arrange(desc(Value)) %>%
                mutate(rrs = rank(-Value, ties.method = "min"))%>%
                group_by(rrs) %>%
                mutate(rrs2 = ifelse(length(rrs) > 1, paste("T", rrs, sep = ""), rrs), Rank = paste(rrs2, "out of", nrow(.))) %>%
                ungroup(rrs) %>%
                arrange(desc(cutyn)) %>%
                select(-Hand, -garb, -cutyn, -rrs, -rrs2) %>%
                kable("html") %>%
                kable_styling("striped", full_width = T)
        }else{
            print("Speed or Info must be set to info...")
        }

    }
    
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

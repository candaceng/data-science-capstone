library(shiny)
library(shinythemes)
library(ggplot2)
library(stringr)
library(tm)
library(NLP)
library(RWeka)

# Define UI for application 
ui <- fluidPage(theme=shinythemes::shinytheme("lumen"),
                
    titlePanel(strong("Text Prediction Using N-gram Models")),
    h5("Candace Ng"),
    h5("August 19, 2020"),
    
    br(), hr(),
    h4(strong("Predictor")),
    fluidRow(
        column(6, align="center",
           textInput("text", h5("Enter Text:"), value=""),
           helpText("See the prediction of the next word on the right")
        ),
        column(1),
        column(4, align="center",
           h5("The next word is predicted to be:"),
           verbatimTextOutput("prediction"),
        )
    ),
    
    br(), hr(), 
    h4(strong("Statistics")),
    h5("Toggle between the tabs to view the most frequently occuring unigrams, bigrams, and trigrams from the text corpus"),
    mainPanel(width="80%", align="center",
        tabsetPanel(
            tabPanel(strong("Unigram"), plotOutput("unigram")),
            tabPanel(strong("Bigram"),  plotOutput("bigram")),
            tabPanel(strong("Trigram"), plotOutput("trigram"))
        )
    )
)

# Define server logic 
server <- function(input, output) {
    unigram <- readRDS("unigram.rds")
    bigram <- readRDS("bigram.rds")
    trigram <- readRDS("trigram.rds")
    
    clean_input <- function(x) {
        xclean <- removeNumbers(removePunctuation(tolower(x)))
        return(strsplit(xclean, " ")[[1]])
    }
    query <- reactive({clean_input(input$text)})
    
    
    output$prediction <- renderText({
        if(length(query()) == 0) { " " }
        else if(length(query()) >= 2 & !(identical(integer(0), which(startsWith(trigram$word, query()))))) { 
            tail(strsplit(trigram$word[which(startsWith(trigram$word, query()))], " ")[[1]], 1)
        }
        else if(length(query()) == 1 & !(identical(integer(0), which(startsWith(bigram$word, query()))))) {
            tail(strsplit(bigram$word[which(startsWith(bigram$word, query()))], " ")[[1]], 1)
        }
        else{ sample(c("and", "is", "the"), 1) }
    })
    
    output$unigram <- renderPlot({
        g1 <- ggplot(unigram[1:20,], aes(x = reorder(word, -freq), y = freq))
        g1 <- g1 + geom_bar(stat = "identity", fill = I("turquoise3"))
        g1 <- g1 + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
        g1 <- g1 + xlab("")
        g1 <- g1 + ylab("Frequency")
        g1 <- g1 + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
                         axis.text.x = element_text(hjust = 1.0, angle = 45),
                         axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
        g1 <- g1 + ggtitle("20 Most Common Unigrams")
        print(g1)
    })
    output$bigram <- renderPlot({
        g2 <- ggplot(bigram[1:20,], aes(x = reorder(word, -freq), y = freq))
        g2 <- g2 + geom_bar(stat = "identity", fill = I("salmon"))
        g2 <- g2 + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
        g2 <- g2 + xlab("")
        g2 <- g2 + ylab("Frequency")
        g2 <- g2 + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
                         axis.text.x = element_text(hjust = 1.0, angle = 45),
                         axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
        g2 <- g2 + ggtitle("20 Most Common Bigrams")
        print(g2)
    })
    output$trigram <- renderPlot({
        g3 <- ggplot(trigram[1:20,], aes(x = reorder(word, -freq), y = freq))
        g3 <- g3 + geom_bar(stat = "identity", fill = I("palegreen3"))
        g3 <- g3 + geom_text(aes(label = freq ), vjust = -0.20, size = 3)
        g3 <- g3 + xlab("")
        g3 <- g3 + ylab("Frequency")
        g3 <- g3 + theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
                         axis.text.x = element_text(hjust = 1.0, angle = 45),
                         axis.text.y = element_text(hjust = 0.5, vjust = 0.5))
        g3 <- g3 + ggtitle("20 Most Common Trigrams")
        print(g3)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

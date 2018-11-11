library(shiny)
library(readr)
library(ggplot2)
library(plotly)
setwd("C:/Users/jerem.DESKTOP-GGM6Q2I/Documents/UNH Data Analytics/r programming 2/final_project")
boardgames <- read_csv("board_games_cleaned.csv")
boardgames <- filter(boardgames, year >= 1950 & year <= 2019)


ui <- fluidPage(
  titlePanel(h1("Board Game Analysis", align = 'center')), 
  navbarPage(
    title = "Interactive Features", 
    id = "conditionedPanels",
    tabPanel(
      "Graph 1", 
      value=1,
      sidebarPanel(
        sliderInput("slider_01", h4("Year"), 1950, 2019, c(1970, 2016)),
        textInput("text_02", "Enter a game name", "Takenoko"),
        plotOutput("graph_02")
      ),
      mainPanel(
        plotlyOutput("graph_01", height = 800)
      )
    ), 
    tabPanel(
      "Graph 2", 
      value=2,
      sidebarPanel(
      ),
      mainPanel(
      )
    ), 
    tabPanel(
      "Board Game Finder", 
      value=3,
      sidebarPanel(
      ),
      mainPanel(
      )
    ), 
    tabPanel(
      "Read Me", 
      value=4,
      mainPanel(
      )
    )
  ),
  h4("test 1", align = "left", condition="input.conditionedPanels==4"),
  h4("test 2", align = 'right', condition="input.conditionedPanels==4")
)


server <- function(input, output) 
{
  boardgames2 <- reactive({filter(boardgames, year >= input$slider_01[1] & year <= input$slider_01[2])})
  output$graph_01 <- renderPlotly({
    p<-ggplot(boardgames2(), aes(x=as.factor(year)))+geom_bar(fill="blue", colour="red")
    p<-p+xlab("Year")+ylab("Count of Board Games")
    p<-p+ggtitle("Count of Board Games by Year")#title
    p<-p+theme_bw()
    p<-p+theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_text(vjust = -1), plot.margin = unit(c(1,1,1,0.5), 'lines'))
    ggplotly(p)
  })
  boardgames3 <- reactive({filter(boardgames, tolower(name) == tolower(input$text_02))})
  output$graph_02 <- renderPlot({
    words1 <- boardgames3()$description
    words1 <- gsub("\n", " ", words1)
    words1 <- gsub("\r", " ", words1)
    words1 <- gsub("<.+>", " ", words1)
    words1<- gsub("@\\w+ *", "", words1)    
    words1 <- gsub('http\\S+\\s*', "", words1)
    words1<-gsub("http\\w+ *", "", words1) 
    words1<-gsub("#", "", words1)     
    words1 <- gsub("[^0-9a-z///' ]", '', words1, ignore.case = TRUE) 
    myCorpus1 <- Corpus(VectorSource(words1))
    myCorpus1 <- tm_map(myCorpus1 , tolower)
    myCorpus1 <- tm_map(myCorpus1, function(x)removeWords(x,stopwords()))
    myCorpus1 <- tm_map(myCorpus1, function(a)removeWords(a,c("boardgame", "will", "one", "board", "game","use", "take", "make", "must", "may", "like", "new", "turn", "try", "also", "along", "onto", "send", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "first", "can", "way", "turn", "turns", "takes", "uses", "player", "players", "already", "could", "should", "would", "every", "end", "beginning", "gives", "gets", "get", "top", "bottom", "needs", "need", "begins", "begin", "ends", "enough", "all")))
    tdm1 <- TermDocumentMatrix(myCorpus1)
    m1 <- as.matrix(tdm1)
    word_freqs1 = sort(rowSums(m1), decreasing=TRUE) 
    dm1 = data.frame(word=names(word_freqs1), freq=word_freqs1) 
    wordcloud(dm1$word, dm1$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), random.color = TRUE, max.words = 150, min.freq = 1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(readr)
library(ggplot2)
library(plotly)
library(rvest)  
library(tm)
library(wordcloud)
library(twitteR)
library(dplyr)
setwd("C:/Users/jerem.DESKTOP-GGM6Q2I/Source_Tree/R fall project")
boardgames <- read_csv("board_games_cleaned.csv")
boardgames <- filter(boardgames, year >= 1950 & year <= 2019)
sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
  div(class = paste0("col-sm-", width), 
      tags$form(class = "well", ...),
      out
  )
}
setwd("C:/Users/jerem.DESKTOP-GGM6Q2I/Source_Tree/R fall project/Board-Game-Project")


ui <- fluidPage(
  div(style="display: inline-block;vertical-align:top;float:left;", img(src = "banner.png", width = 106, height = 67)),
  div(style="display: inline-block;vertical-align:top;float:left;", img(src = "banner.png", width = 106, height = 67)),
  div(style="display: inline-block;vertical-align:top;float:left;", img(src = "banner.png", width = 106, height = 67)),
  div(style="display: inline-block;vertical-align:top;float:left;", img(src = "banner.png", width = 106, height = 67)),
  # div(style="display: inline-block;vertical-align:top;float:right;", h1("Board Game Analysis")),
  div(style="display: inline-block;vertical-align:top;float:right;", img(src = "banner.png", width = 106, height = 67)),
  div(style="display: inline-block;vertical-align:top;float:right;", img(src = "banner.png", width = 106, height = 67)),
  div(style="display: inline-block;vertical-align:top;float:right;", img(src = "banner.png", width = 106, height = 67)),
  div(style="display: inline-block;vertical-align:top;float:right;", img(src = "banner.png", width = 106, height = 67)),
  div(h1("Board Game Analysis", align = 'center')), 
  navbarPage(
    title = "Interactive Features", 
    id = "conditionedPanels",
    tabPanel(
      "Graph 1", 
      value=1,
      sidebarPanel(
        sliderInput("slider_01", h4("Year"), 1950, 2019, c(1970, 2016)),
        textInput("text_02", "Enter a game name", "Takenoko"),
        actionButton("button_01", "Update Changes"),
        h2(""),
        plotOutput("graph_02", height = 510, width = 510)
      ),
      mainPanel(
        plotlyOutput("graph_01", height = 800),
        h3("Insight:  Board Games numbers have been increasing exponentially over time.")
      )
    ), 
    tabPanel(
      "Graph 2", 
      value=2,
      sidebarPanel2(fluid = FALSE,
        selectInput("xlab_03", h4("X Axis"), choices = c("complexity_weight", "rating_average", "users_owned", "users_trading"), selected = "rating_average"),
        selectInput("ylab_03", h4("Y Axis"), choices = c("complexity_weight", "rating_average", "users_owned", "users_trading"), selected = "users_owned"),
        out = htmlOutput("text_03")
      ),
      htmlOutput("text2_03", align = "center"),
      mainPanel(
        plotlyOutput("graph_03", height = 800)
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
      div(img(src="banner.png"))
      )
    )
  ),
  div(style="display: inline-block;vertical-align:top;float:left;", h4("test 1")),
  div(style="display: inline-block;vertical-align:top;float:right;", h4("test 3")),
  div(style="display: inline-block;vertical-align:top;margin:0 auto;left:50%;position:absolute;width:100%;", h4("test 2"))
)


server <- function(input, output) 
{
  boardgames2 <- reactive({filter(boardgames, year >= input$slider_01[1] & year <= input$slider_01[2])})
  output$graph_01 <- renderPlotly({input$button_01
    
    p<-isolate(ggplot(boardgames2(), aes(x=as.factor(year)))+geom_bar(fill="blue", colour="red"))
    p<-isolate(p+xlab("Year")+ylab("Count of Board Games"))
    p<-isolate(p+ggtitle("Count of Board Games by Year"))
    p<-isolate(p+theme_bw())
    p<-isolate(p+theme(panel.grid.major = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x = element_text(vjust = -1), plot.margin = unit(c(1,1,1,0.5), 'lines')))
    ggplotly(p)
  
  })
  boardgames3 <- reactive({filter(boardgames, tolower(name) == tolower(input$text_02))})
  output$graph_02 <- renderPlot({input$button_01
    words1 <- isolate(boardgames3()$description)
    words1 <- isolate(gsub("\n", " ", words1))
    words1 <- isolate(gsub("\r", " ", words1))
    words1 <- isolate(gsub("<.+>", " ", words1))
    words1<- isolate(gsub("@\\w+ *", "", words1))    
    words1 <- isolate(gsub('http\\S+\\s*', "", words1))
    words1<- isolate(gsub("http\\w+ *", "", words1))
    words1<- isolate(gsub("#", "", words1))    
    words1 <- isolate(gsub("[^0-9a-z///' ]", '', words1, ignore.case = TRUE))
    myCorpus1 <- isolate(Corpus(VectorSource(words1)))
    myCorpus1 <- isolate(tm_map(myCorpus1 , tolower))
    myCorpus1 <- isolate(tm_map(myCorpus1, function(x)removeWords(x,stopwords())))
    myCorpus1 <- isolate(tm_map(myCorpus1, function(a)removeWords(a,c("boardgame", "will", "one", "board", "game","use", "take", "make", "must", "may", "like", "new", "turn", "try", "also", "along", "onto", "send", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "first", "can", "way", "turn", "turns", "takes", "uses", "player", "players", "already", "could", "should", "would", "every", "end", "beginning", "gives", "gets", "get", "top", "bottom", "needs", "need", "begins", "begin", "ends", "enough", "all"))))
    tdm1 <- isolate(TermDocumentMatrix(myCorpus1))
    m1 <- isolate(as.matrix(tdm1))
    word_freqs1 = isolate(sort(rowSums(m1), decreasing=TRUE))
    dm1 = isolate(data.frame(word=names(word_freqs1), freq=word_freqs1)) 
    wordcloud(dm1$word, dm1$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), random.color = TRUE, max.words = 150, min.freq = 1, scale = c(4.6,.575))
  })
  data1 <- reactive({
    switch(input$xlab_03,
           complexity_weight = boardgames$complexity_weight,
           rating_average = boardgames$rating_average,
           users_owned = boardgames$users_owned,
           users_trading = boardgames$users_trading
    )
  })
  data2 <- reactive({
    switch(input$ylab_03,
           complexity_weight = boardgames$complexity_weight,
           rating_average = boardgames$rating_average,
           users_owned = boardgames$users_owned,
           users_trading = boardgames$users_trading
    )
  })
  # data3 <- reactive({
  #   switch(input$ylab_03,
  #          complexity_weight = "complexity_weight",
  #          rating_average = "rating_average",
  #          users_owned = "users_owned",
  #          users_trading = "users_trading"
  #   )
  # })
  output$graph_03 <- renderPlotly({
    q<-ggplot(boardgames, aes(x=data1(), y=data2(), label=name, fill = "dark_orange"))+geom_point()
    q<-q+xlab(input$xlab_03)+ylab(input$ylab_03)
    q<-q+theme_bw()+theme(panel.grid.major = element_blank())
    ggplotly(q)
  })
  observe({
    if ((input$xlab_03 == "complexity_weight" | input$xlab_03 == "rating_average") & (input$ylab_03 == "complexity_weight" | input$ylab_03 == "rating_average")){
      output$text_03 <- renderUI({
        HTML(
          paste0(
            h2("Insight:"), h4("As complexity increases, the average rating increases as well.  Due to the spread of the average rating, we can also see lines along the different complexity score values.  Complexity scores are rated between 1 and 5, and 0 represents games that have not been rated on complexity score.")
          )
        )
      })
      output$text2_03 <- renderUI({
        HTML(
          paste0(
            h2("Complexity vs. Average Rating") 
          )
        )
      })
    }
    if ((input$xlab_03 == "complexity_weight" | input$xlab_03 == "users_owned") & (input$ylab_03 == "complexity_weight" | input$ylab_03 == "users_owned")){
      output$text_03 <- renderUI({
        HTML(
          paste0(
            h2("Insight:"), h4("The board games with the highest owned numbers are games which are well known for being good games for both gamers and non-gamers like Catan, Pandemic, and Carcassonne.  Games seem to form a curve with a medium complexity in the center.")
          )
        )
      })
      output$text2_03 <- renderUI({
        HTML(
          paste0(
            h2("Complexity vs. Users Owned") 
          )
        )
      })
    }
    if ((input$xlab_03 == "complexity_weight" | input$xlab_03 == "users_trading") & (input$ylab_03 == "complexity_weight" | input$ylab_03 == "users_trading")){
      output$text_03 <- renderUI({
        HTML(
          paste0(
            h2("Insight:"), h4("Munchkin is the stand-out data point in this graph with a high number of users trading.  It's average is a little on the low side and it has a fairly high numbers of users that own it.")
          )
        )
      })
      output$text2_03 <- renderUI({
        HTML(
          paste0(
            h2("Complexity vs. Users Trading") 
          )
        )
      })
    }
    if ((input$xlab_03 == "rating_average" | input$xlab_03 == "users_owned") & (input$ylab_03 == "rating_average" | input$ylab_03 == "users_owned")){
      output$text_03 <- renderUI({
        HTML(
          paste0(
            h2("Insight:"), h4("Most of the games that are more commonly owned have an average rating between 6 and 8.5.  Gloomhaven and Pandemic Legacy Season 1 are both games that are above the curve with fairly high ownership and high average ratings.  Games like Munchkin, Risk, Uno, and The Game of Life fall below the curve.")
          )
        )
      })
      output$text2_03 <- renderUI({
        HTML(
          paste0(
            h2("Average Rating vs. Users Owned") 
          )
        )
      })
    }
    if ((input$xlab_03 == "rating_average" | input$xlab_03 == "users_trading") & (input$ylab_03 == "rating_average" | input$ylab_03 == "users_trading")){
      output$text_03 <- renderUI({
        HTML(
          paste0(
            h2("Insight:"), h4("Most games have an average rating between 4 and 8 with the center being around 7.  Some of the top games like Gloomhaven and Pandemic Legacy Season 1 are highly rated and have few games available for trading despite their number of copies owned.")
          )
        )
      })
      output$text2_03 <- renderUI({
        HTML(
          paste0(
            h2("Average Rating vs. Users Trading") 
          )
        )
      })
    }
    if ((input$xlab_03 == "users_owned" | input$xlab_03 == "users_trading") & (input$ylab_03 == "users_owned" | input$ylab_03 == "users_trading")){
      output$text_03 <- renderUI({
        HTML(
          paste0(
            h2("Insight:"), h4("This graph is a fairly straight line up the middle showing that more copies of a game are available for trade as more copies of that game are owned.  Some noteable games above the curve are Pandemic Legacy Season 1 and Azul while the Game of Thrones Card Game and Diplomacy are on the lower end.")
          )
        )
      })
      output$text2_03 <- renderUI({
        HTML(
          paste0(
            h2("Users Owned vs. Users Trading") 
          )
        )
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


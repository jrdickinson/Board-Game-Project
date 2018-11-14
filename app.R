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
      "Time Series and WordCloud", 
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
      "Various Insights", 
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
      "Board Game Explorer", 
      value=3,
      sidebarPanel(
        textInput("title_04", "Title"),
        splitLayout(
          textInput("key1_04", "Keyword"),
          textInput("key2_04", "Keyword"),
          textInput("key3_04", "Keyword")
        ),
        checkboxGroupInput("sub_04", "Subdomain", c("thematic_rank" = "thematic_rank", "strategy_rank" = "strategy_rank", "customizable_rank" = "customizable_rank", "family_rank" = "family_rank", "abstract_rank" = "abstract_rank", "childrens_rank" = "childrens_rank", "party_rank" = "party_rank", "war_rank" = "war_rank")),
        selectInput("categories1_04", "Category", c("None","Category: 'Abstract Strategy'","Category: 'Action / Dexterity'","Category: 'Adventure'","Category: 'Age of Reason'","Category: 'American Civil War'","Category: 'American Indian Wars'","Category: 'American Revolutionary War'","Category: 'American West'","Category: 'Ancient'","Category: 'Animals'","Category: 'Arabian'","Category: 'Aviation / Flight'","Category: 'Bluffing'","Category: 'Book'","Category: 'Card Game'","Category: 'City Building'","Category: 'Civil War'","Category: 'Civilization'","Category: 'Collectible Components'","Category: 'Comic Book / Strip'","Category: 'Deduction'","Category: 'Dice'","Category: 'Economic'","Category: 'Educational'","Category: 'Electronic'","Category: 'Environmental'","Category: 'Expansion for Base-game'","Category: 'Exploration'","Category: 'Fantasy'","Category: 'Farming'","Category: 'Fighting'","Category: 'Game System'","Category: 'Horror'","Category: 'Humor'","Category: 'Industry / Manufacturing'","Category: 'Korean War'","Category: 'Mafia'","Category: 'Math'","Category: 'Mature / Adult'","Category: 'Maze'","Category: 'Medical'","Category: 'Medieval'","Category: 'Memory'","Category: 'Miniatures'","Category: 'Modern Warfare'","Category: 'Movies / TV / Radio theme'","Category: 'Murder/Mystery'","Category: 'Music'","Category: 'Mythology'","Category: 'Napoleonic'","Category: 'Nautical'","Category: 'Negotiation'","Category: 'Novel-based'","Category: 'Number'","Category: 'Party Game'","Category: 'Pike and Shot'","Category: 'Pirates'","Category: 'Political'","Category: 'Post-Napoleonic'","Category: 'Prehistoric'","Category: 'Print & Play'","Category: 'Puzzle'","Category: 'Racing'","Category: 'Real-time'","Category: 'Religious'","Category: 'Renaissance'","Category: 'Science Fiction'","Category: 'Space Exploration'","Category: 'Spies/Secret Agents'","Category: 'Sports'","Category: 'Territory Building'","Category: 'Trains'","Category: 'Transportation'","Category: 'Travel'","Category: 'Trivia'","Category: 'Video Game Theme'","Category: 'Vietnam War'","Category: 'Wargame'","Category: 'Word Game'","Category: 'World War I'","Category: 'World War II'","Category: 'Zombies'","Category: \"Children's Game\"")),
        selectInput("categories2_04", "Category", c("None","Category: 'Abstract Strategy'","Category: 'Action / Dexterity'","Category: 'Adventure'","Category: 'Age of Reason'","Category: 'American Civil War'","Category: 'American Indian Wars'","Category: 'American Revolutionary War'","Category: 'American West'","Category: 'Ancient'","Category: 'Animals'","Category: 'Arabian'","Category: 'Aviation / Flight'","Category: 'Bluffing'","Category: 'Book'","Category: 'Card Game'","Category: 'City Building'","Category: 'Civil War'","Category: 'Civilization'","Category: 'Collectible Components'","Category: 'Comic Book / Strip'","Category: 'Deduction'","Category: 'Dice'","Category: 'Economic'","Category: 'Educational'","Category: 'Electronic'","Category: 'Environmental'","Category: 'Expansion for Base-game'","Category: 'Exploration'","Category: 'Fantasy'","Category: 'Farming'","Category: 'Fighting'","Category: 'Game System'","Category: 'Horror'","Category: 'Humor'","Category: 'Industry / Manufacturing'","Category: 'Korean War'","Category: 'Mafia'","Category: 'Math'","Category: 'Mature / Adult'","Category: 'Maze'","Category: 'Medical'","Category: 'Medieval'","Category: 'Memory'","Category: 'Miniatures'","Category: 'Modern Warfare'","Category: 'Movies / TV / Radio theme'","Category: 'Murder/Mystery'","Category: 'Music'","Category: 'Mythology'","Category: 'Napoleonic'","Category: 'Nautical'","Category: 'Negotiation'","Category: 'Novel-based'","Category: 'Number'","Category: 'Party Game'","Category: 'Pike and Shot'","Category: 'Pirates'","Category: 'Political'","Category: 'Post-Napoleonic'","Category: 'Prehistoric'","Category: 'Print & Play'","Category: 'Puzzle'","Category: 'Racing'","Category: 'Real-time'","Category: 'Religious'","Category: 'Renaissance'","Category: 'Science Fiction'","Category: 'Space Exploration'","Category: 'Spies/Secret Agents'","Category: 'Sports'","Category: 'Territory Building'","Category: 'Trains'","Category: 'Transportation'","Category: 'Travel'","Category: 'Trivia'","Category: 'Video Game Theme'","Category: 'Vietnam War'","Category: 'Wargame'","Category: 'Word Game'","Category: 'World War I'","Category: 'World War II'","Category: 'Zombies'","Category: \"Children's Game\"")),
        selectInput("categories3_04", "Category", c("None","Category: 'Abstract Strategy'","Category: 'Action / Dexterity'","Category: 'Adventure'","Category: 'Age of Reason'","Category: 'American Civil War'","Category: 'American Indian Wars'","Category: 'American Revolutionary War'","Category: 'American West'","Category: 'Ancient'","Category: 'Animals'","Category: 'Arabian'","Category: 'Aviation / Flight'","Category: 'Bluffing'","Category: 'Book'","Category: 'Card Game'","Category: 'City Building'","Category: 'Civil War'","Category: 'Civilization'","Category: 'Collectible Components'","Category: 'Comic Book / Strip'","Category: 'Deduction'","Category: 'Dice'","Category: 'Economic'","Category: 'Educational'","Category: 'Electronic'","Category: 'Environmental'","Category: 'Expansion for Base-game'","Category: 'Exploration'","Category: 'Fantasy'","Category: 'Farming'","Category: 'Fighting'","Category: 'Game System'","Category: 'Horror'","Category: 'Humor'","Category: 'Industry / Manufacturing'","Category: 'Korean War'","Category: 'Mafia'","Category: 'Math'","Category: 'Mature / Adult'","Category: 'Maze'","Category: 'Medical'","Category: 'Medieval'","Category: 'Memory'","Category: 'Miniatures'","Category: 'Modern Warfare'","Category: 'Movies / TV / Radio theme'","Category: 'Murder/Mystery'","Category: 'Music'","Category: 'Mythology'","Category: 'Napoleonic'","Category: 'Nautical'","Category: 'Negotiation'","Category: 'Novel-based'","Category: 'Number'","Category: 'Party Game'","Category: 'Pike and Shot'","Category: 'Pirates'","Category: 'Political'","Category: 'Post-Napoleonic'","Category: 'Prehistoric'","Category: 'Print & Play'","Category: 'Puzzle'","Category: 'Racing'","Category: 'Real-time'","Category: 'Religious'","Category: 'Renaissance'","Category: 'Science Fiction'","Category: 'Space Exploration'","Category: 'Spies/Secret Agents'","Category: 'Sports'","Category: 'Territory Building'","Category: 'Trains'","Category: 'Transportation'","Category: 'Travel'","Category: 'Trivia'","Category: 'Video Game Theme'","Category: 'Vietnam War'","Category: 'Wargame'","Category: 'Word Game'","Category: 'World War I'","Category: 'World War II'","Category: 'Zombies'","Category: \"Children's Game\"")),
        selectInput("expansion_04", "Is Expansion?", c("Either", "Yes", "No")),
        selectInput("mechanics1_04", "Mechanic", c("None","Mechanic: 'Acting'","Mechanic: 'Action / Movement Programming'","Mechanic: 'Action Point Allowance System'","Mechanic: 'Area-Impulse'","Mechanic: 'Area Control / Area Influence'","Mechanic: 'Area Enclosure'","Mechanic: 'Area Movement'","Mechanic: 'Auction/Bidding'","Mechanic: 'Betting/Wagering'","Mechanic: 'Campaign / Battle Card Driven'","Mechanic: 'Card Drafting'","Mechanic: 'Chit-Pull System'","Mechanic: 'Commodity Speculation'","Mechanic: 'Cooperative Play'","Mechanic: 'Crayon Rail System'","Mechanic: 'Deck / Pool Building'","Mechanic: 'Dice Rolling'","Mechanic: 'Grid Movement'","Mechanic: 'Hand Management'","Mechanic: 'Hex-and-Counter'","Mechanic: 'Line Drawing'","Mechanic: 'Memory'","Mechanic: 'Modular Board'","Mechanic: 'Paper-and-Pencil'","Mechanic: 'Partnerships'","Mechanic: 'Pattern Building'","Mechanic: 'Pattern Recognition'","Mechanic: 'Pick-up and Deliver'","Mechanic: 'Player Elimination'","Mechanic: 'Point to Point Movement'","Mechanic: 'Press Your Luck'","Mechanic: 'Rock-Paper-Scissors'","Mechanic: 'Role Playing'","Mechanic: 'Roll / Spin and Move'","Mechanic: 'Route/Network Building'","Mechanic: 'Secret Unit Deployment'","Mechanic: 'Set Collection'","Mechanic: 'Simulation'","Mechanic: 'Simultaneous Action Selection'","Mechanic: 'Singing'","Mechanic: 'Stock Holding'","Mechanic: 'Storytelling'","Mechanic: 'Take That'","Mechanic: 'Tile Placement'","Mechanic: 'Time Track'","Mechanic: 'Trading'","Mechanic: 'Trick-taking'","Mechanic: 'Variable Phase Order'","Mechanic: 'Variable Player Powers'","Mechanic: 'Voting'","Mechanic: 'Worker Placement'")),
        selectInput("mechanics2_04", "Mechanic", c("None","Mechanic: 'Acting'","Mechanic: 'Action / Movement Programming'","Mechanic: 'Action Point Allowance System'","Mechanic: 'Area-Impulse'","Mechanic: 'Area Control / Area Influence'","Mechanic: 'Area Enclosure'","Mechanic: 'Area Movement'","Mechanic: 'Auction/Bidding'","Mechanic: 'Betting/Wagering'","Mechanic: 'Campaign / Battle Card Driven'","Mechanic: 'Card Drafting'","Mechanic: 'Chit-Pull System'","Mechanic: 'Commodity Speculation'","Mechanic: 'Cooperative Play'","Mechanic: 'Crayon Rail System'","Mechanic: 'Deck / Pool Building'","Mechanic: 'Dice Rolling'","Mechanic: 'Grid Movement'","Mechanic: 'Hand Management'","Mechanic: 'Hex-and-Counter'","Mechanic: 'Line Drawing'","Mechanic: 'Memory'","Mechanic: 'Modular Board'","Mechanic: 'Paper-and-Pencil'","Mechanic: 'Partnerships'","Mechanic: 'Pattern Building'","Mechanic: 'Pattern Recognition'","Mechanic: 'Pick-up and Deliver'","Mechanic: 'Player Elimination'","Mechanic: 'Point to Point Movement'","Mechanic: 'Press Your Luck'","Mechanic: 'Rock-Paper-Scissors'","Mechanic: 'Role Playing'","Mechanic: 'Roll / Spin and Move'","Mechanic: 'Route/Network Building'","Mechanic: 'Secret Unit Deployment'","Mechanic: 'Set Collection'","Mechanic: 'Simulation'","Mechanic: 'Simultaneous Action Selection'","Mechanic: 'Singing'","Mechanic: 'Stock Holding'","Mechanic: 'Storytelling'","Mechanic: 'Take That'","Mechanic: 'Tile Placement'","Mechanic: 'Time Track'","Mechanic: 'Trading'","Mechanic: 'Trick-taking'","Mechanic: 'Variable Phase Order'","Mechanic: 'Variable Player Powers'","Mechanic: 'Voting'","Mechanic: 'Worker Placement'")),
        selectInput("mechanics3_04", "Mechanic", c("None","Mechanic: 'Acting'","Mechanic: 'Action / Movement Programming'","Mechanic: 'Action Point Allowance System'","Mechanic: 'Area-Impulse'","Mechanic: 'Area Control / Area Influence'","Mechanic: 'Area Enclosure'","Mechanic: 'Area Movement'","Mechanic: 'Auction/Bidding'","Mechanic: 'Betting/Wagering'","Mechanic: 'Campaign / Battle Card Driven'","Mechanic: 'Card Drafting'","Mechanic: 'Chit-Pull System'","Mechanic: 'Commodity Speculation'","Mechanic: 'Cooperative Play'","Mechanic: 'Crayon Rail System'","Mechanic: 'Deck / Pool Building'","Mechanic: 'Dice Rolling'","Mechanic: 'Grid Movement'","Mechanic: 'Hand Management'","Mechanic: 'Hex-and-Counter'","Mechanic: 'Line Drawing'","Mechanic: 'Memory'","Mechanic: 'Modular Board'","Mechanic: 'Paper-and-Pencil'","Mechanic: 'Partnerships'","Mechanic: 'Pattern Building'","Mechanic: 'Pattern Recognition'","Mechanic: 'Pick-up and Deliver'","Mechanic: 'Player Elimination'","Mechanic: 'Point to Point Movement'","Mechanic: 'Press Your Luck'","Mechanic: 'Rock-Paper-Scissors'","Mechanic: 'Role Playing'","Mechanic: 'Roll / Spin and Move'","Mechanic: 'Route/Network Building'","Mechanic: 'Secret Unit Deployment'","Mechanic: 'Set Collection'","Mechanic: 'Simulation'","Mechanic: 'Simultaneous Action Selection'","Mechanic: 'Singing'","Mechanic: 'Stock Holding'","Mechanic: 'Storytelling'","Mechanic: 'Take That'","Mechanic: 'Tile Placement'","Mechanic: 'Time Track'","Mechanic: 'Trading'","Mechanic: 'Trick-taking'","Mechanic: 'Variable Phase Order'","Mechanic: 'Variable Player Powers'","Mechanic: 'Voting'","Mechanic: 'Worker Placement'")),
        numericInput("age_04", "Suitable for Players Age _ and up", 42, 0, 42),
        splitLayout(
          numericInput("complexity1_04", "Complexity Min.(0)", 0, 0, 5),
          numericInput("complexity2_04", "Complexity Max.(5)", 5, 0, 5),
          numericInput("complexityvotes_04", "Num. of Votes(0:6809)", 0, 0, 6809) 
        ),
        splitLayout(
          numericInput("average1_04", "Average Rating Min.(0)", 0, 0, 10),
          numericInput("average2_04", "Average Rating Max.(10)", 10, 0, 10),
          numericInput("averagevotes_04", "Num. of Votes(30:80518)", 30, 30, 80518)
        ),
        splitLayout(
          numericInput("year1_04", "Year Min.(-3500)", -3500, -3500, 2019),
          numericInput("year2_04", "Year Max.(2019)", 2019, -3500, 2019)
        ),
        numericInput("players1_04", "Min. Players(0)", 0, 0, 999),
        numericInput("players2_04", "Max. Players(999)", 999, 0, 999),
        numericInput("time1_04", "Min. Playing Time(0)", 0, 0, 120000),
        numericInput("time2_04", "Max. Playing Time(120000)", 120000, 0, 120000),
        numericInput("owned_04", "Min. Users Owned(2:116851)", 2, 2, 116851),
        numericInput("trading_04", "Min. Users Trading(0:2101)", 0, 0, 2101)
      ),
      mainPanel(
        dataTableOutput("table_04")
      )
    ), 
    tabPanel(
      "Read Me", 
      value=4,
      mainPanel(
      h2("Title:  Board Game Analysis"),
      h2("Author: Jeremy Dickinson"),
      h2("Type: Shiny App"),
      h2("Description:"),
      h3("     Welcome to my Shiny App!  This app displays different types of insights into board game data collected from BoardGameGeek.  The data included here is only for games which are ranked as of November 7th.  Not all of the ranks are perfect since not all the data was pulled at the same time, but ranks are assumed to be close since the data was collected in the span of one day."),
      h3("     On the 'Time Series and WordCloud' tab, users can shift the points of the slider to adjust the range of years for the graph.  Users can also input the name of a boardgame to see what words appear and are the most common within that boardgames description.  Clicking the update button causes these changes to the graphs to take effect; the graphs will not update unless it is clicked."),
      h3("     On the 'Various Insights' tab, users can use the dropdown menus located on the left side of the screen to select between four different choices for the graph axis.  These selections will automatically update the graph.  Below the choice panels, insights will appear for each possible graph.  Experiment with the different combinations to find different insights."),
      h3("     On the 'Board Game Explorer' tab, users can see some of the different features of the dataset and have many different types of filters available to them.  The majority of the boardgamegeek advanced search features have been included plus some extras like number of users owned and the ability to search game descriptions by keywords.  The data set updates automatically when the value of a filter changes.  Experiment with the different filters and see what you can find!"),
      h2("References:"),
      h3("(2018, November 7th) Boardgamegeek.  Data Retrieved from www.boardgamegeek.com"),
      h3("Special thanks to Rstudios and the creators of the boardgamegeek2 python library."),
      h2("Visit the BoardGameGeek site with the link below:"),
      htmlOutput("html_link")
      )
    )
  )
)


server <- function(input, output) 
{
  boardgames2 <- reactive({filter(boardgames, year >= input$slider_01[1] & year <= input$slider_01[2])})
  output$graph_01 <- renderPlotly({input$button_01
    
    p<-isolate(ggplot(boardgames2(), aes(x=as.factor(year)))+geom_bar(fill="blue", colour="red"))
    p<-isolate(p+xlab("Year")+ylab("Count of Ranked Board Games"))
    p<-isolate(p+ggtitle("Count of Ranked Board Games by Year"))
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
  filter1 <- reactive({
    if (input$title_04 != ""){
      mydf <- select(filter(boardgames, tolower(name) == tolower(input$title_04)), id)
      mydf$id
    }else{
      boardgames$id
    }
  })
  filter2 <- reactive({
    if (input$key1_04 != ""){
      templist <- list()
      for (i in 1:16226){
        if (tolower(input$key1_04) %in% tolower(as.list(strsplit(boardgames$description[i], '\\s+')[[1]])) == TRUE){
          templist <- c(templist, boardgames$id[i])
        }
      }
      templist
    }else{
      boardgames$id
    }
  })
  filter3 <- reactive({
    if (input$key2_04 != ""){
      templist <- list()
      for (i in 1:16226){
        if (tolower(input$key2_04) %in% tolower(as.list(strsplit(boardgames$description[i], '\\s+')[[1]])) == TRUE){
          templist <- c(templist, boardgames$id[i])
        }
      }
      templist
    }else{
      boardgames$id
    }
  })
  filter4 <- reactive({
    if (input$key3_04 != ""){
      templist <- list()
      for (i in 1:16226){
        if (tolower(input$key3_04) %in% tolower(as.list(strsplit(boardgames$description[i], '\\s+')[[1]])) == TRUE){
          templist <- c(templist, boardgames$id[i])
        }
      }
      templist
    }else{
      boardgames$id
    }
  })
  filter5 <- reactive({
    if ("thematic_rank" %in% input$sub_04 == TRUE){
      mytest <- select(filter(boardgames, boardgames$thematic_rank > 0), id)
      mytest1 <- mytest$id
    }else{
      mytest1 <- boardgames$id
    } 
    if ("war_rank" %in% input$sub_04 == TRUE){
      mytest <- select(filter(boardgames, boardgames$war_rank > 0), id)
      mytest2 <- mytest$id
    }else{
      mytest2 <- boardgames$id
    } 
    if ("strategy_rank" %in% input$sub_04 == TRUE){
      mytest <- select(filter(boardgames, boardgames$strategy_rank > 0), id)
      mytest3 <- mytest$id
    }else{
      mytest3 <- boardgames$id
    } 
    if ("party_rank" %in% input$sub_04 == TRUE){
      mytest <- select(filter(boardgames, boardgames$party_rank > 0), id)
      mytest4 <- mytest$id
    }else{
      mytest4 <- boardgames$id
    } 
    if ("abstract_rank" %in% input$sub_04 == TRUE){
      mytest <- select(filter(boardgames, boardgames$abstract_rank > 0), id)
      mytest5 <- mytest$id
    }else{
      mytest5 <- boardgames$id
    } 
    if ("family_rank" %in% input$sub_04 == TRUE){
      mytest <- select(filter(boardgames, boardgames$family_rank > 0), id)
      mytest6 <- mytest$id
    }else{
      mytest6 <- boardgames$id
    } 
    if ("customizable_rank" %in% input$sub_04 == TRUE){
      mytest <- select(filter(boardgames, boardgames$customizable_rank > 0), id)
      mytest7 <- mytest$id
    }else{
      mytest7 <- boardgames$id
    } 
    if ("childrens_rank" %in% input$sub_04 == TRUE){
      mytest <- select(filter(boardgames, boardgames$childrens_rank > 0), id)
      mytest8 <- mytest$id
    }else{
      mytest8 <- boardgames$id
    } 
    mytestlist <- Reduce(intersect, list(mytest1, mytest2, mytest3, mytest4, mytest5, mytest6, mytest7, mytest8))
    mytestlist
  })
  filter6 <- reactive({
    if (input$categories1_04 != "None"){
      mydf <- select(filter(boardgames, boardgames[[input$categories1_04]] == 1), id)
      mydf$id
    }else{
      boardgames$id
    }
  })
  filter7 <- reactive({
    if (input$categories2_04 != "None"){
      mydf <- select(filter(boardgames, boardgames[[input$categories2_04]] == 1), id)
      mydf$id
    }else{
      boardgames$id
    }
  })
  filter8 <- reactive({
    if (input$categories3_04 != "None"){
      mydf <- select(filter(boardgames, boardgames[[input$categories3_04]] == 1), id)
      mydf$id
    }else{
      boardgames$id
    }
  })
  filter9 <- reactive({
    if (input$expansion_04 == "Yes"){
      mydf <- select(filter(boardgames, expands_1 > 0), id)
      mydf$id
    }else if (input$expansion_04 == "No"){
      mydf <- select(filter(boardgames, is.na(expands_1)), id)
      mydf$id
    }else{
      boardgames$id
    }
  })
  filter10 <- reactive({
    if (input$mechanics1_04 != "None"){
      mydf <- select(filter(boardgames, boardgames[[input$mechanics1_04]] == 1), id)
      mydf$id
    }else{
      boardgames$id
    }
  })
  filter11 <- reactive({
    if (input$mechanics2_04 != "None"){
      mydf <- select(filter(boardgames, boardgames[[input$mechanics2_04]] == 1), id)
      mydf$id
    }else{
      boardgames$id
    }
  })
  filter12 <- reactive({
    if (input$mechanics3_04 != "None"){
      mydf <- select(filter(boardgames, boardgames[[input$mechanics3_04]] == 1), id)
      mydf$id
    }else{
      boardgames$id
    }
  })
  filter13 <- reactive({
    mydf <- select(filter(boardgames, boardgames$min_age <= input$age_04), id)
    mydf$id
  })
  filter14 <- reactive({
    mydf <- select(filter(boardgames, boardgames$complexity_weight >= input$complexity1_04), id)
    mydf$id
  })
  filter15 <- reactive({
    mydf <- select(filter(boardgames, boardgames$complexity_weight <= input$complexity2_04), id)
    mydf$id
  })
  filter16 <- reactive({
    mydf <- select(filter(boardgames, boardgames$complexity_num_weights >= input$complexityvotes_04), id)
    mydf$id
  })
  filter17 <- reactive({
    mydf <- select(filter(boardgames, boardgames$rating_average >= input$average1_04), id)
    mydf$id
  })
  filter18 <- reactive({
    mydf <- select(filter(boardgames, boardgames$rating_average <= input$average2_04), id)
    mydf$id
  })
  filter19 <- reactive({
    mydf <- select(filter(boardgames, boardgames$users_rated >= input$averagevotes_04), id)
    mydf$id
  })
  filter20 <- reactive({
    mydf <- select(filter(boardgames, boardgames$year >= input$year1_04), id)
    mydf$id
  })
  filter21 <- reactive({
    mydf <- select(filter(boardgames, boardgames$year <= input$year2_04), id)
    mydf$id
  })
  filter22 <- reactive({
    mydf <- select(filter(boardgames, boardgames$min_players >= input$players1_04), id)
    mydf$id
  })
  filter23 <- reactive({
    mydf <- select(filter(boardgames, boardgames$max_players <= input$players2_04), id)
    mydf$id
  })
  filter24 <- reactive({
    mydf <- select(filter(boardgames, boardgames$min_playing_time >= input$time1_04), id)
    mydf$id
  })
  filter25 <- reactive({
    mydf <- select(filter(boardgames, boardgames$max_playing_time <= input$time2_04), id)
    mydf$id
  })
  filter26 <- reactive({
    mydf <- select(filter(boardgames, boardgames$users_owned >= input$owned_04), id)
    mydf$id
  })
  filter27 <- reactive({
    mydf <- select(filter(boardgames, boardgames$users_trading >= input$trading_04), id)
    mydf$id
  })
  output$table_04 <- renderDataTable({
    mylist <- Reduce(intersect, list(filter1(), filter2(), filter3(), filter4(), filter5(), filter6(), filter7(), filter8(), filter9(), filter10(), filter11(), filter12(), filter13(), filter14(), filter15(), filter16(), filter17(), filter18(), filter19(), filter20(), filter21(), filter22(), filter23(), filter24(), filter25(), filter26(), filter27()))
    boardgames4 <- select(arrange(boardgames[boardgames$id %in% mylist,], overall_rank), name, id, year, overall_rank, rating_average, min_age,max_players, min_playing_time, complexity_weight)
    boardgames4
  })
  output$html_link <- renderUI({
    tagList("", a("BoardGameGeek", href=paste("https://www.boardgamegeek.com")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


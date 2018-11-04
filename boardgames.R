library(dplyr)
library(stringr)
library(Hmisc)
library(readr)
library(stringi)
library(splitstackshape)
setwd("C:/Users/jerem.DESKTOP-GGM6Q2I/Documents/UNH Data Analytics/r programming 2/final_project")
boardgames <- read_csv("boardgames.csv")
describe(boardgames)
# boardgames$copy <- boardgames$artists
x=list("a", "b")
y = list("c", "d", "3")
z = list(x,y)
# boardgames$copy
# boardgames$copy<-sub(".*name: ", "",boardgames$copy)
# boardgames$copy<-sub(").*", "",boardgames$copy)
# boardgames$copy <- str_replace(boardgames$copy, pattern=", value", replacement="")
# boardgames$ranks[1]
# x = "a, b, c, d"
# boardgames$copy <- stri_split_fixed(boardgames$copy, pattern = ", ", n = 20)
# boardgames$copy
# boardgames$copy[1]
# cSplit(df, "Events", sep=",")
boardgames$ranks<-sub(")].*", "",boardgames$ranks)
boardgames <- cSplit(boardgames, "ranks", sep=")")
boardgames$ranks_1<-sub(".*name: ", "",boardgames$ranks_1)
boardgames$ranks_2<-sub(".*name: ", "",boardgames$ranks_2)
boardgames$ranks_3<-sub(".*name: ", "",boardgames$ranks_3)
boardgames$ranks_1 <- str_replace(boardgames$ranks_1, pattern=", value", replacement="")
boardgames$ranks_2 <- str_replace(boardgames$ranks_2, pattern=", value", replacement="")
boardgames$ranks_3 <- str_replace(boardgames$ranks_3, pattern=", value", replacement="")
boardgames$ranks_1[1]
boardgames$ranks_2[1]
boardgames$ranks_3[1]
#Clean expansions
boardgames$expansions<-sub(")].*", '',boardgames$expansions)
boardgames <- cSplit(boardgames, "expansions", sep=",")
for (i in 1:62){
  boardgames[[paste0("expansions_", formatC(i, width = 2, flag = '0'))]] <- sub(".*id: ", "", boardgames[[paste0("expansions_", formatC(i, width = 2, flag = '0'))]])
}
for (i in 1:62){
  boardgames[[paste0("expansions_", formatC(i, width = 2, flag = '0'))]] <- str_replace(boardgames[[paste0("expansions_", formatC(i, width = 2, flag = '0'))]], pattern = "\\)","")
}
#Clean categories, families, mechanics, artists, designers, publishers
columns <- names(boardgames)[4:9]
for (i in columns){
  boardgames[[i]] <- str_replace(boardgames[[i]], "\\[", "")
  boardgames[[i]] <- str_replace(boardgames[[i]], "\\]", "")
  boardgames <- cSplit(boardgames, i, sep = ",")
}
#Clean
boardgames$thematic_rank <- 0
boardgames$strategy_rank <- 0
boardgames$customizable_rank <- 0
boardgames$family_rank <- 0
boardgames$abstract_rank <- 0
boardgames$childrens_rank <- 0
boardgames$party_rank <- 0
boardgames$war_rank <- 0
boardgames$overall_rank <- 0
for (i in 1:4){
  boardgames$thematic_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$strategy_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$customizable_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$family_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$abstract_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$childrens_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$party_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$war_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$overall_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$thematic > 0, ifelse(boardgames$thematic > 0, boardgames$thematic, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  
  }
boardgames$ranks_1[5]
boardgames$thematic
boardgames$ranks_4 <- "help!"


library(dplyr)
library(stringr)
library(Hmisc)
library(readr)
library(stringi)
library(splitstackshape)
library(zoo)
setwd("C:/Users/jerem.DESKTOP-GGM6Q2I/Documents/UNH Data Analytics/r programming 2/final_project")
boardgames <- read_csv("boardgames_full.csv")
describe(boardgames)
colnames(boardgames)

#rearrange columns
boardgames <- select(boardgames, c("name", "id", "description", "year", "expansion", "rating_average", "rating_stddev", "users_rated", "min_age", "min_players", "max_players", "min_playing_time", "max_playing_time", "complexity_weight", "complexity_num_weights", "users_owned", "users_trading", "ranks", "expands", "categories", "mechanics"))


#Clean ranks
boardgames$ranks<-sub(")].*", "",boardgames$ranks)
boardgames <- cSplit(boardgames, "ranks", sep=")")
boardgames$ranks_1<-sub(".*name: ", "",boardgames$ranks_1)
boardgames$ranks_2<-sub(".*name: ", "",boardgames$ranks_2)
boardgames$ranks_3<-sub(".*name: ", "",boardgames$ranks_3)
boardgames$ranks_4<-sub(".*name: ", "",boardgames$ranks_4)
boardgames$ranks_5<-sub(".*name: ", "",boardgames$ranks_5)
boardgames$ranks_6<-sub(".*name: ", "",boardgames$ranks_6)
boardgames$ranks_7<-sub(".*name: ", "",boardgames$ranks_7)
boardgames$ranks_1 <- str_replace(boardgames$ranks_1, pattern=", value", replacement="")
boardgames$ranks_2 <- str_replace(boardgames$ranks_2, pattern=", value", replacement="")
boardgames$ranks_3 <- str_replace(boardgames$ranks_3, pattern=", value", replacement="")
boardgames$ranks_4 <- str_replace(boardgames$ranks_4, pattern=", value", replacement="")
boardgames$ranks_5 <- str_replace(boardgames$ranks_5, pattern=", value", replacement="")
boardgames$ranks_6 <- str_replace(boardgames$ranks_6, pattern=", value", replacement="")
boardgames$ranks_7 <- str_replace(boardgames$ranks_7, pattern=", value", replacement="")
# boardgames$ranks_1[1]
# boardgames$ranks_2[1]
# boardgames$ranks_3[1]

#Clean expands
boardgames$expands<-sub(")].*", '',boardgames$expands)
boardgames <- cSplit(boardgames, "expands", sep=",")
#need to make sure width and loop length are correct
for (i in 1:7){
  boardgames[[paste0("expands_", formatC(i, width = 1, flag = '0'))]] <- sub(".*id: ", "", boardgames[[paste0("expands_", formatC(i, width = 1, flag = '0'))]])
}
for (i in 1:7){
  boardgames[[paste0("expands_", formatC(i, width = 1, flag = '0'))]] <- str_replace(boardgames[[paste0("expands_", formatC(i, width = 1, flag = '0'))]], pattern = "\\)","")
}

#Clean categories, families, mechanics, artists, designers, publishers
#left out families, artists, designers, and publishers
columns <- list("categories", "mechanics")
for (i in columns){
  boardgames[[i]] <- str_replace(boardgames[[i]], "\\[", "")
  boardgames[[i]] <- str_replace(boardgames[[i]], "\\]", "")
  boardgames <- cSplit(boardgames, i, sep = ",")
}

#Create subdomain categories on ranks
boardgames$thematic_rank <- 0
boardgames$strategy_rank <- 0
boardgames$customizable_rank <- 0
boardgames$family_rank <- 0
boardgames$abstract_rank <- 0
boardgames$childrens_rank <- 0
boardgames$party_rank <- 0
boardgames$war_rank <- 0
boardgames$overall_rank <- 0
for (i in 1:7){
  boardgames$thematic_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Thematic") == TRUE | boardgames$thematic_rank > 0, ifelse(boardgames$thematic_rank > 0, boardgames$thematic_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$strategy_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Strategy") == TRUE | boardgames$strategy_rank > 0, ifelse(boardgames$strategy_rank > 0, boardgames$strategy_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$customizable_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Customizable") == TRUE | boardgames$customizable_rank > 0, ifelse(boardgames$customizable_rank > 0, boardgames$customizable_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$family_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Family") == TRUE | boardgames$family_rank > 0, ifelse(boardgames$family_rank > 0, boardgames$family_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$abstract_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Abstract") == TRUE | boardgames$abstract_rank > 0, ifelse(boardgames$abstract_rank > 0, boardgames$abstract_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$childrens_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Children's") == TRUE | boardgames$childrens_rank > 0, ifelse(boardgames$childrens_rank > 0, boardgames$childrens_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$party_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Party") == TRUE | boardgames$party_rank > 0, ifelse(boardgames$party_rank > 0, boardgames$party_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$war_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "War") == TRUE | boardgames$war_rank > 0, ifelse(boardgames$war_rank > 0, boardgames$war_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
  boardgames$overall_rank <- ifelse(str_detect(boardgames[[paste0("ranks_", i)]], pattern = "Board Game") == TRUE | boardgames$overall_rank > 0, ifelse(boardgames$overall_rank > 0, boardgames$overall_rank, as.numeric(sub(".*: ", "", boardgames[[paste0("ranks_", i)]]))), 0)
}



# colnames(boardgames)
# new_list = list()
# for (i in 1:511){
#   new_list<-c(new_list, boardgames[[paste0("artists_", formatC(i, width = 3, flag = '0'))]])


#Create flag columns for categories and mechanics 
mechs = levels(unique(boardgames$mechanics_01))
cats = levels(unique(boardgames$categories_01))
for (i in cats){
  boardgames[[paste0("Category: ", i)]] = ifelse(boardgames$categories_01 == i | boardgames$categories_02 == i | boardgames$categories_03 == i | boardgames$categories_04 == i | boardgames$categories_05 == i | boardgames$categories_06 == i | boardgames$categories_07 == i | boardgames$categories_08 == i | boardgames$categories_09 == i | boardgames$categories_10 == i | boardgames$categories_11 == i | boardgames$categories_12 == i | boardgames$categories_13 == i | boardgames$categories_14 == i, 1, NA)
}
for (i in mechs){
  boardgames[[paste0("Mechanic: ", i)]] = ifelse(boardgames$mechanics_01 == i | boardgames$mechanics_02 == i | boardgames$mechanics_03 == i | boardgames$mechanics_04 == i | boardgames$mechanics_05 == i | boardgames$mechanics_06 == i | boardgames$mechanics_07 == i | boardgames$mechanics_08 == i | boardgames$mechanics_09 == i | boardgames$mechanics_10 == i | boardgames$mechanics_11 == i | boardgames$mechanics_12 == i | boardgames$mechanics_13 == i | boardgames$mechanics_14 == i | boardgames$mechanics_15 == i | boardgames$mechanics_16 == i | boardgames$mechanics_17 == i | boardgames$mechanics_18 == i, 1, NA)
}

#remove unwanted columns
for (i in 1:14){
  boardgames <- select(boardgames, -c(paste0("categories_", formatC(i, width = 2, flag = '0'))))
}
for (i in 1:18){
  boardgames <- select(boardgames, -c(paste0("mechanics_", formatC(i, width = 2, flag = '0'))))
}
for (i in 1:7){
  boardgames <- select(boardgames, -c(paste0("ranks_", formatC(i, width = 1, flag = '0'))))
}

#clean expands_1
boardgames$expands_1 <- ifelse(boardgames$expands_1 == "[]", NA, boardgames$expands_1)

#fix NAs and 0s
boardgames$thematic_rank <- ifelse(boardgames$thematic_rank != 0, boardgames$thematic_rank, NA)
boardgames$strategy_rank <- ifelse(boardgames$strategy_rank != 0, boardgames$strategy_rank, NA)
boardgames$customizable_rank <- ifelse(boardgames$customizable_rank != 0, boardgames$customizable_rank, NA)
boardgames$family_rank <- ifelse(boardgames$family_rank != 0, boardgames$family_rank, NA)
boardgames$abstract_rank <- ifelse(boardgames$abstract_rank != 0, boardgames$abstract_rank, NA)
boardgames$childrens_rank <- ifelse(boardgames$childrens_rank != 0, boardgames$childrens_rank, NA)
boardgames$party_rank <- ifelse(boardgames$party_rank != 0, boardgames$party_rank, NA)
boardgames$war_rank <- ifelse(boardgames$war_rank != 0, boardgames$war_rank, NA)
boardgames$overall_rank <- ifelse(boardgames$overall_rank != 0, boardgames$overall_rank, NA)

columns2 <- colnames(select(boardgames, 34:167))
for (i in columns2){
  boardgames[[i]][is.na(boardgames[[i]])] <- 0
}

#values couldn't all be collected at the same time, so there are some duplicate ranks, but not many.  

#remove values without overall rank
boardgames <- boardgames[!is.na(boardgames$overall_rank),]
#fix min age 180, check on 42
boardgames$min_age[boardgames$min_age == 180] <- 18

#create csv
write.csv(boardgames, "board_games_cleaned.csv", row.names = FALSE)
describe(boardgames)


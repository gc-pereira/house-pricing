library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)

df_train <- read_csv('/home/gabriel/Desktop/house-pricing/train.csv')
df_test <- read_csv('/home/gabriel/Desktop/house-pricing/test.csv')

df <- df_train %>% full_join(df_test)
head(df)

shape <- function(df){
  s = c(ncol(df), nrow(df))
  return(s)
}

# train test split
split_size = 0.70
sample_size = floor(split_size * nrow(df))
set.seed(123)
train_indices <- sample(seq_len(nrow(df)), size = sample_size, replace = FALSE)

train <- df[train_indices,]
test <- df[-train_indices,]

# feature engineering
find_na <- function(df){
  na = apply(is.na(df), 2, sum)
  na = as.data.frame(na)
  na <- cbind(variable = rownames(na), na)
  rownames(na) <- 1:nrow(na)
  na <- na[order(na$na, decreasing = T),]
  return(na)
}

delete_na <- function(df, percent){
  na <- find_na(df)
  for(i in 1:nrow(na)){
    if(na[i,2]/nrow(df) > percent){
      #df = subset(df, select = -na[i,1])
      df = df %>% select(!na[i,1])
    }
  }
  return(df)
}


visualize_na <- function(train){
  na <- find_na(train)
  p <- ggplot(data=na %>% filter(na > 0), aes(x=round(na/nrow(train), 5), y = variable)) +
    geom_bar(stat="identity", fill="coral1") + 
    labs(x = "Porcentagem de dados faltantes", y = "Variáveis") +
    geom_text(aes(label=round(na/nrow(train), 5)*100), hjust=0, color="black", size=3.5) +
    theme_minimal()
  return(p)
}

replace_ <- function(data, column, value){
  xbar = mean(as.matrix(data[column]), na.rm = TRUE)
  mode <- getmode(data[column])[1]
  for(i in 1:nrow(data[column])){
    if(is.na(data[i, column])){
      if(value == "xbar"){
        data[i, column] = xbar
      } 
      if(value == "mode"){
        if(is.na(as.double(mode)) == FALSE){
          data[i, column] = as.double(mode)
        } else {
          data[i, column] = mode
        }
      }
    }
  }
  return(data)
}

getmode <- function(x) {
  names(table(x))[table(x)==max(table(x))]
}

visualize_na(train)

train <- delete_na(train, 0.8)
train <- replace_(train, "LotFrontage", value = "xbar")
train <- replace_(train, "GarageYrBlt", value = "mode")
train <- replace_(train, "GarageType", value = "mode")
train <- replace_(train, "GarageQual", value = "mode")
train <- replace_(train, "GarageCond", value = "mode")
train <- replace_(train, "GarageFinish", value = "mode")
visualize_na(train)



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
# apply feature engineer
na <- find_na(train)
head(na)

visualize_na(train)

train <- delete_na(train, 0.9)

visualize_na(train)

library(ggpubr)
library(ggcorrplot)
library(corrgram)
library(GGally)
library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr)

train <- read_csv('/home/gabriel/Desktop/house-pricing/train.csv')
test <- read_csv('/home/gabriel/Desktop/house-pricing/test.csv')

#df <- df_train %>% full_join(df_test)
#head(df)

shape <- function(df){
  s = c(ncol(df), nrow(df))
  return(s)
}

# train test split
#split_size = 0.70
#sample_size = floor(split_size * nrow(df))
#set.seed(123)
#train_indices <- sample(seq_len(nrow(df)), size = sample_size, replace = FALSE)

#train <- df[train_indices,]
#test <- df[-train_indices,]

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

# delete column
train <- delete_na(train, 0.8)
train <- train %>% select(!Id)
train <- train %>% select(!GarageYrBlt)
shape(train)

visualize_na(train)

# fill quantitative variables
train <- replace_(train, "LotFrontage", value = "xbar")
train <- replace_(train, "GarageArea", value = "xbar")
visualize_na(train)


# fill qualitative variables
train <- replace_(train, "GarageType", value = "mode")
train <- replace_(train, "GarageQual", value = "mode")
train <- replace_(train, "GarageCond", value = "mode")
train <- replace_(train, "GarageFinish", value = "mode")
train <- replace_(train, "MasVnrArea", value = "mode")
train <- replace_(train, "MasVnrType", value = "mode")
train <- replace_(train, "FireplaceQu", value = "mode")
train <- replace_(train, "Electrical", value = "mode")
train <- replace_(train, "BsmtQual", value = "mode")
train <- replace_(train, "BsmtFinType2", value = "mode")
train <- replace_(train, "BsmtFinType1", value = "mode")
train <- replace_(train, "BsmtExposure", value = "mode")
train <- replace_(train, "BsmtCond", value = "mode")

visualize_na(train)

k1 = 1 + 3.3+log(shape(train)[2], base = 10)
k2 = sqrt(nrow(train))

ggplot(data = train, aes(x = SalePrice)) +
  geom_histogram(bins = k2, fill = "coral")


categorical <- c('MSZoning', 'Street', 'LotShape', 'LandContour', 'Utilities', 'LotConfig', 'LandSlope', 'Neighborhood', 
  'Condition1', 'Condition2', 'BldgType', 'HouseStyle', 'RoofStyle', 'RoofMatl', 'Exterior1st', 'Exterior2nd', 
  'MasVnrType', 'ExterQual', 'ExterCond', 'Foundation', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 
  'BsmtFinType2', 'Heating', 'HeatingQC', 'CentralAir', 'Electrical', 'KitchenQual', 'Functional', 'FireplaceQu', 
  'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond', 'PavedDrive', 'SaleType', 'SaleCondition')

train_numeric <- select(train, !any_of(categorical))
matrizcor <- ggcorr(train_numeric, 
                  method = c("everything", "pearson")) 

matrizcor
round(cor(train_numeric), digits = 5)

cor_df <- as.data.frame(cor(train_numeric))
head(cor_df)

correlacoes_ordenadas <- sort(cor(train_numeric), decreasing = TRUE)
correlacoes_ordenadas <- as.data.frame(correlacoes_ordenadas)
correlacoes_ordenadas <- correlacoes_ordenadas %>% arrange(desc(abs(correlacoes_ordenadas)))
correlacoes_ordenadas <- correlacoes_ordenadas$correlacoes_ordenadas

conta_elementos <- function(element, v){
  soma = 0
  for(i in v){
    if(i == element){
      soma <- soma + 1
    }
  }
  return(soma)
}
conta_elementos(1, correlacoes_ordenadas)

head(correlacoes_ordenadas)
correlacoes_ordenadas <- unique(correlacoes_ordenadas[-c(1:36)])
correlacoes_ordenadas

encontrar_numeros_dataframe <- function(list, df){
  #listas e contadores utilizados
  cols_positions <- c()
  row_positions <- c()
  values <- c()
  rows <- c()
  sum_ <- c()
  count = 1
  count2 <- 1
  
  for(i in list){
    for(j in 1:ncol(df)){
      t_and_f <- (round(i, 3) == round(df[,j], 3))
      for(k in 1:length(t_and_f)){
        if(t_and_f[k] == TRUE){
          cols_positions[count] = j
          row_positions[count] = k
          values[count] = i
          count = count + 1
        }
      }
    }
  }
  data <- data_frame(
    colunas = cols_positions, 
    linhas = row_positions,
    correlacoes = values
  )
  for(i in 1:nrow(data)){
    if(i %% 2 == 0){
      rows[count2] = i
      count2 = count2 + 1
    }
  }
  data <- data[-rows, ]
  return(data)
}
df <- encontrar_numeros_dataframe(correlacoes_ordenadas[1:20], cor_df)

df$colour <- ifelse(df$correlacoes < 0, "firebrick2", "steelblue")
ggplot(df, aes(x = paste(linhas, colunas), y = abs(correlacoes), colour=colour)) +
  geom_bar(stat = "identity", position = "dodge", fill=df$colour) +
  scale_colour_identity()+
  geom_text(aes(label = round(correlacoes, 3)), position = position_dodge(width = 1),
            vjust = .5, hjust = 1.2, size = 3, colour="white") +
  xlab("Correlações")+
  ylab("Variáveis")+
  coord_flip()

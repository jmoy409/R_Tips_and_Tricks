#library for recursive partitioning - handle missing data
library(rpart)
#filter by dplyr
library(dplyr)
#dfc and dfr by purr - faster than for loop
library(purrr)

markets <- c('Chicago', 'Dallas', 'Boston')

data <- data_frame(Date= rep(seq(from=as.Date('2018-01-01'), to=as.Date('2018-03-01'), by='day'),each=3), 
                   market = rep(markets,60), KPI1=sample(80:100,180,replace=T), KPI2=sample(70:100,180,replace=T), KPI3=sample(80:100,180,replace=T))

#ensure it is data.frame class
#not tbl_df, tbl, and data.frame

data <- as.data.frame(data)
#fill in missing data

c <- which(data[,'KPI1']==80)
#c <- which(data$KPI1==80)

#fill in zeros
for (col in c){
  data$KPI1[c]<-0
}


c <- which(data[,'KPI2']==85)
#fill in zeros
for (col in c){
  data$KPI2[c]<-0
}


#impute portion



#impute portion
final <-data.frame()


#function will run recursive partitioning and pull all necessary information
#will return column of choice for the specified market entered
#will verify if index has missing entry and will impute at missing entry

impute_col <- function(data, mkt, col){
  fit.col <- rpart(data_sub[,col]~.,data=data_sub)
  #choose the lowest error in the datatable
  opt <- which.min(fit.col$cptable[,"xerror"])
  #choose the corresponding CP value
  cp <- fit.col$cptable[opt, "CP"]
  #prune(cost-complexity) the tree to that degree
  #snip off the least important splits based on complexity parameter
  col.prune <- prune(fit.col, cp = cp)
  
  #predict(model,dataframe)
  pred.col <- predict(col.prune,data_sub)
  #how much error are we off by(Of the values that are NOT NA for comparison on the technique itself)
  sqrt(round(mean((data[,col] - pred.col)^2,na.rm=T),3))
  col.impute <- as.data.frame(predict(col.prune,data_sub))
  #modify column name so it is unique to each column name
  colnames(col.impute) <- paste0(col,'.','impute.', mkt)
  #provide the indexes at which that particular column returns 0
  c <- which(data_sub[,col]==0)
  message(length(c))
  #only when there is 1 or more indexes, will need to impute
  if(length(c) > 0){
    #impute at c(index for row), col name
    data_sub[c,col] <- col.impute[c,1]
  }
  
  #put imputed column into dataframe
  df_int <- as.data.frame(data_sub[,col])
  #modify dataframe column name
  colnames(df_int) <- col
  #return column
  df_int
}

#test for function itself on 1 column
#data_sub references 2nd function to split my market
impute_col(data,'Chicago', "KPI1")



#This function will read in each market and read in impute_col() to impute cols if index is missing
#.x will be pulled from cols

impute_JM2 <- function(data, mkt){
  
  #filter by mkt
  ##<< since it is within function
  data_sub <<- data%>%
    filter(data$market==mkt)
  
  #columns to select from, skip first 2 (date and mkt)
  cols <- as.list(colnames(data_sub)[3:ncol(data_sub)])
  
  #map_dfc: map dataframe column
  #similar to apply, maps every col through impute_col() and impute if needed
  #df_int will store every column inputed, no need for for loop
  df_int <- map_dfc(cols, ~impute_col(data_sub, mkt, .x))
  df_int$Date <- data_sub$Date#seq(as.Date(min(data_sub$datetime)), as.Date(max(data_sub$datetime)), 'days')
  df_int$market <- mkt
  df_int %>%
    select(Date, market, everything())
}

mkts <- as.list(unique(data$market))
#use row based instead so each iteration(mkt) will stack
#.x will be in place of mkts to impute each market and stack by row
final <- map_dfr(mkts, ~impute_JM2(data, .x))



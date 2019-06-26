library(BatchGetSymbols)
library(zoo)
# import stock data
df.SP500 <- GetSP500Stocks()
print(df.SP500$tickers)
tickers <- df.SP500$tickers
freq.data <- 'monthly'
first.date <- "2012-01-01"
last.date <- "2016-12-31"

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date, freq.data = freq.data)
table <- l.out$df.tickers

# import ff data
ff_data <-  read.csv("F-F_Research_Data_5_Factors_2x3.CSV", header=TRUE)
# initialize an empty dataframe to store returned data
df <- data.frame(ticker=character(),
                 date = double(),
                 alpha=double(),
                 beta=double())
colnames(df)<-c("ticker","date","alpha","beta")


tck <- unique(table$ticker)
k=1 
for( i in tck) # i = "AAPL"
{  
  tryCatch(
    {
      sub_table <- table[table$ticker==i,] 
      
      for(t in 1:36) #t=15
      {
        # subsetting fama-french data every two years
        # print(t)
        sub_data <- ff_data[t:(t+23),]
        SMB<- sub_data$SMB[-1]
        HML <- sub_data$HML[-1]
        RMW <- sub_data$RMW[-1]
        CMA <- sub_data$CMA[-1]
        Rf <- sub_data$RF[-1]
        ExR <- sub_data$Mkt.RF[-1]
        # subsetting stock data every two years
        rollingdata <- sub_table[t:(t+23),]
        reg_date <- rollingdata$ref.date
        # run the regression on rolling data
        r.price <- rollingdata$ret.adjusted.prices
        r.price <- r.price[-1]
        reg <- lm((r.price-Rf)~ExR+SMB+HML+RMW+CMA)
        #nrow(sub_data)
        ## get beta and intercept, return and store them into a table
        alpha <- summary(reg)$coef[,1][1]
        beta <- summary(reg)$coef[,1][2]
        temp = data.frame(i,reg_date[23],alpha, beta)
        # update df in loop
        df = rbind(df,temp)
      }
      
      cat(k,i,"done","\n")
      k=k+1
    },
    error=function(e){
      cat("ERROR: ",i,"\n")
    })
}

#############################################################
# for each date get mean beta and alpha
# get weights by sorting stocker with beta larger than mean and smaller than mean
# 
ddt <- unique(df$reg_date)

# rank df based on beta in ascending order
sub_beta <-  df[order(df$reg_date,df$beta),]
# attach sorted rank
sub_beta$rank = seq.int(nrow(sub_beta))

# get grand average
temp=na.omit(sub_beta)
which(is.na(sub_beta))
grand_avg <- mean(temp$beta)

df_stock <- data.frame(ticker=character(),
                 alpha_avg=double(),
                 beta_avg=double())
colnames(df_stock)<-c("ticker","alpha_avg","beta_avg")

for(s in tck)
{ # s = "AAPL"
  # get df for a particular stock
  sub_stock <- sub_beta[sub_beta$i==s,]
  # get average beta and alpha for that stock
  stock_avg_beta <- mean(sub_stock$beta)
  stock_avg_alpha <- mean(sub_stock$alpha)
  # store them into a new df
  tempStock = data.frame(s,stock_avg_alpha, stock_avg_beta)
  df_stock = rbind(df_stock,tempStock)
}
# rank 
df_stock <-  df_stock[order(df_stock$stock_avg_beta),]
df_stock$rank = seq.int(nrow(df_stock))

# get weights 
top_stock <- na.omit(df_stock[1:50,])
low_stock <- na.omit(df_stock[(nrow(df_stock)-49):nrow(df_stock),]) # assume naive weights
sum_top <- sum(top_stock$stock_avg_beta)
sum_low <- sum(low_stock$stock_avg_beta)
positionTop <- sum_low/(sum_low-sum_top) # set portfolio beta to zero
positionLow <- 1-positionTop
alpha_top <- mean(top_stock$stock_avg_alpha)*positionTop
alpha_low <- mean(low_stock$stock_avg_alpha)*positionLow
df_portfolio <- rbind(top_stock,low_stock)

########################################################
# testing and evaluating prediction
test.start <- "2016-01-01"
test.end <- "2017-12-31"

l.test <- BatchGetSymbols(tickers = tickers,
                         first.date = test.start,
                         last.date = test.end, freq.data = freq.data)
test.table <- l.test$df.tickers



## portfolio return sum product of subsections weights and its return
## subsection return: sum of return individual stocks / 50

# k="GT"

return_top <- 0
return_low <- 0
df_return <- data.frame(month=double(),
                        port_r=double())
for(m in c(1:24))
{ #m=1
  for (k in df_portfolio$s)
  {
    # k = "GT"
    if(df_portfolio[df_portfolio$s==k,]$rank<51)
    {
      return_top <- return_top+(test.table[test.table$ticker==k,]$ret.adjusted.prices)*positionTop/50
    }
    else
    {
      return_low <- return_low+(test.table[test.table$ticker==k,]$ret.adjusted.prices)*positionLow/50
    }
    
    
  }
  temp_return <- return_top+return_low
  df_return[m,1] = m
  df_return[m,2] = temp_return[m]
  # df_return <- data.frame(df_return$month=m,  df_return$port_r[month==m] = (df_return$port_r[month==m]+temp_return[m]))
}



plot(df_return)



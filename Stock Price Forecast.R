library(here)
library(fUnitRoots)
library(quantmod)
library(vars)

df1 <- read.csv(here("TWITTER.csv"))
head(df1)
df1 <- data.frame(df1)
#plot(ts(df1))
str(df1)
df2 <- df1


#### convert column to date class
df2$Date <- as.Date(df2$Date)

#### Generating time series plot for variables
Open_Price_xts <- xts(df2$Open, order.by=df2$Date)
High_Price_xts <- xts(df2$High, order.by=df2$Date)
Low_Price_xts <- xts(df2$Low, order.by=df2$Date)
Close_Price_xts <- xts(df2$Close, order.by=df2$Date)
Adj.Close_price_xts <- xts(df2$Adj.Close, order.by=df2$Date)
Volume_qnty_xts <- xts(df2$Volume, order.by=df2$Date)

plot(Open_Price_xts)
plot(High_Price_xts)
plot(Low_Price_xts)
plot(Close_Price_xts)
plot(Adj.Close_price_xts)
plot(Volume_qnty_xts)


#### Creating data frame with time series data

stock_data <- data.frame(Open_Price = Open_Price_xts[,1],
                         High_Price = High_Price_xts[,1], 
                         Low_Price = Low_Price_xts[,1], 
                         Close_Price = Close_Price_xts[,1], 
                         Adjusted_Price = Adj.Close_price_xts[,1], 
                         Stock_Qnty = Volume_qnty_xts[,1])
head(stock_data)

#### Split the dataset (70% & 30%)
length(stock_data$Open_Price)
train_set <- data.frame(stock_data[1:1520,])
test_set <- data.frame(stock_data[1521:2172,])

#### Testing the stationary
# Hypothesis: Ho : The process is not stationary 
# Assumption : This is partially random walk process
adfTest(train_set$Open_Price, type = "nc") # for Open Price ;Not stationary
adfTest(train_set$High_Price, type = "nc") # for High Price ;Not stationary
adfTest(train_set$Low_Price, type = "nc") # for Low Price ;Not stationary
adfTest(train_set$Close_Price, type = "nc") # for Close Price ;Not stationary
adfTest(train_set$Adjusted_Price, type = "nc") # for Adjusted Price ;Not stationary
adfTest(train_set$Stock_Qnty, type = "nc") # for Volume Qnty ;Not stationary

#### Creating diffrenced series
DTrain_Open <- diff(train_set$Open_Price,1)
DTrain_High <- diff(train_set$High_Price,1)
DTrain_Low <- diff(train_set$Low_Price,1)
DTrain_Close <- diff(train_set$Close_Price,1)
DTrain_Adjusted <- diff(train_set$Adjusted_Price,1)
DTrain_Volume <- diff(train_set$Stock_Qnty,1)

DOpen_Price_xts <- xts(DTrain_Open, order.by=df2$Date[2:1520])
DHigh_Price_xts <- xts(DTrain_High, order.by=df2$Date[2:1520])
DLow_Price_xts <- xts(DTrain_Low, order.by=df2$Date[2:1520])
DClose_Price_xts <- xts(DTrain_Close, order.by=df2$Date[2:1520])
DAdj.Close_price_xts <- xts(DTrain_Adjusted, order.by=df2$Date[2:1520])
DVolume_qnty_xts <- xts(DTrain_Volume, order.by=df2$Date[2:1520])

plot(DClose_Price_xts)

D_stock_Traindata <- data.frame(D_Open_Price = DOpen_Price_xts[,1],
                         D_High_Price = DHigh_Price_xts[,1], 
                         D_Low_Price = DLow_Price_xts[,1], 
                         D_Close_Price = DClose_Price_xts[,1], 
                         D_Adjusted_Price = DAdj.Close_price_xts[,1], 
                         D_Stock_Qnty = DVolume_qnty_xts[,1])

#### Testing the stationary of differenced series
adfTest(D_stock_Traindata$D_Open_Price, type = "nc") # for Open Price ;stationary
adfTest(D_stock_Traindata$D_High_Price, type = "nc") # for High Price ;stationary
adfTest(D_stock_Traindata$D_Low_Price, type = "nc") # for Low Price ;stationary
adfTest(D_stock_Traindata$D_Close_Price, type = "nc") # for Close Price ;stationary
adfTest(D_stock_Traindata$D_Adjusted_Price, type = "nc") # for Adjusted Price ;stationary
adfTest(D_stock_Traindata$D_Stock_Qnty, type = "nc") # for Volume Qnty ;stationary

#### Selecting the lag length
VARselect(D_stock_Traindata, lag.max = 8, type = "const")

#### Fitting the model
varM1 <- VAR(D_stock_Traindata, p=1, type = "const")
varM1

VarM.ser <- restrict(varM1, method = "ser")
VarM.ser

#### Checking the model adequacy #### 
serial.test(VarM.ser,lags.pt = 12)

forecast(varM1, h=14)

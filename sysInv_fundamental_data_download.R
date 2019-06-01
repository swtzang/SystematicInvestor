###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
rm(list=ls())
#
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)


###############################################################################
# determine date when fundamental data is available
# use 'date preliminary data loaded' when available
# otherwise lag 'quarter end date' 2 months for Q1/2/3 and 3 months for Q4
###############################################################################     
date.fund.data <- function(data)
{
  # construct date
  quarter.end.date = as.Date(paste(data['quarter end date',], '/1', sep=''), '%Y/%m/%d')  
  quarterly.indicator = data['quarterly indicator',]
  date.preliminary.data.loaded = as.Date(data['date preliminary data loaded',], '%Y-%m-%d') + 1
  
  months = seq(quarter.end.date[1], tail(quarter.end.date,1)+365, by='1 month') 
  index = match(quarter.end.date, months)
  quarter.end.date = months[ iif(quarterly.indicator == '4', index+3, index+2) + 1 ] - 1
  
  fund.date = date.preliminary.data.loaded
  fund.date[is.na(fund.date)] = quarter.end.date[is.na(fund.date)] 
  
  return(fund.date)
}

#*****************************************************************
# Load historical fundamental data
# http://advfn.com/p.php?pid=financials&symbol=NYSE:WMT&mode=quarterly_reports
#****************************************************************** 
Symbol = 'NYSE:WMT'
fund = fund.data(Symbol, 80)

# construct date
fund.date = date.fund.data(fund)    

#*****************************************************************
# Create and Plot Earnings per share
#****************************************************************** 
EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
EPS.Q = as.xts(EPS.Q, fund.date)    
EPS = runSum(EPS.Q, 4)

# Plot
layout(1:2)
par(mar=c(2,2,2,1))
x = barplot(EPS.Q, main='Wal-Mart Quarterly Earnings per share', border=NA)
text(x, EPS.Q, fund['quarterly indicator',], adj=c(0.5,-0.3), cex=0.8, xpd = TRUE)

barplot(EPS, main='Wal-Mart Rolling Annual Earnings per share', border=NA)

#
# Next let’s align Wal-Mart prices and EPS and plot them on the same graph.
#*****************************************************************
# Load historical data
#****************************************************************** 
load.packages('quantmod')
tickers = 'WMT'

data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1980-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)

data$WMT = merge(data$WMT, EPS)
# back fill EPS
data$WMT$EPS = ifna.prev(coredata(data$WMT$EPS))    

# Plot
y = data$WMT['1990::']
plota(Cl(y), type = 'l', LeftMargin=3)

plota2Y(y$EPS, type='l', las=1, col='red', col.axis = 'red')

plota.legend('WMT(rhs),WMT.EPS(lhs)', 'blue,red', list(Cl(y),y$EPS))
#
#Next let’s repeat the above steps for all companies in the Dow Jones index.
#*****************************************************************
# Load historical data
#****************************************************************** 
library(tidyquant)
library(XML)
library(RCurl)

tq_exchange('amex')

load.packages('quantmod') 
#tickers = dow.jones.components()
#
url = 'http://finance.yahoo.com/q/cp?s=^DJI+Components'
#txt = join(readLines(url))
#temp = extract.table.from.webpage(txt, 'Volume', has.header = T)
#tickers = temp[, 'Symbol']
#return(tickers)
#
#pop<-readHTMLTable(url, which = 1)
#
#tabs <- getURL(url)
#tabs <- readHTMLTable(tabs, stringsAsFactors = F)
#
library(httr)
tabs <- GET(url)
tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)
class(tabs)
tickers<-tabs[[1]]$Symbol


# get fundamental data
data.fund <- new.env()
temp = paste(iif( nchar(tickers) <= 3, 'NYSE:', 'NASDAQ:'), tickers, sep='')
for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
save(data.fund, file='D:/亞洲大學上課資料/Asia teaching materials/Data base management 2019/data.fund.Rdata')
load('D:/亞洲大學上課資料/Asia teaching materials/Data base management 2019/data.fund.Rdata')
names(data.fund)
dim(data.fund$AAPL)
aapl.mtx<-data.fund$AAPL
aapl.tbl<-as_tibble(data.fund$AAPL, rownames = "Acc_name")
glimpse(aapl.tbl)


aapl.df<-as.data.frame(data.fund$AAPL) 
tail(aapl.df, 2)
# get pricing data
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)    
save(data, file='D:/亞洲大學上課資料/Asia teaching materials/Data base management 2019/data.Rdata')
load('D:/亞洲大學上課資料/Asia teaching materials/Data base management 2019/data.Rdata')
names(data)
class(data$AAPL)
tail(data$AAPL)



#load(file='data.fund.Rdata')
#load(file='data.Rdata')


# combine fundamental and pricing data
tickers
i = 'AAPL'
for(i in tickers) {
  fund = data.fund[[i]]
  fund.date = date.fund.data(fund)
  
  EPS.Q = as.double(fund['Diluted EPS from Total Operations',])
  EPS.Q = as.xts(EPS.Q, fund.date)    
  EPS = runSum(EPS.Q, 4)
  
  data[[i]] = merge(data[[i]], EPS)
}

bt.prep(data, align='keep.all', dates='1995::2018')

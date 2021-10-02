require("httr");require("rvest");require("xml2");require("quantmod");require("stringr")

# ticker to get BL
ticker = "AMZN"

# this function will get you the latest quarterly BL (5 quarters)
getLatestBL = function(ticker)
{
  url = paste0("https://www.barchart.com/stocks/quotes/",ticker,
               "/balance-sheet/quarterly")
  pg = read_html(url)
  
  df = pg %>% html_nodes("table") %>% html_table() %>% as.data.frame()
  VAL = pg %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                       "div/div[2]/div/div/div/div[3]/div[1]",
                                       "/span/span")) %>% html_text()
  
  if(VAL == "thousands"){VAL = "000"}
  # change 1st value of df - for colname change
  df[1,1] <- "Description"
  colnames(df) <- df[1,]
  # subset df to remove colnames
  df = df[2:nrow(df),]
  # TOTAL appears 5 different times
  df$Description[str_detect(df$Description,"TOTAL")] <- c("Total Current Assets",
                                                          "Total Non-Current Assets",
                                                          "Total Current Liabilities",
                                                          "Total Non-Current Liabilities",
                                                          "Total Shareholder's Equity")
  # get rid of special characters
  df[,2] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,2])))
  df[,3] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,3])))
  df[,4] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,4])))
  df[,5] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,5])))
  df[,6] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,6])))
  
  # add VAL to each numeric variable in BL statement otherwise NA
  for(ii in 2:ncol(df)){
    df [,ii]<- as.numeric(suppressWarnings(ifelse(is.na(as.numeric(df[,ii])*0), 
                                                  df[,ii], 
                                                  as.numeric(paste0(df[,ii],VAL))
    )
    )
    )
  }
  # remove ALL empty rows
  df = df[!(rowSums(is.na(df)) == 5),]
  # remove duplicated rows
  df <- unique(df) %>% as.data.frame()
  # make sure it is data.frame
  df <- data.frame(df[,2:6],row.names = df$Description)
  # return data.frame
  df
}
# test function
pg1 = getLatestBL(ticker=ticker)

# this function will get you 9 pages of quarterly BL (45 quarters)
getRestBL = function(ticker)
{
  urls = paste0("https://www.barchart.com/stocks/quotes/",ticker,
                "/balance-sheet/quarterly?reportPage=",paste(2:10))
  
  pg = lapply(as.list(1:length(urls)), function(ii){
    # 3 second script sleep
    Sys.sleep(3)
    pg = try(read_html(urls[ii]),silent = TRUE)
    if(!inherits(pg,'try-error'))
      pg
  })
  # remove empty lists - removes pages without content
  pg = pg[lapply(as.list(1:length(pg)), function(ii) pg[[ii]] %>% html_nodes("table") %>% length) > 0]
  # extract tables
  df = lapply(as.list(1:length(pg)), function(ii){
    
    df = pg[ii][[1]] %>% html_nodes("table") %>% html_table() %>% as.data.frame()
    VAL = pg[ii][[1]] %>% html_nodes(xpath=paste0("/html/body/main/div/div[2]/div[2]/",
                                                  "div/div[2]/div/div/div/div[3]/div[1]",
                                                  "/span/span")) %>% html_text()
    
    if(VAL == "thousands"){VAL = "000"}
    # change 1st value of df - for colname change
    df[1,1] <- "Description"
    colnames(df) <- df[1,]
    # subset df to remove colnames
    df = df[2:nrow(df),]
    # TOTAL appears 5 different times
    df$Description[str_detect(df$Description,"TOTAL")] <- c("Total Current Assets",
                                                            "Total Non-Current Assets",
                                                            "Total Current Liabilities",
                                                            "Total Non-Current Liabilities",
                                                            "Total Shareholder's Equity")
    # get rid of special characters
    df[,2] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,2])))
    df[,3] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,3])))
    df[,4] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,4])))
    df[,5] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,5])))
    df[,6] <- gsub("N/A",NA,gsub("\\$","",gsub("\\,","",df[,6])))
    
    # add VAL to each numeric variable in CF statement otherwise NA
    for(ii in 2:ncol(df)){
      df [,ii]<- as.numeric(suppressWarnings(ifelse(is.na(as.numeric(df[,ii])*0), 
                                                    df[,ii], 
                                                    as.numeric(paste0(df[,ii],VAL))
      )
      )
      )
    }
    # remove ALL empty rows
    df = df[!(rowSums(is.na(df)) == 5),]
    # remove duplicated rows
    df <- unique(df) %>% as.data.frame()
    # make sure it is data.frame
    df <- data.frame(df[,2:6],row.names = df$Description)
    # return data.frame
    df
  })
  # merge by row.names
  df2 = merge(df[[1]],df[[2]],by="row.names", all=TRUE)
  df2 = data.frame(df2[,2:ncol(df2)], row.names = df2[,1])
  # merge by row names and convert to data.frame
  for(ii in 3:length(df)){
    df2 = merge(df2,df[[ii]],by="row.names", all=TRUE) %>% suppressWarnings()
    df2 = data.frame(df2[,2:ncol(df2)], row.names = df2[,1])
  }
  # return df2
  df2
}
# test function
PGS = getRestBL(ticker=ticker)
# combine tables & merge by row.names
BL = merge(pg1,PGS,by="row.names", all=TRUE)
BL = data.frame(BL[,2:ncol(BL)], row.names = BL[,1])

# add BL ratios
getBLRatios = function(BL,ticker)
{
  # ***************************************************************************************
  #                                     add STK PRC
  # ***************************************************************************************
  # get data convert to quarterly and extract closing prices
  tmp = Cl(to.quarterly(getSymbols(ticker, auto.assign = FALSE, from="2000-01-01")))
  # convert BL - names to Quarterly timestamps
  Qdates = as.yearqtr(names(BL)[1:length(BL)], format="X%m.%Y")
  # order is decreasing
  Qdates = Qdates[order(Qdates,decreasing = TRUE)]
  # convert stk prices to as.data.frame
  stk = as.data.frame(t(tmp[Qdates]), row.names = NULL)
  # run through each column to extract stk price at the end of each Quarter
  toR <- as.data.frame(NA)
  for(ii in 1:ncol(BL))
  {
    # converts to yearqtr for each column
    QQ = as.yearqtr(names(BL)[ii], format="X%m.%Y")
    # subset desired Quarter price
    prc = try(stk[,paste(QQ)],silent = TRUE)
    if(inherits(prc,'try-error'))
    {
      # add NA if no stk Price is available
      toR = cbind(toR,NA)
    }else{
      # otherwise add stock price
      toR = cbind(toR,prc)  
    }
  }
  # data frame// format rows
  toR <- data.frame(toR[,2:ncol(toR)], row.names = "Stock Price")
  colnames(toR) <- names(BL)
  # ***************************************************************************************
  #                                       Current Ratio
  # ***************************************************************************************
  currentR = round(as.numeric(BL["Total Current Assets",])/
                   as.numeric(BL["Total Current Liabilities",]),4) %>% t %>% as.data.frame
  # data frame// format rows
  currentR <- data.frame(currentR, row.names = "Current Ratio")
  colnames(currentR) <- names(BL)
  # ***************************************************************************************
  #                                        Quick Ratio
  # ***************************************************************************************
  quickR = round((as.numeric(BL["Total Current Assets",])-as.numeric(BL["Inventories",]))/
                     as.numeric(BL["Total Current Liabilities",]),4) %>% t %>% as.data.frame
  # data frame// format rows
  quickR <- data.frame(quickR, row.names = "Quick Ratio")
  colnames(quickR) <- names(BL)
  # ***************************************************************************************
  #                                       Total Debt 2 Equity Ratio
  # ***************************************************************************************
  debt2Eqt = round((as.numeric(BL["Total Liabilities",])/
                      as.numeric(BL["Total Shareholder's Equity",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  debt2Eqt <- data.frame(debt2Eqt, row.names = "Total Debt-to-Equity Ratio")
  colnames(debt2Eqt) <- names(BL)
  # ***************************************************************************************
  #                                       Long-Term Debt 2 Equity Ratio
  # ***************************************************************************************
  Ltdebt2Eqt = round((as.numeric(BL["Total Non-Current Liabilities",])/
                      as.numeric(BL["Total Shareholder's Equity",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  Ltdebt2Eqt <- data.frame(Ltdebt2Eqt, row.names = "Long-Term Debt-to-Equity Ratio")
  colnames(Ltdebt2Eqt) <- names(BL)
  # ***************************************************************************************
  #                                       Short-Term Debt 2 Equity Ratio
  # ***************************************************************************************
  Stdebt2Eqt = round((as.numeric(BL["Total Current Liabilities",])/
                        as.numeric(BL["Total Shareholder's Equity",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  Stdebt2Eqt <- data.frame(Stdebt2Eqt, row.names = "Short-Term Debt-to-Equity Ratio")
  colnames(Stdebt2Eqt) <- names(BL)
  # ***************************************************************************************
  #                                        Working Capital
  # ***************************************************************************************
  workingK = round((as.numeric(BL["Total Current Assets",])-
                    as.numeric(BL["Total Current Liabilities",])),4) %>% t %>% as.data.frame
  # data frame// format rows
  workingK <- data.frame(workingK, row.names = "Working Capital")
  colnames(workingK) <- names(BL)
  # ***************************************************************************************
  #                                        Working Capital/Share
  # ***************************************************************************************
  workKPerShr = round((as.numeric(BL["Total Current Assets",])-
                         as.numeric(BL["Total Current Liabilities",]))/
                        as.numeric(BL["Shares Outstanding, K",]),4) %>% t %>% as.data.frame
  # data frame// format rows
  workKPerShr <- data.frame(workKPerShr, row.names = "Working Capital Per Share")
  colnames(workKPerShr) <- names(BL)
  # ***************************************************************************************
  #                                     Rowbind -> Output
  # ***************************************************************************************
  ALL = rbind(toR,currentR,quickR,Stdebt2Eqt,Ltdebt2Eqt,debt2Eqt,workingK,workKPerShr)
  
  ALL
}
# test function
RATIOS = getBLRatios(BL=BL,ticker=ticker)
# rowbind with BL
BL = rbind(BL,RATIOS)

# write table as csv
write.table(BL,paste0("~/Desktop/",ticker,"_BL.csv"),sep = ",")


# transpose table
BLt = as.data.frame(t(BL))
# write table as csv
write.table(BLt,paste0("~/Desktop/",ticker,"_BLT.csv"),sep = ",")


library(lubridate)

option_types = c("Call", "Put")
position_types = c("Long", "Short")

option <- function(underlying, call, long, premium, strike, maturity, quantity=1){
  # Inputs:
  #   underlying - String Specifying the ticker of the underlying asset
  #   call - T if option is a call, F if put
  #   long - T if options is long, F if short
  #   premium - Specifies the market premium on the option
  #   strike - Strike price of the option
  #   maturity - Date object in the future for which the option expires
  #   quantity - Number of options owned (affects the payoff structure)
  
  if( !(call %in% c(T, F)) || !(long %in% c(T, F)) ){
    stop("call or long arguments are not of type Boolean")
  }
  if( (long && premium > 0) || (!long && premium < 0) ){
    premium = -1*premium
  }
  
  maturity <- as_date(maturity)
  opt_type_index = 1 + !call
  pos_type_index = 1 + !long
  
  option.type = option_types[opt_type_index]
  position.type = position_types[pos_type_index]
  
  retList <- list(underlying=underlying,
                  option.type=option.type,
                  position.type=position.type,
                  isCall=call,
                  isLong=long,
                  premium=premium,
                  strike=strike,
                  expiration.date=maturity,
                  quantity=quantity
                  )
  return(retList)
}

{
c1 <- option("SPY", call=T, long=T, 
             premium = -5, strike = 50, 
             maturity = Sys.Date()+months(1), quantity=1)
c2 <- option("SPY", call=T, long=F, 
             premium = -5, strike = 50, 
             maturity = Sys.Date()+months(1), quantity=1)
p1 <- option("SPY", call=F, long=T, 
             premium = -5, strike = 50, 
             maturity = Sys.Date()+months(1), quantity=1)
p2 <- option("SPY", call=F, long=F, 
             premium = -5, strike = 50, 
             maturity = Sys.Date()+months(1), quantity=1)
}

opt_set1 = list(c1, c2, p1, p2)

get_option_table <- function(option_set){
  # Option set is a vector of options
  option_table <- data.frame()
  
  for(i in 1:length(option_set)){
    option_table <- rbind(option_table, option_set[[i]], stringsAsFactors=FALSE)
  }
  return(option_table)
}

dfSet1 <- get_option_table(opt_set1)

plot_option <- function(option_obj, line_color="red", new_plot=TRUE){
  TerminalSpot <- 100
  quantity <- option_obj$quantity
  totalPremium <- option_obj$premium*quantity
  strike <- option_obj$strike
  xCoords <- c(0, option_obj$strike, TerminalSpot)
  yCoords <- c(totalPremium,
               totalPremium,
               TerminalSpot*quantity-strike-totalPremium)
  
  if(!option_obj$isLong){
    yCoords[3] <- yCoords[3]*-1
  }
  
  if(!option_obj$isCall){
    #yCoords[3] <- -1*((strike*quantity)-totalPremium)
    yCoords <- rev(yCoords)
  }
  
  {
  if(new_plot){
    a <- plot(NULL,NULL,xlim=c(0, 100), 
           ylim=c(-50, 50), 
           xlab="Spot at time T", 
           ylab="P/L ($)", 
           main=paste(option_obj$quantity, "x",option_obj$position.type, option_obj$option.type,"on", option_obj$underlying), new=new_plot)
    abline(h=0, v=0, new=FALSE)
  }
  lines(x=xCoords, y=yCoords, col=line_color, lwd="2", new=FALSE)
  
  }
  return(a)
}

# Plot Put/Call, Long/Short options
{
plot_option(c1) #Long Call
plot_option(c2) #Short Call
plot_option(p1) #Long Put
plot_option(p2) #Short Put
}

get_strategy_table <- function(option_frame){
  unique_k <- unique(option_frame$strike)
  nColumns <- length(unique_k) + 1
  nRows <- nrow(option_frame)
  c.names <- c(sprintf("K%d", 1:(nColumns-1)), "inf")
  r.names <- sprintf("Option %d", 1:nRows)

  tempMatrix <- matrix(data = rep(x=c(0), nRows*nColumns), nrow=nRows, ncol=nColumns)
  tempFrame <- data.frame(tempMatrix)
  rownames(tempFrame) <- r.names
  colnames(tempFrame) <- c.names
  
  totalPremium <- option_frame$premium %*% option_frame$quantity
  TerminalSpot <- 100
  
  for(i in 1:nRows){
    yCoord <- NA
    
    if(option_frame[i,]$isCall){
      yCoord <- (TerminalSpot*option_frame[i,]$quantity) - option_frame[i,]$strike
    } else {
      yCoord <- option_frame[i,]$strike
    }
    
  }
  
  return(tempFrame)
}
get_strategy_table(dfSet1)






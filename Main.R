source("Option Strategy Visualizer.R")

{
  c1 <- option("SPY", call=T, long=T, 
               premium = -5, strike = 60, 
               maturity = Sys.Date()+months(1), quantity=1)
  c2 <- option("SPY", call=F, long=T, 
               premium = 5, strike = 55, 
               maturity = Sys.Date()+months(1), quantity=1)
  p1 <- option("SPY", call=F, long=T, 
               premium = -5, strike = 50, 
               maturity = Sys.Date()+months(1), quantity=1)
  p2 <- option("SPY", call=F, long=F, 
               premium = 5, strike = 55, 
               maturity = Sys.Date()+months(1), quantity=1)

  opt_set1 = list(c1, c2, p1, p2)  
}



dfSet1 <- get_option_table(opt_set1)
dfSet1

{
plot_option(c1) #Long Call
plot_option(c2) #Short Call
plot_option(p1) #Long Put
plot_option(p2) #Short Put
}

{
sTable <- get_strategy_table(dfSet1[,])
sTable
plot_strategy(sTable$strategy.frame, sTable$strikes)
}


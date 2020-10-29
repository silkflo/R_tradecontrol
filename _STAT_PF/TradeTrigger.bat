@echo off
:: Use this line of code for debugging purposes *** Adapt the paths for R installation and R script!!!
::"C:\Program Files\R\R-4.0.2\bin\R.exe" CMD BATCH "E:\trading\Git\R_tradecontrol\_STAT_PF\TradeTrigger.R"
:: Use this code in 'production'
"C:\Program Files\R\R-4.0.2\bin\Rscript.exe" "E:\trading\Git\R_tradecontrol\_STAT_PF\TradeTrigger.R"

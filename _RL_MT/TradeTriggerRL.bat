@echo off
:: Use this line of code for debugging purposes *** Adapt the paths for R installation and R script!!!
::"C:\Program Files\R\R-3.5.0\bin\R.exe" CMD BATCH "C:\Users\fxtrams\Documents\000_TradingRepo\R_tradecontrol\_RL_MT\TradeTriggerRL.R"
:: Use this code in 'production'
"C:\Program Files\R\R-3.5.0\bin\Rscript.exe" "C:\Users\fxtrams\Documents\000_TradingRepo\R_tradecontrol\_RL_MT\TradeTriggerRL.R"
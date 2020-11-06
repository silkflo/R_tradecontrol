# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# Copyright (C) 2018 Vladimir Zhbanko

# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# NOTE:    Results are triggered by writing to the file of the MT4 Trading Terminal

# packages used *** make sure to install these packages
library(tidyverse) #install.packages("tidyverse")
library(lubridate) #install.packages("lubridate") 

# ----------- Applied Logic -----------------
# -- Read trading results from Terminal 2
# -- Split trading results from Terminal 2 into categories using profit factor
# -- Depend on conditions of LAST 10 trades in Terminal 2, allow trades in Terminal 3
#   -- on last 10 trades in T2
#   -- enable T3 when Profit factor of 10 trades > 2
#   -- disable T3 when Profit Factor of 10 trades < 1.6
# -- Start/Stop trades on Terminals at MacroEconomic news releases (will be covered in Course #5)

# ----------- TESTS -----------------
# -- Select entire content of the script and execute
# -- Pass: Data object DFT1 contains observations
# -- Pass: xxx
# -- Pass: Files SystemControlXXXXXXX.csv are generated in the Terminal 3 sandbox
# -- Pass: If file "01_MacroeconomicEvent.csv" exists trade policy is overwritten
# -- Fail: DFT1 class 'try-error'
# -- Fail: xxx
# ----------------
# Used Functions
#-----------------
# *** make sure to customize this path
source("E:/trading/Git/R_tradecontrol/import_data.R")
source("E:/trading/Git/R_tradecontrol/get_profit_factorDF.R")
source("E:/trading/Git/R_tradecontrol/writeCommandViaCSV.R")

# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 2 path *** make sure to customize this path 
#(when run in live instead of EA path might be ./MQL4/Files)
path_T2 <- "C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/Files/"
# terminal 3 path *** make sure to customize this path
path_T3 <- "C:/Program Files (x86)/AM MT4 - Terminal 3/MQL4/Files/"

# -------------------------
# read data from trades in terminal 2
# -------------------------
# # uncomment code below to test functionality without MT4 platform installed
# DFT1 <- try(import_data(trade_log_file = "_TEST_DATA/OrdersResultsT1.csv",
#                         demo_mode = T),
#             silent = TRUE)

DFT2 <- try(import_data(path_T2, "OrdersResultsT2.csv"),silent = TRUE)


# get last 10 trades for each Magic system and arrange orders to have descending order
DFT2_L <- DFT2 %>%  # filtered to contain last 10 orders for each system
  group_by(MagicNumber) %>% 
  arrange(MagicNumber, desc(OrderCloseTime)) %>% 
  filter(row_number() <= 11) # +1 for the function to work
  
# ----------------
# Implementation of logic
#-----------------
#### SORTING AND DECIDE TRADING ON THE DEMO ACCOUNT #### -----------------------------
# DEMO always allow trades in Terminal 2                               
DFT2_L %>%
  group_by(MagicNumber) %>%
  summarise(nOrders = n()) %>%
  select(MagicNumber) %>%
  mutate(IsEnabled = 1) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T2)

#### DECIDE IF TRADING ON THE T3 ACCOUNT #### -----------------------------
# Last 10 orders on DEMO && pr.fact >= 2 start trade T3
DFT2_L %>%
  get_profit_factorDF(10) %>% 
  ungroup() %>% 
  filter(PrFact >= 1.2) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 100, IsEnabled = 1) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T3)

#### DECIDE IF NOT TO TRADING ON THE T3 ACCOUNT #### -----------------------------
# 4. Last 10 orders on DEMO && pr.fact < 1.6 stop trade T3
DFT2_L %>%
  get_profit_factorDF(10) %>% 
  ungroup() %>% 
  filter(PrFact < 1.2) %>% 
  select(MagicNumber) %>% 
  mutate(MagicNumber = MagicNumber + 100, IsEnabled = 0) %>% 
  # Write command "allow"
  writeCommandViaCSV(path_T3)


ifelse(get_profit_factorDF(DFT2_L,10)< 1.2, flag<-1, flag <-0)


#write_rds(DFT1_L, "test_data_profit_factorDF.rds")
##========================================
# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT3 <- try(import_data(path_T3, "OrdersResultsT3.csv"),silent = TRUE)

# -------------------------
# stopping all systems when macroeconomic event is present
# this will be covered in the Course #5 of the Lazy Trading Series!
# -------------------------
##========================================
# -------------------------
# stopping all systems when macroeconomic event is present
# this will be covered in the Course #5 of the Lazy Trading Series!
# -------------------------


if(file.exists(file.path(path_T2, "01_MacroeconomicEvent.csv"))& flag = 0){
  DF_NT <- (read_csv(file= file.path(path_T2, "01_MacroeconomicEvent.csv"), col_types = "ici"))
  DF_NT  <- as.data.frame(DF_NT) %>% filter(DF_NT$Flag == 1)
  
  #if(DF_NT[1,1] == 1) {
    # disable trades
    if(!class(DF_NT)[1]=='try-error'){
      DF_NT %>%
        group_by(MagicNumber) %>%
        select(MagicNumber) %>% 
        mutate(IsEnabled = 0) %>% 
        # write commands to disable systems
        writeCommandViaCSV(path_T2)
      }
    if(!class(DFT3)[1]=='try-error' && !class(DF_NT)[1]=='try-error'){
      DF_NT %>%
          group_by(MagicNumber) %>% 
          inner_join(DFT3, by = "MagicNumber") %>%
          select(MagicNumber) %>% 
          mutate(IsEnabled = 0) %>% 
          writeCommandViaCSV(path_T3)}
    
    
 # }
  # enable systems of T1 in case they were disabled previously
 # if(DF_NT[1,1] == 0) {
  DF_NT <- read_csv(file= file.path(path_T2, "01_MacroeconomicEvent.csv"), col_types = "ici") 
  DF_NT  <- as.data.frame(DF_NT) %>% filter(DF_NT$Flag == 0)
      # enable trades
    if(!class(DFT2)[1]=='try-error'){
      DF_NT %>%
        group_by(MagicNumber) %>% 
        select(MagicNumber) %>% 
        mutate(IsEnabled = 1) %>% 
        # write commands to disable systems
        writeCommandViaCSV(path_T2)}
    # in this algorithm SystemControl file must be enabled in case there are no MacroEconomic Event
    if(!class(DFT3)[1]=='try-error' && !class(DF_NT)[1]=='try-error'){
      DF_NT %>%
        group_by(MagicNumber) %>% 
        inner_join(DFT3, by = "MagicNumber") %>%
        select(MagicNumber) %>% 
        mutate(IsEnabled = 1) %>% 
        writeCommandViaCSV(path_T3)}
    
#  }
  
}

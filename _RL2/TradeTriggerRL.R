# This is a dedicated script for the Lazy Trading 4th Course: Statistical Analysis and Control of Trades
# Copyright (C) 2018 Vladimir Zhbanko
# Preferrably to be used only with the courses Lazy Trading see: https://vladdsm.github.io/myblog_attempt/index.html
# https://www.udemy.com/your-trading-control-reinforcement-learning/?couponCode=LAZYTRADE4-10
# PURPOSE: Analyse trade results in Terminal 1 and Trigger or Stop Trades in Terminal 3
# DETAILS: Trades are analysed and RL model is created for each single Expert Advisor
#        : Q states function is calculated, whenever Action 'ON' is > than 'OFF' trade trigger will be active   
#        : Results are written to the file of the MT4 Trading Terminal
#        : Using dummy dataframe for the initial learning of the RL model

# packages used *** make sure to install these packages
library(tidyverse) 
library(lubridate) 
library(ReinforcementLearning) 
library(magrittr)

# ----------- Main Steps -----------------
# -- Read trading results from Terminal 1
# -- Rearrange data for RL
# -- Perform Reinforcement Learning or Update Model with New Data
# -- Start/Stop trades in Terminal 3 based on New Policy
# -- Start/Stop trades on Terminals at MacroEconomic news releases (will be covered in Course #5)

# ----------- TESTS -----------------
# -- Select entire content of the script and execute
# -- Pass: Data object DFT1 contains observations
# -- Pass: DFT1_sum contains trades summary
# -- Pass: Files SystemControlXXXXXXX.csv are generated in the Terminal 3 sandbox
# -- Pass: If file "01_MacroeconomicEvent.csv" exists trade policy is overwritten
# -- Fail: DFT1 class 'try-error'
# -- Fail: xxx

# ----------------
# Used Functions (to make code more compact). See detail of each function in the repository
#-----------------
# *** make sure to customize this path
source("E:/trading/Git/R_tradecontrol/import_data.R") 
source("E:/trading/Git/R_tradecontrol/_RL2/generate_RL_policy.R")
source("E:/trading/Git/R_tradecontrol/_RL2/record_policy.R")
source("E:/trading/Git/R_tradecontrol/writeCommandViaCSV.R")
source("E:/trading/Git/R_tradecontrol/_RL2/Adapt_RL_control.R")
 
# -------------------------
# Define terminals path addresses, from where we are going to read/write data
# -------------------------
# terminal 1 path *** make sure to customize this path
path_T2 <- "C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/Files/"

# terminal 3 path *** make sure to customize this path
path_T3 <- "C:/Program Files (x86)/AM MT4 - Terminal 3/MQL4/Files/"

# path where to read control parameters from
path_control_files = "E:/trading/Git/R_tradecontrol/_RL2/control"

# -------------------------
# read data from trades in terminal 1
# -------------------------
# uncomment code below to test functionality without MT4 platform installed
# DFT1 <- try(import_data(trade_log_file = "_TEST_DATA/OrdersResultsT1.csv",
#                         demo_mode = T),
#             silent = TRUE)
DFT2 <- try(import_data(path_T2, "OrdersResultsT2.csv"), silent = TRUE)
# -------------------------
# read data from trades in terminal 3
# -------------------------
DFT3 <- try(import_data(path_T3, "OrdersResultsT3.csv"), silent = TRUE)

# Vector with unique Trading Systems
vector_systems <- DFT2 %$% MagicNumber %>% unique() %>% sort()

# For debugging: summarize number of trades to see desired number of trades was achieved
DFT2_sum <- DFT2 %>% 
  group_by(MagicNumber) %>% 
  summarise(Num_Trades = n(),
            Mean_profit = sum(Profit)) %>% 
  arrange(desc(Num_Trades))

# create rds file needed with the alpha,gamma , epsilon best value into control folder
# WARNING : this function take a lot of time to be executed 
# if there is no control variab le existing execute this function
# Adapt_RL_control(DFT2,path_T2,path_control_files)
### ============== FOR EVERY TRADING SYSTEM ###
for (i in 1:length(vector_systems)) {
  # tryCatch() function will not abort the entire for loop in case of the error in one iteration
  tryCatch({
    # execute this code below for debugging:
    # i <- 1 #policy off
    # i <- 2 #policy on
    # i <- 3
    
    # extract current magic number id
  trading_system <- vector_systems[i]
  # get trading summary data only for one system 
  trading_systemDF <- DFT2 %>% filter(MagicNumber == trading_system)
  
  ## -- Go to the next Loop iteration if there is too little trades! -- ##
  if(nrow(trading_systemDF) < 5) { next }
  

  #==============================================================================
  # Define state and action sets for Reinforcement Learning
  states <- c("tradewin", "tradeloss")
  actions <- c("ON", "OFF") # 'ON' and 'OFF' are referring to decision to trade with Slave system
  
  # Define reinforcement learning parameters (see explanation below or in vignette)
  # -----
  # alpha - learning rate      0.1 <- slow       | fast        -> 0.9
  # gamma - reward rate        0.1 <- short term | long term   -> 0.9
  # epsilon - sampling rate    0.1 <- high sample| low sample  -> 0.9
  # iter 
  # ----- 
  #control <- list(alpha = 0.3, gamma = 0.6, epsilon = 0.1)
  # Use optimal control parameters found by auxiliary function
 
  control <- read_rds(paste0(path_control_files,"/", trading_system, ".rds"))
  #control <- read_rds(paste0(path_control_files,"/", 8118102, ".rds"))
  
  # perform reinforcement learning and return policy
  policy_tr_systDF <- generate_RL_policy(trading_systemDF, states = states,actions = actions,
                                         control = control)
  # get the latest trade of that system (will be used to match with policy of RL)
  latest_trade <- DFT2 %>% 
    filter(MagicNumber == trading_system) %>% 
    arrange(desc(OrderCloseTime)) %>% 
    mutate(NextState = ifelse(Profit>0, "tradewin",
                              ifelse(Profit<0, "tradeloss", NA)),
           Reward =  Profit,
           State = NextState) %>% head(1) %$% NextState
  
  # record policy to the sandbox of Terminal 3, this should be analysed by EA
  record_policy(x = policy_tr_systDF, last_result = latest_trade, trading_system = trading_system, path_sandbox = path_T3)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}
### ============== END of FOR EVERY TRADING SYSTEM ###



##========================================
# -------------------------
# stopping all systems when macroeconomic event is present
# this will be covered in the Course #5 of the Lazy Trading Series!
# -------------------------
if(file.exists(file.path(path_T2, "01_MacroeconomicEvent.csv"))){
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
  
}

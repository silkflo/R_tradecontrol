#' Function to log RL progress. Function will record Q values during updating of the model
#'
#' @param x - dataframe containing trading results
#' @param states 
#' @param actions 
#' @param control 
#'
#' @return
#' @export
#'
#' @examples
#'
#' require(magrittr)
#'  source("E:/trading/Git/R_tradecontrol/import_data.R") 
#'  path_T2 <- "C:/Program Files (x86)/AM MT4 - Terminal 2/MQL4/Files/"
#'  DFT2 <- try(import_data(path_T2, "OrdersResultsT2.csv"), silent = TRUE)
#'  states <- c("tradewin", "tradeloss")
#'  actions <- c("ON", "OFF")
#'  control <- list(alpha = 0.7, gamma = 0.3, epsilon = 0.1)
#' 
#' vector_systems <- DFT2 %$% MagicNumber %>% unique() %>% sort()

#' DFT2_sum <- DFT2 %>%
#'  group_by(MagicNumber) %>% 
#'  summarise(Num_Trades = n(),
#'            Mean_profit = sum(Profit)) %>% 
#'  arrange(desc(Num_Trades))
#' 
#' 
#'  trading_system1 <- vector_systems[1]
#'  trading_systemDF1 <- DFT2 %>% filter(MagicNumber == trading_system1)
#'  x <- trading_systemDF1
#' 
#'  trading_system2 <- vector_systems[2]
#'  trading_systemDF2 <- DFT2 %>% filter(MagicNumber == trading_system2)
#'  y <- trading_systemDF2
#' 
#' 
log_RL_progress <- function(x, states, actions, control){
  
  require(tidyverse)
  require(ReinforcementLearning)
  require(magrittr)
  
  
  # add dummy tupples with states and actions with minimal reward
  d_tupple <- data.frame(State = states,
                         Action = rep(actions,length(states)),
                         Reward = rep(0,length(states)),
                         NextState = states,
                         stringsAsFactors = F)
  # generate RL model
  model <- ReinforcementLearning(d_tupple, s = "State", a = "Action", r = "Reward", 
                                 s_new = "NextState",iter = 1, control = control)
  
  # add rows of the x one by one to gradually update this model
  
  
  
  for (i in 2:nrow(x)) {
    # i <- 6
    # State 
    State <- x[i-1,] %>% mutate(State = ifelse(Profit>0, "tradewin", ifelse(Profit<0, "tradeloss", NA))) %$% State
    
    # predict on i
    Action <- computePolicy(model)[State]
    
    # reward
    Reward <- x[i,]$Profit
    
    # next state
    NextState <- x[i, ] %>% mutate(State = ifelse(Profit>0, "tradewin", ifelse(Profit<0, "tradeloss", NA))) %$% State
    # combine data as dataframe
    i_tupple <- data.frame(State,Action,Reward,NextState,row.names = i, stringsAsFactors = F) %>%
      # change factor column to as.character (required by RL function)
      mutate_if(is.factor, as.character)
    # join dummy tupple to current row in the new object
    if(!exists("df_tupple")){df_tupple <- bind_rows(d_tupple, i_tupple)} else {
      df_tupple <- bind_rows(df_tupple, i_tupple)
    }
    
    # update model with new data tupple
    model <- ReinforcementLearning(df_tupple, s = "State", a = "Action", r = "Reward",
                                   s_new = "NextState", control = control, iter = 1, model = model)
    #model$Q
    
      # generate dataframe with reward sequence of this learning
      df_q <- data.frame(alpha = control$alpha, gamma = control$gamma, epsilon = control$epsilon, 
                         trstate = model$States, rewardseq = model$Q, totreward = model$Reward)
    
      # create dataframe that will append data to previous records
      if(!exists("df_Q")){df_Q <- df_q} else {
        df_Q <- bind_rows(df_Q, df_q)
      }
      
    #print(i)
    
  }
  
  
  #plot(model)
  # return log of RL model
  return(df_Q)
  
}

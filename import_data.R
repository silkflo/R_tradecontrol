#' Import Data to R. Function imports file and change data column type
#'
#' @param path_terminal - path of the Trading Terminal where the file with data is written
#' @param trade_log_file - File name where the order results are written
#' @param demo_mode - When true function uses data stored in the _TEST_DATA folder
#'
#' @author (C) 2018 Vladimir Zhbanko
#'
#' @return Function will return the dataframe with trade data
#' @export
#'
#' @examples
import_data <- function(path_terminal, trade_log_file, demo_mode = F){
  ### uncomment for debugging of this function
  #path_terminal <- "C:/Program Files (x86)/AM MT4 - Terminal 2/tester/files/"
  #trade_log_file <- "OrdersResultsT2.csv"
  # trade_log_file <- "_TEST_DATA/OrdersResultsT1.csv"
  # demo_mode <- T
  require(tidyverse)
  require(lubridate)
  
  if(!demo_mode){
  
  DFT2 <- try(read_csv(file = file.path(path_terminal, trade_log_file), 
                       col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                                     "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                       col_types = "iiccdci"),
              silent = TRUE)
  
  } else {
    
    
    DFT2 <- try(read_csv(file = trade_log_file, 
                         col_names = c("MagicNumber", "TicketNumber", "OrderStartTime", 
                                       "OrderCloseTime", "Profit", "Symbol", "OrderType"),
                         col_types = "iiccdci"),
                silent = TRUE)
  }
  
  if(class(DFT2)[1] == "try-error") {stop("Error reading file. File with trades may not exist yet!",
                                       call. = FALSE)}
  if(!nrow(DFT2)==0){
    # data frame preparation
    DFT2$OrderStartTime <- ymd_hms(DFT2$OrderStartTime)
    DFT2$OrderCloseTime <- ymd_hms(DFT2$OrderCloseTime)
    DFT2$OrderType      <- as.factor(DFT2$OrderType)
    # removes duplicates
    DFT2 <- unique(DFT2)
    print("DFT2 initialized")
    
    return(DFT2)
  } else {
    stop("No trades executed so far. Trade data log is empty!",
         call. = FALSE)
    }

  
}
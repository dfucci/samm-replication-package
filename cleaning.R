options(warn = -1)
library(janitor)
library(readxl)
library(tidyverse)
library(stringr)
library(purrr)
library(dplyr)
library(lubridate)

# This function results in wrong value due to how time cell are handled on different
# operating system. You will have to manually calculate time using the information
# reported in the SAMM questionnaire xlsx file
transform_time <- function(start, end){
  convert <- function(t){
    x <- as.numeric(t)*24 
    minutes <- round((t %% 1)*60, digits = 0) 
    hours <- round(x - minutes/60, digits = 0)
    if (minutes < 10){ #if minutes is a single digit need to insert a preceding 0
      minutes= paste0("0",minutes)
    }
    result <- paste0(hours, ":", minutes)
    return(hm(result))
  }
  
  if (is.numeric(start) & is.numeric(end) & !is.na(start) & !is.na(end)) {
    start <- convert(start)
    end <- convert(end)
    time <- period_to_seconds(end-start)/60
    return(time)
  } else {
    return(0)
  }
}

clean_raw_samm <- function(questionnaire) {
  SAMM <- read_excel(questionnaire, sheet = "Interview",  col_names = FALSE, skip = 13)
  SAMM <- SAMM %>% dplyr::select(-1, -3, -5, -11) # removes unnecessary columns
  name <- SAMM %>% dplyr::select(2) # hacky way of selecting single cell from tibble
  name <- name$...4[1]
  name <- str_replace_all(name, " ", "_")
  column_i <- SAMM %>% dplyr::select(6) # column I in the spreadsheet
  start <- as.numeric(column_i$...9[1]) #time start
  end <- as.numeric(column_i$...9[219]) #time end
  time <- transform_time(start, end)
  
  role <- column_i$...9[220] 
  
  SAMM <- SAMM %>% slice(4:(n()-1))
  SAMM <- SAMM %>% 
    row_to_names(1) %>% 
    clean_names() %>% 
    rename(answer_numeric = na, answer_avg = na_2, difficulty = na_3, confidence = na_4, question = strategy_metrics)
  SAMM <- SAMM %>% filter(str_detect(question, "\\?$"))
  SAMM <- SAMM %>% mutate(answer = if_else(is.na(answer), "NO ANSWER", answer))  
  SAMM <- SAMM %>% fill(c(stream, rating, difficulty, confidence))
  SAMM <- SAMM %>% mutate(respondent = name)
  SAMM <- SAMM %>% mutate(role = role)
  SAMM <- SAMM %>% mutate(time = time)
  SAMM <- SAMM %>% mutate(comments = str_replace_all(comments, "\\r\\n", " "))
  filename <- str_interp("clean/SAMM_${name}_${time}.csv")
  SAMM <- list(SAMM, filename)
  return(SAMM)
  # SAMM %>% write.csv2(file = filename, row.names = FALSE, sep = ",")
}
save_to_csv <- function(d) {
  write.csv(d[[1]], file = d[[2]], row.names = F, sep = ",")
  }
raw_files <- list.files(path = "data/survey", pattern = "*.xlsx", full.names = T)
clean_samm_dfs <- map(raw_files, clean_raw_samm)
map(clean_samm_dfs, save_to_csv)

# merge all answers
data_all <- list.files(path = "clean/",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows  
results_file <- "results/dataset/answers.csv"
write.csv(data_all, results_file)
workshop_file <-  "data/workshop/overall_SAMM.xlsx"
clean_workshop <- clean_raw_samm(workshop_file)
write.csv(clean_workshop[[1]], file = "clean/workshop/SAMM.csv")


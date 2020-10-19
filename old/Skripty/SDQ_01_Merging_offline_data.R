library(tidyverse) #need for data manipulation

#----1 az 5 vlna----
setwd("C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/orig")

df_master = read.csv("C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/orig/data_all_incl_w5_postClean.csv") #read the main file
df_master = mutate_all(df_master, as.character) #transform everything to characters, same reason as above

#----6 vlna---
setwd ("C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/orig/6vlna") # target the folder with data

wave6 <- lapply(Sys.glob("*.csv"), read_csv2) # read in all data files
wave6 = lapply(wave6, mutate_all, as.character) #transform everything to characters, because bind_rows is a wimp afraid of variable types not matching
wave6 = bind_rows(wave6) #merges individual dataframes in the list into one


#----7 vlna---

setwd("C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/orig/7vlna") # target the folder with data

wave7 <- lapply(Sys.glob("*.csv"), read_csv2) # read in all data files
wave7 = lapply(wave7, mutate_all, as.character) #transform everything to characters, because bind_rows is a wimp afraid of variable types not matching
wave7 = bind_rows(wave7) #merges individual dataframes in the list into one


#---Spojeni datasetu---
df_fin = bind_rows(df_master, wave6, wave7)

#---Cisteni datasetu----
df_fin = df_fin %>% filter(is.na(wave) == F) %>% select(-c("id_old", "age_deprecated", "nch", "X40",
                                                                 "na_count_all", "na_count_sdq"))

df_fin$id_class = as.factor(df_fin$id_class) #changes id_class back to factor
df_fin = df_fin %>% mutate_if(is.character, as.integer) #changes everything else back to integers

remove(df_master, wave6, wave7)

#---saving dataset----
write_rds(x = df_fin, path = "C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/SDQ_offline.rds")

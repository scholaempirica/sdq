###_______________________ read in and basic clean

#__________________________read in

setwd ("data_input_wave5") # target the folder with data

filenames <- list.files()  # check the files available

# using read_csv2 from readr rather than read.csv2 creates tibbles rather than data frames
# and prevents issues with encoding
dataFiles <- lapply(Sys.glob("*.csv"), read_csv2) # read in all data files


# the following reads splits data into individual DFs
for (i in 1:length(dataFiles)){
  if(i<=9){
    df_temp <- dataFiles[[i]]
    colnames(df_temp)[1] <- "region" # for some datasets, the name was "ï..region", IDK why 
    assign(paste0("df0",i), df_temp)
  } 
  else{
    df_temp <- dataFiles[[i]]
    colnames(df_temp)[1] <- "region"
    assign(paste0("df",i), df_temp)
  }
}


# __________ the follwoing chunk only needs to be once with every new patch of data

# usually, the separator is ";", when not, the dataset only has 1 variable 
# I have changed this mannualy by opening and saving the file with the right separator
lapply(Sys.glob("*.csv"), list) # I used this to identify which .csv need to be changed
# __________ 


#____________________cleaning individual datasets
# issue 1: datasets have different number of variables
# issue 2: somes have been created electronically - different coding of values AND variables
# issue 3: some control classes from the first wave stopped being control classes
# issue 4: there are some missing values for classes
# issue 5: need to check in physical questionnaires the teacher coding for school 1

#____ issue 1
names(df01) # 39 vars - this is correct
names(df11) # 42 vars - old vars, can stay for future check
names(df12) # 43 vars - old vars, can stay for future check

#____ issue 2, specifficaly in wave 5 schools 6 and 7, i.e. dfs 34, 36
head(df34)
head(df36)

#first solve the issue with colname duplicity
colnames(df34)[1] <- "timestamp"
colnames(df36)[1] <- "timestamp"

df34 <- rbind (df34, df36) # merge files from the online questionnaire to recode them together
rm(df36)

df34$timestamp <- NULL #this is redundant, participants filled in month and year manully
df34$techaer_name <- NULL #there is id_teacher in the data set, name redundant 


# recoding gender

table(df34$gender_girl, useNA = "ifany")
df34$gender_girl <- as.numeric(recode(df34$gender_girl, Chlapec = 0L, Dívka = 1L))
str(df34$gender_girl)

# recoding SDQ by position (!!! check positions !!!)

apply(df34[15:39],2, function(x) table(x, useNA = "ifany")) # look at your tables before recoding

# the recoding is wierd, because encoding still causes problems for "Definitivně pravda"
# solution - .default option is used for "Definitivně pravda"
# missing are not affected by the default setting

df34 <- df34 %>% 
  mutate_at(vars(15:39), 
            funs(
              recode(., `Není pravda` = 0, `Tak trochu pravda` = 1, .default = 2)))

# split the data files again, this also gets rid of the imported empty rows

str(df34$id_school)
table(df34$id_school, useNA = "ifany")

df34c <- df34
df34 <- df34c[df34c$id_school=="06" & !is.na(df34c$id_school),]
df36 <- df34c[df34c$id_school=="07" & !is.na(df34c$id_school),]
# View(df34)
# View(df36)
rm(df34c)

#_____ before issue 3: time to merge all files

# first, create a list of data frames

dataFiles2 = list() # prepare empty list

# the following only works if the number of DFs is the same as in folder
# it creates a list containing all the datasets
# it also transforms variable class where there are issues
# check that with future patches of data

for (i in 1:length(dataFiles)){      
  if(i<=9){
    df_temp <- eval(parse(text=paste0("df0",i)))
    df_temp$id_school <- as.character(df_temp$id_school) # character in some of the data frames
    df_temp$tlies <- as.character(df_temp$tlies) # character in some of the data frames
    df_temp$id_teacher <- as.character(df_temp$id_teacher) # character in some of the data frames
    df_temp$id_or_name <- as.character(df_temp$id_or_name) # character in some of the data frames
    df_temp$born_month <- as.character(df_temp$born_month) # character in some of the data frames
    dataFiles2[[i]] <- df_temp
  } 
  else{
    df_temp <- eval(parse(text=paste0("df",i)))
    df_temp$id_school <- as.character(df_temp$id_school)
    df_temp$tlies <- as.character(df_temp$tlies)
    df_temp$id_teacher <- as.character(df_temp$id_teacher) 
    df_temp$id_or_name <- as.character(df_temp$id_or_name) 
    df_temp$born_month <- as.character(df_temp$born_month)
    dataFiles2[[i]] <- df_temp
  }
}

# summary(dataFiles2)
# str(dataFiles2)

# now bind them, THIS STEP MIGHT CAUSE PROBLEMS IS THERE ARE OTHER VARIABLES IN THE FUTURE 
# WHERE SOME ARE NUMERIC AND SOME CHARACTER - need to be changed in the above loop

df_master <- bind_rows(dataFiles2, .id = "id_df")


str(df_master)

#__________ make a copy
# df_master_copy <- df_master
# rm(df_master); df_master <- df_master_copy

# are there empty rows in the data?
nas = sapply (df_master, function (x) sum(is.na(x)))
nas[nas>0]
min(nas[nas>0])

# View(df_master[is.na(df_master$region),]) # check it is all really empty rows

df_master = df_master[!is.na(df_master$region),] # these are all empty rows

#____ issue 3, some control classes from the first wave stopped being control classes

# identify data frame with mix of classes - both intervention and

df_master %>% group_by(id_school, wave) %>% 
  summarise(intervention_mean = mean(is_intervention, na.rm=T), n = n()) %>% 
  as.data.frame()

# this requires to check schools in the following waves:
 
# s1000w5 - ok
# s14w5 - ok
# s15w5 - ok
# s16w5 - ok
# s17w5 - ok
# s18w5 - ok

# s9w3 - training from the CKP - no longer control school
# s8w3 - training from the CKP - no longer control school
# s7w3 - training from the CKP - no longer control school
### for the three above: can we understand the period between the first two measurements as control?

# s4w3 - one school divided by classes but control classes became intervention classes soon - RECODE
# s5w3 - one school divided by classes but control classes became intervention classes soon - RECODE

df_master$is_intervention2 <- df_master$is_intervention
df_master$is_intervention2[(df_master$id_school == 4 & df_master$wave == 3)|
                             (df_master$id_school == 5 & df_master$wave == 3)|
                             (df_master$id_school == 7 & df_master$wave == 3)|
                             (df_master$id_school == 8 & df_master$wave == 3)|
                             (df_master$id_school == 9 & df_master$wave == 3)] <- 1

df_master %>% group_by(id_school, wave) %>% 
  summarise(intervention_mean = mean(is_intervention2, na.rm=T), n = n()) %>% 
  as.data.frame()


######################################## UNUSED

################### UNSED BUT INTERESTING

# recoding multiple data sets using a loop

# for (i in c(34, 36)) {
#   nam_temp <- eval(parse(text=paste0("df",i,"c")))
#   nam_temp$gender_girl <- as.numeric(recode(nam_temp$gender_girl, Chlapec = 0L, Dívka = 1L))
#   assign(paste0("df",i,"c"), nam_temp)
# }





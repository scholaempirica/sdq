library(tidyverse)

df_fin = read_rds("C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/SDQ_offline.rds")
df_elektro_fin = read_rds("C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/SDQ_online.rds")


#----Merging offline and online data----
df_fin$id_class = as.character(df_fin$id_class)

df_master = bind_rows(df_fin, df_elektro_fin)

#---Cleaning and transforming----
df_master = df_master %>% mutate(is_intervention2 = ifelse(is.na(is_intervention2) == T,
                                                           is_intervention,
                                                           is_intervention2)) #unifies group membership indicator

df_master = df_master %>% mutate(id_teacherM = ifelse(is.na(id_teacherM) == T, id_teacher, id_teacherM))

df_master <- df_master[!is.na(df_master$id_or_name),] #deletes NAs in id_or_name

df_master$id_pupil = paste(
  sprintf("%02d",df_master$id_school),
  sprintf("%04d",df_master$id_or_name), sep = "-") #combines school and pupil id into an unique pupil id

df_master$id_teach_unq = paste(
  sprintf("%02d",df_master$id_school),
  sprintf("%03d",df_master$id_teacherM), sep = "-") #combines school and teacher id into an unique teacher id

#===Reversing scale items===
df_master$uobeys <- car::recode (df_master$tobeys, "0=2; 1=1; 2=0; else=NA")
df_master$ureflect <- car::recode (df_master$treflect, "0=2; 1=1; 2=0; else=NA")
df_master$uattends <- car::recode (df_master$tattends, "0=2; 1=1; 2=0; else=NA")
df_master$ufriend <- car::recode (df_master$tfriend, "0=2; 1=1; 2=0; else=NA")
df_master$upopular <- car::recode (df_master$tpopular, "0=2; 1=1; 2=0; else=NA")

#===Computing scales===
#emotions
df_temotion <- data.frame(df_master$tsomatic, 
                          df_master$tworries, 
                          df_master$tunhappy, 
                          df_master$tclingy, 
                          df_master$tafraid) # data frame of five items for emotional symptoms
df_master$tnemotion <- apply(df_temotion, 1, function(x) sum(is.na(x))) # count NAs within the five items, create a variable in the original data set
df_master$temotion <- ifelse(df_master$tnemotion<3, rowMeans(df_temotion, na.rm=TRUE), NA) #count mean value for emotional symptoms for cases with fewer than 3 missing values
df_master$temotion10 <- as.numeric(df_master$temotion) * 5 #transform to 0 to 10 scale
df_master$temotion10 <- floor(0.5 + df_master$temotion10) #Gets rid of decimal spaces


#conduct
df_tconduct <- data.frame(df_master$ttantrum, 
                          df_master$uobeys, 
                          df_master$tfights, 
                          df_master$tlies, 
                          df_master$tsteals)
df_master$tnconduct <- apply(df_tconduct, 1, function(x) sum(is.na(x)))
df_master$tconduct <- ifelse(df_master$tnconduct<3, rowMeans(df_tconduct, na.rm=TRUE), NA)
df_master$tconduct10 <- as.numeric(df_master$tconduct) * 5
df_master$tconduct10 <- floor(0.5 + df_master$tconduct10)

#hyperactivity symptoms - externalizing
df_thyper <- data.frame(df_master$trestles, 
                        df_master$tfidgety, 
                        df_master$tdistrac, 
                        df_master$ureflect, 
                        df_master$uattends)
df_master$tnhyper <- apply(df_thyper, 1, function(x) sum(is.na(x)))
df_master$thyper <- ifelse(df_master$tnhyper<3, rowMeans(df_thyper, na.rm=TRUE), NA)
df_master$thyper10 <- as.numeric(df_master$thyper) * 5
df_master$thyper10 <- floor(0.5 + df_master$thyper10)

#relations to peers symptoms - internalizing
df_tpeer <- data.frame(df_master$tloner, 
                       df_master$ufriend, 
                       df_master$upopular, 
                       df_master$tbullied, 
                       df_master$toldbest)
df_master$tnpeer <- apply(df_tpeer, 1, function(x) sum(is.na(x)))
df_master$tpeer <- ifelse(df_master$tnpeer<3, rowMeans(df_tpeer, na.rm=TRUE), NA)
df_master$tpeer10 <- as.numeric(df_master$tpeer) * 5
df_master$tpeer10 <- floor(0.5 + df_master$tpeer10)

#pro-social symptoms
df_tprosoc <- data.frame(df_master$tconsid, 
                         df_master$tshares, 
                         df_master$tcaring, 
                         df_master$tkind, 
                         df_master$thelpout)
df_master$tnprosoc <- apply(df_tprosoc, 1, function(x) sum(is.na(x)))
df_master$tprosoc <- ifelse(df_master$tnprosoc<3, rowMeans(df_tprosoc, na.rm=TRUE), NA)
df_master$tprosoc10 <- as.numeric(df_master$tprosoc) * 5
df_master$tprosoc10 <- floor(0.5 + df_master$tprosoc10)

remove(df_tconduct, df_temotion, df_thyper, df_tpeer, df_tprosoc) #gets rid of now unnecessary objects

#===Transforming variables into better types and creating aux. variables===
df_master$id_school = as.factor(df_master$id_school) #changes id_school into factor
df_master$gender_girl = factor(df_master$gender_girl, labels = c("boys", "girls")) #changes gender_girl into factors and adds labels
#df_master_omited = subset(df_master, !is.na(df_master$gender_girl)) #Creates dataframe without NAs in gender_girl (only needed for graphs with gender facets)

df_master <- left_join(df_master, 
                       df_master %>% group_by(id_pupil) %>% summarise(min_waves = min(wave)),
                       by = "id_pupil") #min_wave = first wave, where pupil appeared

df_master$wave_ind <- df_master$wave - df_master$min_waves + 1 #wave_ind = measurement number

df_master <- left_join(df_master, 
                       df_master %>% group_by(id_pupil) %>% summarise(wave_number = max(wave_ind)),
                       by = "id_pupil") #wave_number = total number of measurements the pupil participated in
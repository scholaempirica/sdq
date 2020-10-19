#################################################
# This script contains:
# - missing value analysis
# - constructing identifiers
# - deleting dupliates
# - recoding auxilary variables with meaningful names for analysis

#___________________________________________________________________
# explore missing values

# df_master_copy4 = df_master

# how many missing values per observation?
# sdq items
table(apply(df_master[,17:41], 1, function(x) sum(is.na(x))))
# all vars
table(apply(df_master, 1, function(x) sum(is.na(x))))



# explore

table(df_master$gender_girl, useNA = "ifany")  # 25 missing gender - can we read across waves
table (df_master$id_school, useNA = "ifany")
table (df_master$wave, useNA = "ifany")
table (df_master$id_teacherM, useNA = "ifany")

# a couple cases with missing teacher - set by original value
# df_master$id_teacherM[is.na(df_master$id_teacherM)] <- 
#   df_master$teacher_orig[is.na(df_master$id_teacherM)]

# one missing value within class - fill in
# df_master$id_teacherM[is.na(df_master$id_teacherM)] <- 8


### CONSTRUCT unique Identifiers
table(df_master$id_or_name, useNA = "ifany") # there are 6 NAs, not able to figure out
df_master <- df_master[!is.na(df_master$id_or_name),] # delete NAs in id_or_name

df_master$id_pupil = paste(
  sprintf("%02d",df_master$id_school),
  sprintf("%04d",df_master$id_or_name), sep = "-")

df_master$id_teach_unq = paste(
  sprintf("%02d",df_master$id_school),
  sprintf("%03d",df_master$id_teacherM), sep = "-")

# frequency of unique id_pupils
as.data.frame(table (df_master$id_pupil)) # df of no. of unique
table(as.data.frame(table (df_master$id_pupil))[,2]) # summary table


#look at those of which there are too many (more than 1 per wave)

# table (df_master$id_pupil)[table (df_master$id_pupil)>5] # old code

df_master %>% group_by(id_pupil, wave) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% head(n=20) # change head to see all bigger than 1

#####################################################
# deleting duplicates
# these are not complete duplicates, just in id_pupil
# therefore,  deleted are stored in df_dropped
#####################################################

# View(df_master[df_master$id_pupil=="01-0124" &
#                  df_master$wave==2,])

df_dropped <- subset(df_master, 
                     id_pupil=="01-0124" &
                       df_master$wave==2 &
                       df_master$tconsid==1)

df_master <- subset(df_master, 
                    !(id_pupil=="01-0124" &
                        df_master$wave==2 &
                        df_master$tconsid==1)) # delete one of two

###

df_dropped <- rbind(df_dropped, subset(df_master,
                                       id_pupil=="01-0180" &
                                         df_master$wave==3 &
                                         df_master$age_deprecated==6))

df_master <- subset(df_master,
                    !(id_pupil=="01-0180" &
                        df_master$wave==3 &
                        df_master$age_deprecated==6)) # delete one of two

###

df_dropped <- rbind(df_dropped, subset(df_master,
                                       id_pupil=="01-0263" &
                                         df_master$wave==3 &
                                         df_master$gender_girl==0))

df_master <- subset(df_master,
                    !(id_pupil=="01-0263" &
                        df_master$wave==3 &
                        df_master$gender_girl==0)) # delete one of two

df_dropped <- rbind(df_dropped, subset(df_master,
                                       id_pupil=="01-0263" &
                                         df_master$wave==4 &
                                         df_master$gender_girl==0))
df_master <- subset(df_master,
                    !(id_pupil=="01-0263" &
                        df_master$wave==4 &
                        df_master$gender_girl==0)) # delete one of two
###

df_dropped <- rbind(df_dropped, subset(df_master,
                                       id_pupil=="01-0318" &
                                         df_master$wave==5 &
                                         df_master$born_month==7))

df_master <- subset(df_master,
                    !(id_pupil=="01-0318" &
                        df_master$wave==5 &
                        df_master$born_month==7)) # delete one of two
###
df_dropped <- rbind(df_dropped, subset(df_master,
                                       id_pupil=="01-0431" &
                                         df_master$wave==5 &
                                         df_master$gender_girl==1))

df_master <- subset(df_master,
                    !(id_pupil=="01-0431" &
                        df_master$wave==5 &
                        df_master$gender_girl==1)) # delete one of two


# View(df_master[df_master$id_pupil=="05-0128",])

#####################################################
# end of deleting duplicates
# some duplicates in id_pupil are kept, because the id_pupil only appears in one wave
#####################################################



### In the past, follwoing changes were made manually directly to datasets
# this data (see list below) are deleted without back-up

# PROVEDENE AKCE na zaklade pohledu do papirovych dotazniku 
#(ZANESENE I DO ORIDGINALNICH DAT CSV VE SLOZCE DATA CLEAN)
# 04-0020 - jeden z duplikatu vymazan
# 04-0024 - age_depre == NA vymazano
# 05-0013 - age_depre == 5 vymazano
# 05-0077 - jeden z duplikatu vymazan
# 05-0078 - jeden z duplikatu vymazan
# 05-0079 - jeden z duplikatu vymazan


### Recoding auxilary variables

table(df_master$id_school, useNA = "ifany")

# this will create alphabaticl order of factor levels
df_master$id_school2 = car::recode(df_master$id_school, "
                                   1 = 'Steti';
                                   2 = 'Slunicko Roudnice';
                                   3 = 'Pisnicka Krupka';
                                   4 = 'Mozaika Jihlava'; 
                                   5 = 'Merickova Brno'; 
                                   6 = 'Dobromerice';
                                   7 = 'Kastanek Jirkov';
                                   8 = 'Louny';
                                   9 = 'Slunicko Usti';
                                   10 = 'Kyticka Liberec';
                                   11 = 'K Polabinam Pardubice';
                                   12 = 'Albertova HK';
                                   13 = 'Kampanova HK';
                                   14 = 'Trebes HK';
                                   15 = 'Ohrazenice Pardubice';
                                   16 = 'Slavetin';
                                   17 = 'Mozaika Kontrolni';
                                   18 = 'Lastuvkova Brno';
                                   99 = 'Kyticka Kontrolni'
                                   " )

# this will keep order of factor levels
df_master$id_school3 = dplyr::recode_factor(df_master$id_school,
                                            '1' = 'Steti',
                                            '2' = 'Slunicko Roudnice',
                                            '3' = 'Pisnicka Krupka',
                                            '4' = 'Mozaika Jihlava', 
                                            '5' = 'Merickova Brno', 
                                            '6' = 'Dobromerice',
                                            '7' = 'Kastanek Jirkov',
                                            '8' = 'Louny',
                                            '9' = 'Slunicko Usti',
                                            '10' = 'Kyticka Liberec',
                                            '11' = 'K Polabinam Pardubice',
                                            '12' = 'Albertova HK',
                                            '13' = 'Kampanova HK',
                                            '14' = 'Trebes HK',
                                            '15' = 'Ohrazenice Pardubice',
                                            '16' = 'Slavetin',
                                            '17' = 'Mozaika Kontrolni',
                                            '18' = 'Lastuvkova Brno',
                                            '99' = 'Kyticka Kontrolni')





table(df_master$gender_girl, useNA = "ifany")
df_master$gender_girl2 = dplyr::recode_factor(df_master$gender_girl, 
                                          '0' = 'chlapec',
                                          '1' = 'divka')


table(df_master$gender_girl2, useNA = "ifany")

table(df_master$age_deprecated, useNA = "ifany")

# df$age_deprecated2 = car::recode(df$age_deprecated, "2 = '2 az 3 roky'; 
#                                  3 = '2 az 3 roky'; 
#                                  4 = '4 roky';
#                                  5 = '5 let';
#                                  6 = '6 a vice';
#                                  7 = '6 a vice'" )



# age needs some special treatment here...

# df_help = df[df$wave == 3, c("id_pupil", "age_deprecated")]
# lookup <- unique(df_help)
# 
# df.AGE <- (merge(lookup, df, by = 'id_pupil'))
# rm(df_help, lookup)
# 
# table(df.AGE$age_deprecated.x, useNA = "ifany")
# df.AGE$age_deprecated2 = car::recode(df.AGE$age_deprecated.x, "2 = '2 az 3 roky'; 
#                                      3 = '2 az 3 roky'; 
#                                      4 = '4 roky';
#                                      5 = '5 let';
#                                      6 = '6 a vice';
#                                      7 = '6 a vice'" )
# table(df.AGE$age_deprecated2, useNA = "ifany")
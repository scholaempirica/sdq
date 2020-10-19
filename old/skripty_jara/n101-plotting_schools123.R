### Plotting school 1, 2, and 3 after 5 waves for 11-04-2019 session

# select working variables
df_work <- df_master %>% select(id_pupil, id_teach_unq, id_school, id_school2, id_school3,
                               temotion10,tconduct10, thyper10,tpeer10,tprosoc10, tebdtot40, 
                               wave,gender_girl2, is_intervention2, id_class, 
                               born_month, born_year, fill_in_month, fill_in_year, age_deprecated
                               )

# filter schools of interest

df_work <- df_work %>% dplyr::filter(as.numeric(as.character(id_school))<4)


# this shows: waves = 1 ... number of people observed in just 1 wave
# waves = 5 ... number of people observed across all five waves
df_work %>% group_by(id_pupil) %>% summarise(n_waves = n()) %>% group_by(n_waves) %>% summarise(obs=n())

# to get the number of waves per individual into the data frame, we need...

df_work <- left_join(df_work, 
                     df_work %>% group_by(id_pupil) %>% summarise(n_waves = n()),
                     by = "id_pupil")

# explore

df_work %>% group_by(id_school3, wave) %>% summarise(n=n())
df_work %>% group_by(id_school3, is_intervention2) %>% summarise(n=n())
table(df_work$id_school4, df_work$wave)
table(df_work$id_school4, df_work$wave_ind)


# for one word name

df_work$id_school4 = dplyr::recode_factor(df_work$id_school3,
                                            'Steti' = 'Steti',
                                            'Slunicko Roudnice' = 'Roudnice',
                                            'Pisnicka Krupka' = 'Krupka')

#####################################################
# Plotting
#####################################################


#####################################################
# Set 1 - all observations - i.e. results are NOT based on the same people in each wave
#####################################################



# To plot the results, we will need some data manipulation

# first - re-number waves from individuals perspective - x-th wave for me

# step 1 - find min value for wave variable and convert it into a new variable
df_work <- left_join(df_work, 
                     df_work %>% group_by(id_pupil) %>% summarise(min_waves = min(wave)),
                     by = "id_pupil")

# step 2 - construct the variable of interest

df_work$wave_ind <- df_work$wave - df_work$min_waves + 1 

# this removes NAs, always check how many
# IS IT A GOOD IDEA TO DO THIS?
# df_work_complete <- df_work[complete.cases(df_work),] 

# first results - show development in time for all avaible per wave_ind
df_work %>% group_by(wave_ind) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),
               funs(mean=mean(.,na.rm = T)))

df_work %>% group_by(wave_ind) %>% summarise(n=n())

g1_wave_ind <- left_join(df_work %>% group_by(wave_ind) %>% 
                           summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),
                                        funs(mean=mean(.,na.rm = T))),
                         df_work %>% group_by(wave_ind) %>% summarise(n=n()),
                         by = "wave_ind")

# from wide to long for plotting

g1_dat <- g1_wave_ind %>% gather(key = dimension, 
                                 value = value,
                                 temotion10_mean:tprosoc10_mean,
                                 factor_key=TRUE)

g1 <- ggplot (g1_dat, aes(x=as.factor(wave_ind), 
                                      y=value, group = dimension, color = dimension))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna", "3. vlna", "4. vlna","5. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  labs(color='Dimenze')


### BY SCHOOL

g2_school <- left_join(df_work %>% group_by(wave_ind, id_school4) %>% 
                           summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),
                                        funs(mean=mean(.,na.rm = T))),
                         df_work %>% group_by(wave_ind, id_school4) %>% summarise(n=n()),
                         by = c("wave_ind", "id_school4"))

g2_dat <- g2_school %>% gather(key = dimension, 
                                 value = value,
                                 temotion10_mean:tprosoc10_mean,
                                 factor_key=TRUE)

# use faceting

g2 <- ggplot (g2_dat, aes(x=as.factor(wave_ind), y=value, group = dimension, color = dimension))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna", "3. vlna", "4. vlna","5. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  facet_wrap(~id_school4)


### BY GENDER

# use faceting

g3_gender <- left_join(df_work %>% filter(!is.na(gender_girl2)) %>% group_by(wave_ind, gender_girl2) %>% 
                         summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),
                                      funs(mean=mean(.,na.rm = T))),
                       df_work %>% filter(!is.na(gender_girl2)) %>%
                         group_by(wave_ind, gender_girl2) %>% summarise(n=n()),
                       by = c("wave_ind", "gender_girl2"))

g3_dat <- g3_gender %>% gather(key = dimension, 
                               value = value,
                               temotion10_mean:tprosoc10_mean,
                               factor_key=TRUE)

# use faceting

g3 <- ggplot (g3_dat, aes(x=as.factor(wave_ind), y=value, group = dimension, color = dimension))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna", "3. vlna", "4. vlna","5. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  facet_wrap(~gender_girl2)


### BY GENDER AND SCHOOL


g4_gender_school <- left_join(df_work %>% filter(!is.na(gender_girl2)) %>% 
                                group_by(wave_ind, gender_girl2, id_school4) %>% 
                                summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),
                                             funs(mean=mean(.,na.rm = T))),
                              df_work %>% filter(!is.na(gender_girl2)) %>%
                                group_by(wave_ind, gender_girl2, id_school4) %>% summarise(n=n()),
                              by = c("wave_ind", "gender_girl2","id_school4"))

g4_dat <- g4_gender_school %>% gather(key = dimension, 
                               value = value,
                               temotion10_mean:tprosoc10_mean,
                               factor_key=TRUE)

g4 <- ggplot (g4_dat, aes(x=as.factor(wave_ind), y=value, group = dimension, color = dimension))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna", "3. vlna", "4. vlna","5. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  facet_wrap(~gender_girl2 + id_school4)

#####################################################
# Set 2 - all observations with at least four waves
#####################################################



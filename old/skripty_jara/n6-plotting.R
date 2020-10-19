### development in time

# ALL THREE SCHOOLS

df.waves = df %>% select(temotion10,tconduct10, thyper10,tpeer10,
           tprosoc10,wave, id_pupil, id_school2, gender2, id_teacher_unique)

# drop all pupils who are not in both waves (for now, it drops 4 rows)

df.waves = df.waves[df.waves$id_pupil %in% 
                      names(table (df.waves$id_pupil)[table (df.waves$id_pupil)==2]),]


# this removes NAs, always check how many
# IS IT A GOOD IDEA TO DO THIS?
df.waves = df.waves[complete.cases(df.waves),] 


df.waves.G1 = df.waves %>% group_by(wave) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T)

df.waves.G1 = reshape2::melt (df.waves.G1, id.vars = c ("wave"))
df.waves.G1$wave = as.factor(df.waves.G1$wave)

ggplot (df.waves.G1, aes(x=wave, y=value, group = variable, color = variable))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)

# ggplot (df.waves.G1, aes(x=variable, y=value, group = wave, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge", color="black")+
#   geom_point(size=3)+
#   labs (y="", x="")+
#   scale_x_discrete(labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
#                             "Pro-sociální"))+
#   ylim(0,10)
# 
# ggplot (df.waves.G1, aes(x=variable, y=value, group = wave, fill = variable))+
#   geom_bar(stat = "identity", position = "dodge", color="black")+
#   geom_point(size=3)+
#   labs (y="", x="")+
#   scale_x_discrete(labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
#                             "Pro-sociální"))+
#   ylim(0,10)


### BY SCHOOL

# use faceting

df.waves.G2 = df.waves %>% group_by(wave, id_school2) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T)

df.waves.G2 = reshape2::melt (df.waves.G2, id.vars = c ("wave", "id_school2"))
df.waves.G2$wave = as.factor(df.waves.G2$wave)

# hodnoty na grafu se prekryvaji - potreba upravit
df.waves.G2$value[7]=df.waves.G2$value[7]+0.05
df.waves.G2$value[10]=df.waves.G2$value[10]+0.05

ggplot (df.waves.G2, aes(x=wave, y=value, group = variable, color = variable))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  facet_wrap(~id_school2)



### BY GENDER

# use faceting

df.waves.G3 = df.waves %>% group_by(wave, gender2) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T)

df.waves.G3 = reshape2::melt (df.waves.G3, id.vars = c ("wave", "gender2"))
df.waves.G3$wave = as.factor(df.waves.G3$wave)


ggplot (df.waves.G3, aes(x=wave, y=value, group = variable, color = variable))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  facet_wrap(~gender2)


### BY GENDER AND SCHOOL


df.waves.G4 = df.waves %>% group_by(wave, gender2, id_school2) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T)

df.waves.G4 = reshape2::melt (df.waves.G4, id.vars = c ("wave", "gender2", "id_school2"))
df.waves.G4$wave = as.factor(df.waves.G4$wave)


ggplot (df.waves.G4, aes(x=wave, y=value, group = variable, color = variable))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  facet_wrap(~gender2+id_school2)


#provizorni
#provizorni
#provizorni
### BY AGE - provizorni - do budoucna je potreba zpracovat na zaklade mesice a roku narozeni
#provizorni
#provizorni
#provizorni

table(df.AGE$age_deprecated2, useNA = "ifany")

df.AGE.waves = df.AGE %>% select(temotion10,tconduct10, thyper10,tpeer10,
                         tprosoc10,wave, id_pupil, id_school2, gender2, age_deprecated2)

# drop all pupils who are not in both waves (for now, it drops 1 row)

df.AGE.waves = df.AGE.waves[df.AGE.waves$id_pupil %in% 
                      names(table (df.AGE.waves$id_pupil)
                            [table (df.AGE.waves$id_pupil)==2]),]


# this removes NAs, always check how many
df.AGE.waves = df.AGE.waves[complete.cases(df.AGE.waves),] 

table(df.AGE.waves$age_deprecated2, useNA = "ifany")


df.waves.G5 = df.AGE.waves %>% group_by(wave, age_deprecated2) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T)

df.waves.G5 = reshape2::melt (df.waves.G5, id.vars = c ("wave","age_deprecated2"))
df.waves.G5$wave = as.factor(df.waves.G5$wave)


ggplot (df.waves.G5, aes(x=wave, y=value, group = variable, color = variable))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  facet_wrap(~age_deprecated2)


# by teacher

# df.waves.G6 = df.waves %>% group_by(wave,  id_school2, id_teacher_unique) %>% 
#   summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T) %>% 
#   arrange(id_teacher_unique)
# 
# df.waves.G6 = reshape2::melt (df.waves.G6, id.vars = c ("wave", "id_teacher_unique", 
#                                                         "id_school2"))
# df.waves.G6$wave = as.factor(df.waves.G6$wave)
# 
# df.waves.G6 = reshape2::dcast(df.waves.G6,
#                               id_teacher_unique + id_school2 + variable ~ wave,
#                               value.var="value")
# 
# df.waves.G6 = df.waves.G6[complete.cases(df.waves.G6),]
# colnames(df.waves.G6)[4] = "third"
# colnames(df.waves.G6)[5] = "fourth"
# 
# 
# # ggplot (df.waves.G6, aes(x=wave, y=value, group = id_teacher_unique, color = variable))+
# #   geom_line(size=1)+
# #   geom_point(size=3)+
# #   labs (y="", x="")+
# #   scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
# #   scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
# #                                  "Pro-sociální"))+
# #   ylim(0,10)+
# #   facet_wrap(~id_school2)
# 
# 
# ggplot (df.waves.G6, aes(y=value, group = variable, color = variable))+
#   geom_segment(size=1)+
#   labs (y="", x="")+
#   scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
#   scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci",
#                                  "Pro-sociální"))+
#   ylim(0,10)+
#   facet_wrap(~id_school2)





###########
###########
###########
###########
########### unused material - but also good


# comparison by school produced chart by chart, then put together

df.waves.jihlava = df[df$id_school2 == "Mozaika Jihlava",] %>%
  select(temotion10,tconduct10, thyper10,tpeer10,tprosoc10,wave, id_pupil) %>%
  group_by(wave) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T)

df.waves.brno = df[df$id_school2 == "Merickova Brno",] %>%
  select(temotion10,tconduct10, thyper10,tpeer10,tprosoc10,wave, id_pupil)%>%
  group_by(wave) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T)

df.waves.dobro = df[df$id_school2 == "Dobromerice",] %>%
  select(temotion10,tconduct10, thyper10,tpeer10,tprosoc10,wave, id_pupil)%>%
  group_by(wave) %>% 
  summarise_at(vars(temotion10,tconduct10, thyper10,tpeer10,tprosoc10),mean, na.rm=T)

df.waves.jihlava.G1 =  reshape2::melt (df.waves.jihlava, id.vars = c ("wave"))
df.waves.brno.G1 = reshape2::melt (df.waves.brno, id.vars = c ("wave"))
df.waves.dobro.G1 = reshape2::melt (df.waves.dobro, id.vars = c ("wave"))

df.waves.jihlava.G1$wave = as.factor(df.waves.jihlava.G1$wave)
df.waves.brno.G1$wave = as.factor(df.waves.brno.G1$wave)
df.waves.dobro.G1$wave = as.factor(df.waves.dobro.G1$wave)



g.jihlava = ggplot (df.waves.jihlava.G1, aes(x=wave, y=value, group = variable,
                                             color = variable))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  ggtitle("Jihlava")

g.brno = ggplot (df.waves.brno.G1, aes(x=wave, y=value, group = variable, color = variable))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  ylim(0,10)+
  ggtitle("Brno")

df.waves.dobro.G1b = df.waves.dobro.G1

df.waves.dobro.G1b$value[3]=df.waves.dobro.G1b$value[3]+0.05
df.waves.dobro.G1b$value[4]=df.waves.dobro.G1b$value[4]+0.05

g.dobro = ggplot (df.waves.dobro.G1b, aes(x=wave, y=value, group = variable, 
                                          color = variable))+
  geom_line(size=1)+
  geom_point(size=3)+
  labs (y="", x="")+
  scale_x_discrete(labels=c("1. vlna", "2. vlna"))+
  scale_color_discrete (labels=c("Emoce", "Chování", "Hyperaktivita", "Vrstevníci", 
                                 "Pro-sociální"))+
  scale_y_continuous(limits = c(0,10))+
  ggtitle("Dobromerice")

prow <- plot_grid( g.jihlava + theme(legend.position="none"),
                   g.brno + theme(legend.position="none"),
                   g.dobro + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 1
)
legend_b <- get_legend(g.jihlava + theme(legend.position="bottom"))
plot_4_5_6 <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
plot_4_5_6

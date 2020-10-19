###
# Recoding variables and then scoring the teacher SDQ scores 
# (sometimes 0 is positive, sometimes 2 is positive...)


# recode reversed scales
df_master$uobeys <- car::recode (df_master$tobeys, "0=2; 1=1; 2=0; else=NA")
df_master$ureflect <- car::recode (df_master$treflect, "0=2; 1=1; 2=0; else=NA")
df_master$uattends <- car::recode (df_master$tattends, "0=2; 1=1; 2=0; else=NA")
df_master$ufriend <- car::recode (df_master$tfriend, "0=2; 1=1; 2=0; else=NA")
df_master$upopular <- car::recode (df_master$tpopular, "0=2; 1=1; 2=0; else=NA")


############# Teacher evaluation - constructing dimensions

### emotional symptoms - internalizing

# data frame of five items for emotional symptoms
df_temotion <- data.frame(df_master$tsomatic, 
                          df_master$tworries, 
                          df_master$tunhappy, 
                          df_master$tclingy, 
                          df_master$tafraid)
# count NAs within the five items, create a variable in the original data set
df_master$tnemotion <- apply(df_temotion, 1, function(x) sum(is.na(x)))
# count mean value for emotional symptoms for cases with fewer than 3 missing values
df_master$temotion <- ifelse(df_master$tnemotion<3, rowMeans(df_temotion, na.rm=TRUE), NA)
# transform to 0 to 10 scale
df_master$temotion10 <- as.numeric(df_master$temotion) * 5
# takes a single numeric argument x and returns a numeric vector containing the largest integers not greater 
# than the corresponding elements of x
# this step is needed to avoid wierd decimals
df_master$temotion10 <- floor(0.5 + df_master$temotion10) 


### conduct symptoms - externalizing

df_tconduct <- data.frame(df_master$ttantrum, 
                          df_master$uobeys, 
                          df_master$tfights, 
                          df_master$tlies, 
                          df_master$tsteals)
df_master$tnconduct <- apply(df_tconduct, 1, function(x) sum(is.na(x)))
df_master$tconduct <- ifelse(df_master$tnconduct<3, rowMeans(df_tconduct, na.rm=TRUE), NA)
df_master$tconduct10 <- as.numeric(df_master$tconduct) * 5
df_master$tconduct10 <- floor(0.5 + df_master$tconduct10)

### hyperactivity symptoms - externalizing

df_thyper <- data.frame(df_master$trestles, 
                        df_master$tfidgety, 
                        df_master$tdistrac, 
                        df_master$ureflect, 
                        df_master$uattends)
df_master$tnhyper <- apply(df_thyper, 1, function(x) sum(is.na(x)))
df_master$thyper <- ifelse(df_master$tnhyper<3, rowMeans(df_thyper, na.rm=TRUE), NA)
df_master$thyper10 <- as.numeric(df_master$thyper) * 5
df_master$thyper10 <- floor(0.5 + df_master$thyper10)

### relations to peers symptoms - internalizing

df_tpeer <- data.frame(df_master$tloner, 
                       df_master$ufriend, 
                       df_master$upopular, 
                       df_master$tbullied, 
                       df_master$toldbest)
df_master$tnpeer <- apply(df_tpeer, 1, function(x) sum(is.na(x)))
df_master$tpeer <- ifelse(df_master$tnpeer<3, rowMeans(df_tpeer, na.rm=TRUE), NA)
df_master$tpeer10 <- as.numeric(df_master$tpeer) * 5
df_master$tpeer10 <- floor(0.5 + df_master$tpeer10)

### pro-social symptoms

df_tprosoc <- data.frame(df_master$tconsid, 
                         df_master$tshares, 
                         df_master$tcaring, 
                         df_master$tkind, 
                         df_master$thelpout)
df_master$tnprosoc <- apply(df_tprosoc, 1, function(x) sum(is.na(x)))
df_master$tprosoc <- ifelse(df_master$tnprosoc<3, rowMeans(df_tprosoc, na.rm=TRUE), NA)
df_master$tprosoc10 <- as.numeric(df_master$tprosoc) * 5
df_master$tprosoc10 <- floor(0.5 + df_master$tprosoc10)



# sum of mean problem behaviour
df_master$tebdtot <- df_master$temotion+df_master$tconduct+df_master$thyper+df_master$tpeer 

# sum of mean problem behaviour on scale 0-40
df_master$tebdtot40 <- df_master$temotion10+df_master$tconduct10+df_master$thyper10+df_master$tpeer10

rm (df_temotion, df_tconduct, df_thyper, df_tpeer, df_tprosoc)# clean what you no longer need

# data types

df_master$tebdtot40 = as.integer(df_master$tebdtot40)
df_master$temotion10 = as.integer (df_master$temotion10)
df_master$tconduct10 = as.integer (df_master$tconduct10)
df_master$thyper10 = as.integer (df_master$thyper10)
df_master$tpeer10 =as.integer (df_master$tpeer10)
df_master$tprosoc10 = as.integer(df_master$tprosoc10)

# setwd("C:/Users/mazak/OneDrive/Dokumenty/rwd/sdq_2018")
# write_csv(df_master, "./output/data_SDQ_recoded.csv")
# 
# save.image(file="all_objects_at_data_SDQ_recoded.RData")
# load(file="all_objects_at_data_SDQ_recoded.RData")
# rm(list=ls())
# 
# df_master <- read_csv("./output/data_SDQ_recoded.csv")

rm(list=setdiff(ls(), "df_master")) # remove all but df_master - good to keep as vars str already set

# overall distribution
# See below for norms (benchmarks) - but no Czech benchmarks are available
# browseURL("http://www.sdqinfo.com/g0.html")

# german Data normalization: DOI: 10.1024//1422-4917.30.2.105




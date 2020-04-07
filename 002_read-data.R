# should hold code that reads the data and does any transformations immediately tied to data reading,
# e.g. setting data types or basic filtering. Again, this is code that you don’t expect to change
# as you work on the actual analysis. You may want to save the result into rds files
# in data-intermediate (or data-input if it is simply an rds mirror of the input data saved for quick access.)



# set data types
# basic filtering
# SAVE results in RDS to data-intermediate
#

library(here)
library(tidyverse)
library(magrittr)


# read everything as character, so nothing is lost due to "force parsing" of readr
dfm <- read_csv(here("data-input/SDQ_all_incl_w7.csv"),
                col_types = cols(.default = col_character()))

# quick check
dfm %>% glimpse

# change types - factors
dfm %<>% mutate(
  is_intervention2 = factor(
    is_intervention2,
    labels = c("control", "experimental")
  ),
  gender_girl = parse_factor(gender_girl, include_na = FALSE) # NA as is
)

# integers
dfm %<>%
  mutate_at(vars(born_month,
                 born_year,
                 fill_in_month:tattends,
                 uobeys:min_waves,
                 wave_ind,
                 wave_number,
                 -c(temotion, tconduct, thyper, tpeer, tprosoc)),
            parse_integer)

# numeric
dfm %<>% mutate_at(vars(temotion, tconduct, thyper, tpeer, tprosoc), parse_double)

# row ID for exact identification of each observation
dfm %<>% rowid_to_column()

# quick check
dfm %>% glimpse

# write to intermediate data folder in RDS
dfm %>% write_rds(here("data-intermediate/waves_1-7.rds"))

# remove dataset from the environment to ensure all
# subsequent analyses are based on saved RDS file
rm(dfm)

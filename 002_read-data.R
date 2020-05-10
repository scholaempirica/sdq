library(here)
library(tidyverse)
library(magrittr)

# read everything as character, so nothing is lost due to "force parsing" of readr
dfm <- read_csv(here("data-input/SDQ_all_incl_w7.csv"),
                col_types = cols(.default = col_character()))

# quick check
# dfm %>% glimpse

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
# dfm %>% glimpse

# write to intermediate data folder in RDS
dfm %>% write_rds(here("data-intermediate/waves_1-7.rds"))

# remove dataset from the environment to ensure all
# subsequent analyses are based on saved RDS file
rm(dfm)

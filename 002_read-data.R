library(tidyverse)
library(magrittr)
library(here)
library(lubridate)
library(readxl)


# 8th wave import ---------------------------------------------------------


# read separate XLSX files that contains "sablonaNaVyplnovani" in filename
files <- list.files(here("data-input"), pattern = "sablonaNaVyplnovani.+\\.xlsx$")

xlsx <- map(here("data-input", files), ~ read_xlsx(.x, skip = 2, col_types = c("id_school" = "text")))

# friendly file names
names(xlsx) <- files %>% str_extract("(?<=.[:space:]..).*(?=\\.xlsx)")

# bind all into bigger df
from_xlsx <- xlsx %>% bind_rows()


from_xlsx %<>% mutate(
  wave = as.numeric(wave),
  is_intervention2 = if_else(is_intervention == "1", "experimental", "control") %>% factor(levels = c("control", "experimental")),
  born_month = parse_number(born_month),
  born_year = parse_number(born_year),
  fill_in_month = parse_number(fill_in_month),
  fill_in_year = parse_number(fill_in_year),
  gender_girl = parse_integer(gender_girl),
  id_school = parse_number(id_school),
  across(starts_with("t"), parse_number)
)


wave_8_summer <- readxl::read_xlsx(here("data-input/SDQ_CKP_leto_2020.xlsx"))
wave_8_fall <- read_delim("data-input/SDQ_CKP_podzim2019.csv",
  ";",
  locale = locale(
    encoding = "windows-1250"
  )
)

# sanify names
names(wave_8_summer) %<>% map_chr(~ .x %>%
  if_else(str_detect(., "[\\[\\]]"), str_extract(., "(?<=\\[).*(?=\\])"), .) %>%
  if_else(str_detect(., "\\["), str_extract(., ".*(?=\\[)"), .) %>%
  str_trim())

# rename vars - from old scripts

item_labs <- c(
  id_school = "Číslo školky (první část trojmístného kódu)",
  id_teacher = "Číslo učitelky (druhá část trojmístného kódu)",
  id_or_name = "Číslo dítěte (třetí část trojmístného kódu)",
  born_month = "Měsíc narození dítěte (číslovkou)",
  born_year = "Rok narození dítěte",
  gender_girl = "Pohlaví dítěte",
  teacher_name = "Vaše příjmení (usnadňuje kontrolu dat)", # JN'S addition
  id_class = "Název třídy, do které dítě chodí (umožňuje analýzu po skupinách)",
  tconsid = "Snaží se chovat pěkně k druhým lidem. Bere ohled na jejich pocity",
  trestles = "Je neklidný/á. Nevydrží dlouho bez hnutí",
  tsomatic = "Často si stěžuje na bolesti hlavy, žaludku nebo na nevolnost",
  tshares = "Obvykle se dělí s druhými (o jídlo, hry, psací potřeby aj.)",
  ttantrum = "Často má záchvaty vzteku nebo výbušnou náladu",
  tloner = "Je spíše samotář/samotářka. Má sklon hrát si sám/sama",
  tobeys = "Je celkem poslušný/á. Obvykle dělá, co si dospělí přejí",
  tworries = "Má hodně starostí, často vypadá ustaraně",
  tcaring = "Vždy ochotný pomoct, když si někdo ublíží, je zarmoucený nebo mu je zle",
  tfidgety = "Je neposedný",
  tfriend = "Má alespoň jednoho dobrého kamaráda nebo kamarádku",
  tfights = "Často se pere s jinými dětmi nebo je šikanuje",
  tunhappy = "Je často nešťastný/á, skleslý/á nebo smutný/á",
  tpopular = "Je vcelku oblíbený/á mezi jinými dětmi",
  tdistrac = "Snadno se dá vyrušit. Špatně se soustředí",
  tclingy = "Je nervózní nebo nesamostatný v nových situacích. Snadno ztratí sebedůvěru",
  tkind = "Je laskavý/á k mladším dětem",
  tlies = "Často lže nebo podvádí",
  tbullied = "Jiné děti ho/ji šikanují",
  thelpout = "Často dobrovolně pomáhá druhým (rodičům, učitelům, jiným dětem)",
  treflect = "Přemýšlí, než něco udělá",
  tsteals = "Je zlomyslný/á",
  toldbest = "Lépe vychází s dospělými než jinými dětmi",
  tafraid = "Má mnoho strachů. Snadno se poleká",
  tattends = "Vytrvá u úkolu do konce"
)

wave_8_summer %<>% rename(all_of(item_labs))

# fall 2019 renames, leaving timestamp alone
names(wave_8_fall)[-1] <- names(item_labs)

# strip out messed up HMS part...
wave_8_fall %<>% mutate(`Časová značka` = `Časová značka` %>% str_remove("(?=[:space:]).*") %>% ymd)

# unite classes
wave_8_fall %<>% mutate(across(c(starts_with("id"), born_month), as.character))

# merge
wave_8 <- bind_rows(wave_8_summer, wave_8_fall)

# WARNING - hardcoded region from the raw spreadsheets
wave_8 %<>% add_column(region = "6", id_town = "6", wave = 8, is_intervention2 = 1, .before = 1)


# derive filled in mont and year from timestamp
wave_8 %<>% mutate(fill_in_month = lubridate::month(`Časová značka`), fill_in_year = lubridate::year(`Časová značka`)) %>%
  select(-`Časová značka`)

# gender_girl as integer, born_month as integer
wave_8 %<>%
  mutate(
    id_school = parse_number(id_school),
    gender_girl = if_else(gender_girl == "Dívka", 1, 0),
    is_intervention2 = if_else(is_intervention2 == 1, "experimental", "control") %>% factor(levels = c("control", "experimental")),
    born_month = born_month %>% parse_number()
  )



# integrify SDQ items
wave_8 %<>% mutate(across(tconsid:tattends, ~ case_when(
  .x == "Definitivně pravda" ~ 2,
  .x == "Tak trochu pravda" ~ 1,
  .x == "Není pravda" ~ 0
)))

wave_8 <- bind_rows(wave_8, from_xlsx)

# normalize class names
wave_8 %<>% mutate(id_class = id_class %>% stringi::stri_trans_general("Latin-ASCII") %>% str_to_lower())


# wave_8 %>% glimpse

# wave_8 = wave_8 %>% mutate(id_teacherM = ifelse(is.na(id_teacherM) | is.null(id_teacherM), id_teacher, id_teacherM))

wave_8$id_pupil = paste(
  sprintf("%02d",wave_8$id_school %>% as.numeric()),
  sprintf("%04d",wave_8$id_or_name %>% as.numeric()), sep = "-") #combines school and pupil id into an unique pupil id

wave_8$id_teach_unq = paste(
  sprintf("%02d",wave_8$id_school %>% as.numeric()),
  sprintf("%03d",wave_8$id_teacher %>% as.numeric()), sep = "-") #combines school and teacher id into an unique teacher id

#===Reversing scale items===
wave_8$uobeys <- car::recode (wave_8$tobeys, "0=2; 1=1; 2=0; else=NA")
wave_8$ureflect <- car::recode (wave_8$treflect, "0=2; 1=1; 2=0; else=NA")
wave_8$uattends <- car::recode (wave_8$tattends, "0=2; 1=1; 2=0; else=NA")
wave_8$ufriend <- car::recode (wave_8$tfriend, "0=2; 1=1; 2=0; else=NA")
wave_8$upopular <- car::recode (wave_8$tpopular, "0=2; 1=1; 2=0; else=NA")

#===Computing scales===
#emotions
df_temotion <- data.frame(wave_8$tsomatic,
                          wave_8$tworries,
                          wave_8$tunhappy,
                          wave_8$tclingy,
                          wave_8$tafraid) # data frame of five items for emotional symptoms
wave_8$tnemotion <- apply(df_temotion, 1, function(x) sum(is.na(x))) # count NAs within the five items, create a variable in the original data set
wave_8$temotion <- ifelse(wave_8$tnemotion<3, rowMeans(df_temotion, na.rm=TRUE), NA) #count mean value for emotional symptoms for cases with fewer than 3 missing values
wave_8$temotion10 <- as.numeric(wave_8$temotion) * 5 #transform to 0 to 10 scale
wave_8$temotion10 <- floor(0.5 + wave_8$temotion10) #Gets rid of decimal spaces


#conduct
df_tconduct <- data.frame(wave_8$ttantrum,
                          wave_8$uobeys,
                          wave_8$tfights,
                          wave_8$tlies,
                          wave_8$tsteals)
wave_8$tnconduct <- apply(df_tconduct, 1, function(x) sum(is.na(x)))
wave_8$tconduct <- ifelse(wave_8$tnconduct<3, rowMeans(df_tconduct, na.rm=TRUE), NA)
wave_8$tconduct10 <- as.numeric(wave_8$tconduct) * 5
wave_8$tconduct10 <- floor(0.5 + wave_8$tconduct10)

#hyperactivity symptoms - externalizing
df_thyper <- data.frame(wave_8$trestles,
                        wave_8$tfidgety,
                        wave_8$tdistrac,
                        wave_8$ureflect,
                        wave_8$uattends)
wave_8$tnhyper <- apply(df_thyper, 1, function(x) sum(is.na(x)))
wave_8$thyper <- ifelse(wave_8$tnhyper<3, rowMeans(df_thyper, na.rm=TRUE), NA)
wave_8$thyper10 <- as.numeric(wave_8$thyper) * 5
wave_8$thyper10 <- floor(0.5 + wave_8$thyper10)

#relations to peers symptoms - internalizing
df_tpeer <- data.frame(wave_8$tloner,
                       wave_8$ufriend,
                       wave_8$upopular,
                       wave_8$tbullied,
                       wave_8$toldbest)
wave_8$tnpeer <- apply(df_tpeer, 1, function(x) sum(is.na(x)))
wave_8$tpeer <- ifelse(wave_8$tnpeer<3, rowMeans(df_tpeer, na.rm=TRUE), NA)
wave_8$tpeer10 <- as.numeric(wave_8$tpeer) * 5
wave_8$tpeer10 <- floor(0.5 + wave_8$tpeer10)

#pro-social symptoms
df_tprosoc <- data.frame(wave_8$tconsid,
                         wave_8$tshares,
                         wave_8$tcaring,
                         wave_8$tkind,
                         wave_8$thelpout)
wave_8$tnprosoc <- apply(df_tprosoc, 1, function(x) sum(is.na(x)))
wave_8$tprosoc <- ifelse(wave_8$tnprosoc<3, rowMeans(df_tprosoc, na.rm=TRUE), NA)
wave_8$tprosoc10 <- as.numeric(wave_8$tprosoc) * 5
wave_8$tprosoc10 <- floor(0.5 + wave_8$tprosoc10)

remove(df_tconduct, df_temotion, df_thyper, df_tpeer, df_tprosoc) #gets rid of now unnecessary objects

#===Transforming variables into better types and creating aux. variables===
wave_8$id_school = as.character(wave_8$id_school) #changes id_school into factor
wave_8$gender_girl = factor(wave_8$gender_girl, labels = c("boys", "girls")) #changes gender_girl into factors and adds labels
#wave_8_omited = subset(wave_8, !is.na(wave_8$gender_girl)) #Creates dataframe without NAs in gender_girl (only needed for graphs with gender facets)




# -------------------------------------------------------------------------







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


# quick check
# dfm %>% glimpse

# write to intermediate data folder in RDS
# dfm %>% write_rds(here("data-intermediate/waves_1-7.rds"))

# remove dataset from the environment to ensure all
# subsequent analyses are based on saved RDS file
# rm(dfm)


master <- bind_rows(dfm, wave_8)

master %<>%
  group_by(id_pupil) %>%
  mutate(min_waves_w8 = min(wave)) %>%
  ungroup() %>%
  mutate(wave_ind_w8 = (wave - min_waves_w8) + 1) %>%
  group_by(id_pupil) %>%
  mutate(wave_number_w8 = max(wave_ind_w8)) %>%
ungroup


# check if equal
master %>%
  transmute(diff = wave_ind - wave_ind_w8, wave_ind, wave_ind_w8, wave) %>%
  filter(diff != 0 | is.na(diff))

master %>%
  transmute(diff = wave_number - wave_number_w8, wave_number, wave_number_w8, wave) %>%
  filter(diff != 0 | is.na(diff))


# merge wave indices
master %<>% mutate(wave_ind_w8 = wave_ind_w8 %>% as.integer(), wave_ind = if_else(wave_ind == wave_ind_w8 | is.na(wave_ind), wave_ind_w8, wave_ind))


# row ID for exact identification of each observation
master %<>% rowid_to_column()


master %>% write_rds(here("data-intermediate/waves_1-8.rds"))


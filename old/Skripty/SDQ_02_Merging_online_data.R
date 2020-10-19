library(tidyverse)

#----Wave 6----

SDQ_elektro = read_csv2("C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/SDQ_elektro.csv")

SDQ_elektro = SDQ_elektro %>% mutate_if(is.character, as.factor) #predela stringy na factory

#zmeni nazvy sloupcu a vymaze jmena ucitelek
SDQ_elektro = SDQ_elektro %>% select(id_school = "Číslo školky (první část trojmístného kódu)",
                                     id_teacher = "Číslo učitelky (druhá část trojmístného kódu)",
                                     id_or_name = "Číslo dítěte (třetí část trojmístného kódu)",
                                     born_month = "Měsíc narození dítěte (číslovkou)",
                                     born_year = "Rok narození dítěte",
                                     gender_girl = "Pohlaví dítěte",
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
                                     tattends = "Vytrvá u úkolu do konce")


SDQ_elektro$region = 6 #doplni promennou region podle skoly

SDQ_elektro = SDQ_elektro %>% select(region, everything()) #preradi promennou region na zacatek

SDQ_elektro$gender_girl = SDQ_elektro$gender_girl %>% fct_recode("1" = "Dívka", "0" = "Chlapec") #prekoduje gender
SDQ_elektro$gender_girl = as.integer(as.character(SDQ_elektro$gender_girl)) #predela gender na cislo


SDQ_elektro$id_class = SDQ_elektro$id_class %>% fct_recode("kotata" = "Koťata",
                                                           "kotata" = "KoŤata",
                                                           "mysky" = "Myšky") #prekoduje tridu


#prekoduje hodnoceni jednotlivych polozek hodnoceni
SDQ_elektro = SDQ_elektro %>% mutate_at(vars("tconsid":"tattends"), ~fct_recode(., "2" = "Definitivně pravda",
                                                                                "1" = "Tak trochu pravda",
                                                                                "0" = "Není pravda")) %>% 
  mutate_at(vars("tconsid":"tattends"), as.character) %>% mutate_at(vars("tconsid":"tattends"), as.integer)

SDQ_elektro$wave = 6 #prida cislo vlny
SDQ_elektro$id_teacherM = SDQ_elektro$id_teacher #prida promennou id_teacherM pro kompatibilitu s predeslym kodem

SDQ_elektro$is_intervention2 = 1

#----Wave 7----
SDQ_elektro2 = read_csv2("C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/orig/7vlna/online_data_7_vlna/SDQ pro Centrum kolegiální podpory podzim 2019 (Odpovědi).csv")

SDQ_elektro2 = SDQ_elektro2 %>% mutate_if(is.character, as.factor) #predela stringy na factory

#zmeni nazvy sloupcu a vymaze jmena ucitelek
SDQ_elektro2 = SDQ_elektro2 %>% select(id_school = "Číslo školky (první část trojmístného kódu)",
                                     id_teacher = "Číslo učitelky (druhá část trojmístného kódu)",
                                     id_or_name = "Číslo dítěte (třetí část trojmístného kódu)",
                                     born_month = "Měsíc narození dítěte (číslovkou)",
                                     born_year = "Rok narození dítěte",
                                     gender_girl = "Pohlaví dítěte",
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
                                     tattends = "Vytrvá u úkolu do konce")


SDQ_elektro2 = SDQ_elektro2 %>% mutate(region = case_when(id_school == 6 ~ 6,
                                                          id_school == 13 ~ 8,
                                                          id_school == 14 ~ 8))
SDQ_elektro2 = SDQ_elektro2 %>% mutate(is_intervention2 = case_when(id_school == 6 ~ 1,
                                                                    id_school == 13 ~ 1,
                                                                    id_school == 21 ~ 1,
                                                                    id_school == 14 ~ 0))

SDQ_elektro2 = SDQ_elektro2 %>% select(region, everything()) #preradi promennou region na zacatek

SDQ_elektro2$gender_girl = SDQ_elektro2$gender_girl %>% fct_recode("1" = "Dívka", "0" = "Chlapec") #prekoduje gender
SDQ_elektro2$gender_girl = as.integer(as.character(SDQ_elektro2$gender_girl)) #predela gender na cislo


SDQ_elektro2$id_class = SDQ_elektro2$id_class %>% fct_recode("kotata" = "Koťata",
                                                           "kotata" = "KoŤata",
                                                           "mysky" = "Myšky") #prekoduje tridu


#prekoduje hodnoceni jednotlivych polozek hodnoceni
SDQ_elektro2 = SDQ_elektro2 %>% mutate_at(vars("tconsid":"tattends"), ~fct_recode(., "2" = "Definitivně pravda",
                                                                                "1" = "Tak trochu pravda",
                                                                                "0" = "Není pravda")) %>% 
  mutate_at(vars("tconsid":"tattends"), as.character) %>% mutate_at(vars("tconsid":"tattends"), as.integer)

SDQ_elektro2$wave = 7 #prida cislo vlny
SDQ_elektro2$id_teacherM = SDQ_elektro2$id_teacher #prida promennou id_teacherM pro kompatibilitu s predeslym kodem

#---- Data merge----
df_elektro_fin = bind_rows(SDQ_elektro, SDQ_elektro2)


#----Saving dataset----
write_rds(x = df_elektro_fin, path = "C:/Users/ales_/Desktop/work/Schola Empirica/SDQ/Data/SDQ_online.rds")

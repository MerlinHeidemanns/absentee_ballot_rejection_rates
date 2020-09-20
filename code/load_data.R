library(tidyverse)
# load data
f_c <- read.csv("data/eavs_final_c_july23.csv") %>% 
    mutate(JurisdictionName = as.character(JurisdictionName) ) %>%
    filter(!State %in% c("VI", "PR", "GU")) %>%
    mutate(
    C1a = get_int(C1a),
    C1b = get_int(C1b),
    C4a = get_int(C4a), 
    C4b = get_int(C4b)
    )
df_f <- read.csv("data/eavs_final_f_july23.csv") %>% 
  mutate(JurisdictionName = as.character(JurisdictionName)) %>%
  filter(!State %in% c("VI", "PR", "GU")) %>%
  mutate(
    F1a = get_int(F1a),
    F1d = get_int(F1d),
    F1f = get_int(F1f)
  )
df_vermont <- read.csv("data/vermont_voterlist.csv")
df_vermont_collapsed <- read.csv("data/task1_vermont_voterlist_collapsed.csv")
df_c_for_census <- read.csv("data/eavs_final_c_july23.csv") %>%
  mutate(FIPSCode = as.integer(as.numeric(as.character(FIPSCode))/1e5))
df_census <- read.csv("data/census.csv") %>%
  mutate(FIPSCode = as.numeric(as.character(FIPS)))
df_tx <- read.csv("data/TXturnout.csv")
df_wi <- read.csv("data/wi.csv")
# adjust df_c
df_tmp <- df_c %>% 
  filter(State == "VT") %>%
  arrange(JurisdictionName)
df_vermont_collapsed <- df_vermont_collapsed %>%
  arrange(Town.Name)
for (i in colnames(df_vermont_collapsed)[3:20]){
  df_c[,i]   <- as.numeric(as.character(df_c[,i]))
  df_tmp[,i] <- df_vermont_collapsed[,i]
}
df_c <- rbind(df_c %>% filter(State != "VT"), df_tmp)
# fun
get_int <- function(x){
  x <- as.numeric(as.character(x))
  #x <- ifelse(is.na(x), 0, x)
  return(x)
}
get_int_NA0 <- function(x){
  x <- as.numeric(as.character(x))
  x <- ifelse(is.na(x), 0, x)
  return(x)
}
# adjust df_f
df_f <- df_f %>%
  mutate(F1a = ifelse(FIPSCode == 501300000, 2284, F1a),
         F1a = ifelse(FIPSCode == 504300000, 6641, F1a),
         F1a = ifelse(FIPSCode == 508500000, 27306, F1a),
         F1a = ifelse(FIPSCode == 510100000, 2802, F1a),
         F1a = ifelse(FIPSCode == 512300000, 7500, F1a),
         F1a = ifelse(FIPSCode == 514500000, 28286, F1a),
         F1a = ifelse(FIPSCode == 1500500000, 20, F1a),
         F1a = ifelse(FIPSCode == 2300302760, 20, F1a))
df_tmp <- df_f %>% filter(State == "TX") %>% 
  arrange(JurisdictionName)
df_tmp$F1a <- get_int(df_tx$F1a)
df_f <- rbind(df_f %>% filter(State != "TX") %>% mutate(F1a = get_int(F1a)), df_tmp)
# parameters
plot_width = 6
plot_height = 4

make_num <- function(x){
  return(as.numeric(as.character(x)))
}

df_acs <- read.csv("data/acs_13_18_2.csv") %>%
  left_join(read.csv("data/acs_econ_13_18.csv"), by = c("FIPSCode")) %>% 
  left_join(read.csv("data/acs_econ_13_18_full.csv"), by = c("FIPSCode")) %>% 
  left_join(read.csv("data/acs_econ_pov13-18.csv"), by = c("FIPSCode")) %>%
  left_join(read.csv("data/election_outcome.csv"), by = c("FIPSCode")) %>%
  transmute(FIPSCode = FIPSCode,
            jurisdiction = Geographic.Area.Name.x,
            total_2016 = make_num(total_2016),
            gop_2016 = make_num(gop_2016),
            dem_2016 = make_num(dem_2016),
            oth_2016 = make_num(oth_2016),
            WINNER = WINNER,
            unemp_pop_16plus_percentage = make_num(Percent.Estimate..EMPLOYMENT.STATUS..Civilian.labor.force..Unemployment.Rate),
            below_pov_past12_percentage = make_num(Percent.Estimate..PERCENTAGE.OF.FAMILIES.AND.PEOPLE.WHOSE.INCOME.IN.THE.PAST.12.MONTHS.IS.BELOW.THE.POVERTY.LEVEL..All.families),
            households         = make_num(households),
            median_household_income = make_num(median_household_income),
            total_pop           = make_num(Estimate..RACE..Total.population),
            pop_16              = 0.01 * make_num(Percent.Estimate..SEX.AND.AGE..Total.population..16.years.and.over * total_pop),
            white_count         = make_num(Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..White.alone),
            white_percentage    = make_num(Percent.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..White.alone),
            nonwhite_count      = make_num(total_pop - white_count),
            nonwhite_percentage = 100 - make_num(white_percentage), 
            hispanic_count      = make_num(Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Hispanic.or.Latino..of.any.race.),
            hispanic_percentage = make_num(Percent.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Hispanic.or.Latino..of.any.race.),
            black_count         = make_num(Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..Black.or.African.American.alone),
            black_percentage    = make_num(Percent.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..Black.or.African.American.alone), 
            native_count        = make_num(Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..American.Indian.and.Alaska.Native.alone),
            native_percentage   = make_num(Percent.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..American.Indian.and.Alaska.Native.alone),
            asian_count        = make_num(Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..Asian.alone),
            asian_percentage   = make_num(Percent.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..Asian.alone),
            pac_count          = make_num(Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..Native.Hawaiian.and.Other.Pacific.Islander.alone),
            pac_percentage     = make_num(Percent.Estimate..HISPANIC.OR.LATINO.AND.RACE..Total.population..Not.Hispanic.or.Latino..Native.Hawaiian.and.Other.Pacific.Islander.alone),
            voting_age    = make_num(Estimate..SEX.AND.AGE..Total.population..18.years.and.over)) %>%
  group_by(FIPSCode) %>%
  summarize(total_2016 = sum(total_2016, na.rm = TRUE),
            gop_2016 = sum(gop_2016, na.rm = TRUE),
            dem_2016 = sum(dem_2016, na.rm = TRUE),
            oth_2016 = sum(oth_2016, na.rm = TRUE),
            unemp_pop_16plus_percentage = mean(unemp_pop_16plus_percentage, na.rm = TRUE),
            below_pov_past12_percentage  = mean(below_pov_past12_percentage, na.rm = TRUE),
            households                   = floor(sum(households, na.rm = TRUE)),
            median_household_income   = mean(median_household_income, na.rm = TRUE),
            total_pop                 = floor(sum(total_pop, na.rm = TRUE)),
            pop_16                    = floor(sum(pop_16, na.rm = TRUE)),
            white_count               = floor(sum(white_percentage, na.rm = TRUE)),
            white_percentage          = mean(white_percentage, na.rm = TRUE),
            nonwhite_count            = floor(sum(nonwhite_count, na.rm = TRUE)),
            nonwhite_percentage       = mean(nonwhite_percentage, na.rm = TRUE),
            hispanic_count            = floor(sum(hispanic_count, na.rm = TRUE)),
            hispanic_percentage       = mean(hispanic_percentage, na.rm = TRUE),
            black_count               = floor(sum(black_count, na.rm = TRUE)),
            black_percentage          = mean(black_percentage, na.rm = TRUE),
            native_count              = floor(sum(native_count, na.rm = TRUE)),
            native_percentage         = mean(native_percentage, na.rm = TRUE),
            asian_count               = floor(sum(asian_count, na.rm = TRUE)),
            asian_percentage          = mean(asian_percentage, na.rm = TRUE),
            pac_count                 = floor(sum(pac_count, na.rm = TRUE)),
            pac_percentage            = mean(pac_percentage, na.rm = TRUE),
            voting_age                = floor(sum(voting_age, na.rm = TRUE))) %>%
  mutate(
    WINNER = ifelse(dem_2016 > gop_2016, "Dem", "Rep")
  )

eavs <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
  mutate(
    rejected =
      ifelse(!is.na(C4a) | !is.na(F1d) | !is.na(F1d) | is.na(C1a), C4b,
             ifelse(C4a > (F1d + F1d), C1b - C4a, C4b)),
    rejected =
      ifelse(!is.na(C4b) | !is.na(C1b) | !is.na(C4a) | is.na(C1b), C4b,
             ifelse(C4b < (C1b - C4a), C1b - C4a, C4b)),
    rejected = 
      ifelse(!is.na(C1b) & !is.na(C4a) & is.na(rejected),
             C1b - C4a, rejected),
    
  ) %>%
  rename(population = F1a, 
         submitted = C1b,
         transmitted = C1a,
         JurisdictionName = JurisdictionName.x) %>%
  mutate(
    counted = submitted - rejected,
    FIPSCode = as.integer(floor(as.numeric(as.character(FIPSCode))/1e5))
  ) %>%
  select(population, transmitted, submitted, counted, JurisdictionName, 
         rejected, State, FIPSCode, population) %>%
  filter(State != "PR", State != "VI", State != "GU") %>% as_tibble()

## Wisconsin
eavs_wi <- eavs  %>% 
  mutate(JurisdictionName = 
           ifelse(State == "WI",
                  gsub(pattern = ".*- ", "", x = JurisdictionName),
                  JurisdictionName)) %>%
  filter(State == "WI", JurisdictionName != "MULTIPLE COUNTIES") %>%
  #mutate(FIPSCode = as.character(paste0("0", FIPSCode))) %>%
  group_by(State, JurisdictionName, FIPSCode) %>%
  summarize(population = sum(population, na.rm = TRUE),
            transmitted = sum(transmitted, na.rm = TRUE),
            submitted = sum(submitted, na.rm = TRUE),
            rejected = sum(rejected, na.rm = TRUE),
            counted = sum(counted, na.rm = TRUE)) %>% 
  arrange(JurisdictionName)
eavs_wi$FIPSCode <- df_wi %>% arrange(County.Name) %>% pull(FIPS.Code)
eavs_wi <- eavs_wi %>% mutate(
  FIPSCode = as.integer(ifelse(
    FIPSCode < 10, paste0("5500", FIPSCode),
    ifelse(FIPSCode < 100, paste0("550", FIPSCode), paste0("55", FIPSCode))
  ))
) %>% 
  select(population, transmitted, submitted, counted, JurisdictionName, 
         rejected, State, FIPSCode, population)
eavs <- bind_rows(eavs %>% filter(State != "WI"), eavs_wi)
## MA, NH
eavs_ma_nh <- eavs %>% filter(State == "MA" | State == "NH") %>%
  group_by(State, FIPSCode) %>%
  summarize(population = sum(population, na.rm = TRUE),
            transmitted = sum(transmitted, na.rm = TRUE),
            submitted = sum(submitted, na.rm = TRUE),
            rejected = sum(rejected, na.rm = TRUE),
            counted = sum(counted, na.rm = TRUE)) %>%
  mutate(JurisdictionName = "Original data contained subcounty names.") %>%
  select(population, transmitted, submitted, counted, JurisdictionName, 
         rejected, State, FIPSCode, population)
eavs <- bind_rows(eavs %>% filter(State != "MA", State != "NH"), eavs_ma_nh)

## MA, NH
eavs_other <- eavs %>% filter(State == "CT" | 
                                State == "IL" |
                                State == "ME" |
                                State == "RI" |
                                State == "VT") %>%
  group_by(State, FIPSCode) %>%
  summarize(population = sum(population, na.rm = TRUE),
            transmitted = sum(transmitted, na.rm = TRUE),
            submitted = sum(submitted, na.rm = TRUE),
            rejected = sum(rejected, na.rm = TRUE),
            counted = sum(counted, na.rm = TRUE)) %>%
  mutate(JurisdictionName = "Original data contained subcounty names.") %>%
  select(population, transmitted, submitted, counted, JurisdictionName, 
         rejected, State, FIPSCode, population)
eavs <- bind_rows(eavs %>% filter(State != "CT", 
                                  State != "IL", 
                                  State != "ME",
                                  State != "RI",
                                  State != "VT",), eavs_other)

eavs <- eavs %>%
  rename(
    mail_ballots_submitted = submitted,
    mail_ballots_counted   = counted,
    total_votes_2016 = population
  ) %>%
  mutate(
    # Pr(ballot returned | absentee ballot)  
    pr2 = ifelse(transmitted > 0 & mail_ballots_submitted <= transmitted, mail_ballots_submitted / transmitted, NA),
    pr3 = ifelse(mail_ballots_submitted   > 0 & rejected <= mail_ballots_submitted, rejected  / mail_ballots_submitted, NA),
    rejected = ifelse(rejected < 0, NA, rejected),
    pr3 = ifelse(pr3 < 0, NA, pr3),
    #rejected = ifelse(State == "AL", NA, rejected),
    pr3      = ifelse(State == "AL", NA, pr3),
    rejected = ifelse(State == "AL", NA, rejected),
    pr2      = ifelse(State %in% c("HI", "CT"), NA, pr2)
  ) 

# submitted 
# YATES COUNTY
df_acs <- df_acs %>% mutate(
  FIPSCode = ifelse(FIPSCode == 36123, 36122, FIPSCode)
)




merged <- merge(eavs, df_acs, by = "FIPSCode")
cat(dim(merged), dim(eavs), dim(df_acs))

df_acs %>% group_by(FIPSCode) %>%
  summarize(count = n()) %>% 
  filter(count > 1) %>% pull(count) %>% sum()

cond1 = ifelse(min(merged$rejected, na.rm = TRUE) < 0, TRUE, FALSE)
cond2 = ifelse(max(merged$pr3, na.rm = TRUE) > 1 |
                 min(merged$pr3, na.rm = TRUE) < 0 |
                 max(merged$pr2, na.rm = TRUE) > 1 |
                 min(merged$pr2, na.rm = TRUE) < 0 ,
               TRUE, FALSE)  
merged %>% filter(
  rejected < 0 |
    pr2      > 1 |
    pr2      < 0 |
    pr3      > 1 |
    pr3      < 0
) %>%
  select(State, JurisdictionName, rejected, pr2, pr3)



write.csv(merged,"data/eavs_merged_w_acs15_18_2020_08_20.csv", na="N/A")
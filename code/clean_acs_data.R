# libraries
library(tidyverse)
# functions
source("code/functions.R")
# clean ACS data
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
            white_count               = floor(sum(white_count, na.rm = TRUE)),
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

# submitted 
df_acs <- df_acs %>% mutate(
  FIPSCode = ifelse(FIPSCode == 36123, 36122, FIPSCode), # YATES COUNTY
  FIPSCode = ifelse(FIPSCode == 46102, 46113, FIPSCode)  # Oglala Lakota County
)

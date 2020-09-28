library(tidyverse)
## eavs data
source("code/functions.R")
# load data
df_b <- read.csv("data/eavs_final_b_july23.csv") %>% 
  mutate(JurisdictionName = as.character(JurisdictionName) ) %>%
  filter(!State %in% c("VI", "PR", "GU"))
df_c <- read.csv("data/eavs_final_c_july23.csv") %>% 
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



## eavs data

# combine eavs data
eavs <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
  left_join(df_b, by = c("State", "FIPSCode")) %>%
  mutate(
    FIPSCode = as.integer(as.numeric(as.character(FIPSCode))/1e5)
  )

# Missing data
cat("Percentage missing rejected", mean(!is.na(eavs$C4b)), "\n")
cat("Percentage missing submitted", mean(!is.na(eavs$C1b)), "\n")
cat("Percentage missing transmitted", mean(!is.na(eavs$C1a)), "\n")
cat("Percentage missing rejected", mean(!is.na(eavs$rejected)), "\n")
mean_changes_C1b_C4a <- eavs %>% transmute(rejected = C4b, 
                   rejected = 
                     ifelse(is.na(C4b) | is.na(C4a) | is.na(C1b), rejected,
                            ifelse(C4b < (C1b - C4a), C1b - C4a, rejected)),
                   change = ifelse(!is.na(C1b) & !is.na(C4a) & is.na(rejected), 1, 0)) %>% 
  pull(change) %>% mean(.)


# CT ----------------------------------------------------------------------
eavs_ct <- eavs %>% 
  filter(State == "CT") %>%
  dplyr::select(F1d, C1a, C4a, C1b, C4b)

# HI ----------------------------------------------------------------------
eavs_hi <- eavs %>% 
  filter(State == "HI") %>% 
  mutate(transmitted = C1a - get_int_NA0(F1c),
         submitted = C1b - get_int_NA0(F1c),
         counted = C4a,
         rejected = C4b,
         population = F1a) %>%
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)

# TX ----------------------------------------------------------------------
eavs_tx <- eavs %>% 
  filter(State == "TX", C1b == C4a) %>% 
  mutate(C4b_alt = get_int_NA0(C5a) + 
           get_int_NA0(C5a) + 
           get_int_NA0(C5b) + 
           get_int_NA0(C5c) + 
           get_int_NA0(C5d) + 
           get_int_NA0(C5e) + 
           get_int_NA0(C5f) + 
           get_int_NA0(C5g) + 
           get_int_NA0(C5h) + 
           get_int_NA0(C5i) + 
           get_int_NA0(C5j) + 
           get_int_NA0(C5k) + 
           get_int_NA0(C5l) + 
           get_int_NA0(C5m) + 
           get_int_NA0(C5n) + 
           get_int_NA0(C5o) + 
           get_int_NA0(C5p) + 
           get_int_NA0(C5q) + 
           get_int_NA0(C5r) + 
           get_int_NA0(C5s) + 
           get_int_NA0(C5t) + 
           get_int_NA0(C5u) + 
           get_int_NA0(C5v),
         rejected = ifelse(is.na(C4b) & C4b_alt != 0, C4b_alt, C4b),
         rejected = ifelse(is.na(rejected), C1a - C4a, rejected),
         rejected = ifelse(C4a == C4b, C4b_alt, rejected),
         rejected = ifelse(is.na(C4b) & C4b_alt == 0, C1b - C4a, rejected),
         rejected = ifelse(is.na(rejected) & C4b_alt != 0, C4b_alt, rejected),
         submitted = ifelse(rejected > C1b - C4a, C1b + rejected, C1b),
         transmitted = ifelse(rejected > C1b - C4a, C1a + rejected, C1a),
         counted = submitted - rejected,
         population = F1a
  ) %>%
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)

# NM ----------------------------------------------------------------------
eavs_nm <- eavs %>% 
  filter(State == "NM") %>% 
  mutate(C4b_alt = get_int_NA0(C5a) + 
           get_int_NA0(C5a) + 
           get_int_NA0(C5b) + 
           get_int_NA0(C5c) + 
           get_int_NA0(C5d) + 
           get_int_NA0(C5e) + 
           get_int_NA0(C5f) + 
           get_int_NA0(C5g) + 
           get_int_NA0(C5h) + 
           get_int_NA0(C5i) + 
           get_int_NA0(C5j) + 
           get_int_NA0(C5k) + 
           get_int_NA0(C5l) + 
           get_int_NA0(C5m) + 
           get_int_NA0(C5n) + 
           get_int_NA0(C5o) + 
           get_int_NA0(C5p) + 
           get_int_NA0(C5q) + 
           get_int_NA0(C5r) + 
           get_int_NA0(C5s) + 
           get_int_NA0(C5t) + 
           get_int_NA0(C5u) + 
           get_int_NA0(C5v),
         transmitted = C1a,
         rejected = ifelse(is.na(C4b) & C4b_alt == 0, NA, 
                    ifelse(is.na(C4b), C4b_alt, C4b )),
         rejected = ifelse(!is.na(C4b) & C4b_alt != 0,
                    ifelse(C4b > C4b_alt, C4b, 
                    ifelse(C4b < C4b_alt, C4b_alt, rejected)), rejected) ,
         rejected = ifelse(is.na(rejected) & C1b > C4a, C1b - C4a, rejected),
         counted = ifelse(is.na(C4a), C1b - rejected, 
                   ifelse(C4a > C1b, C1b - rejected, 
                   ifelse(C4a == 0, C1b - rejected, C4a)
                          )),
         submitted = ifelse(is.na(C1b), counted + rejected, C1b),
         population = F1a
                           ) %>%
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)

# AZ ----------------------------------------------------------------------
eavs_az <- eavs %>% 
  filter(State == "AZ") %>%
  mutate(transmitted = C1a - get_int_NA0(C1c),
         submitted = C1b,
         counted = C4a,
         rejected = C4b,
         population = F1a) %>%
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)

# AL ----------------------------------------------------------------------
  # B8a is N of counted UOVCA ballots which is included in C4a here
eavs_al <- eavs %>%
  filter(State == "AL") %>% 
  mutate(transmitted = C1a,
         submitted = C1b,
         counted = C4a - get_int_NA0(B8a),
         rejected = submitted - counted,
         transmitted_m_submitted = transmitted - submitted,
         population = F1a) %>% 
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x) %>%
  filter(rejected >= 0)
  
# RI ----------------------------------------------------------------------
eavs_ri <- eavs %>%
  filter(State == "RI") %>% 
  mutate(transmitted = C1a - get_int_NA0(C1c),
           submitted = C1b,
           counted = C4a,
           rejected = C4b,
         population = F1a) %>% 
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)
  

# WV ----------------------------------------------------------------------
eavs_wv <- eavs %>%
    filter(State == "WV") %>% 
  mutate(transmitted = C1a - get_int_NA0(C1c),
         submitted = C1b,
         counted = C1b - C4b,
         rejected = C4b,
         population = F1a) %>% 
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)

# IL ----------------------------------------------------------------------
eavs_il <- eavs %>%
  filter(State == "IL") %>% 
  mutate(transmitted = C1a - get_int_NA0(C1c),
         submitted = C1b,
         counted = ifelse(is.na(C4a), 0, C4a),
         rejected = ifelse(is.na(C4b), 0, C4b),
         population = F1a) %>% 
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)
  
# ME ----------------------------------------------------------------------
eavs_me <- eavs %>%
  filter(State == "ME", JurisdictionName.x != "MAINE - UOCAVA", JurisdictionName.x != "BANCROFT") %>% 
  mutate(transmitted = C1a - get_int_NA0(C1c),
         submitted = C1b,
         counted = C4a,
         rejected = C4b,
         population = F1a) %>% 
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)

# AR ----------------------------------------------------------------------
eavs_ar <- eavs %>% 
  filter(State == "AR") %>%
  mutate(transmitted = C1a - get_int_NA0(C1c),
         submitted = ifelse(is.na(C1b), C4a + C4b, C1b),
         counted = C4a,
         rejected = ifelse(is.na(C4b), C1b - counted, C4b),
         rejected_alt = get_int_NA0(C5a) + 
           get_int_NA0(C5a) + 
           get_int_NA0(C5b) + 
           get_int_NA0(C5c) + 
           get_int_NA0(C5d) + 
           get_int_NA0(C5e) + 
           get_int_NA0(C5f) + 
           get_int_NA0(C5g) + 
           get_int_NA0(C5h) + 
           get_int_NA0(C5i) + 
           get_int_NA0(C5j) + 
           get_int_NA0(C5k) + 
           get_int_NA0(C5l) + 
           get_int_NA0(C5m) + 
           get_int_NA0(C5n) + 
           get_int_NA0(C5o) + 
           get_int_NA0(C5p) + 
           get_int_NA0(C5q) + 
           get_int_NA0(C5r) + 
           get_int_NA0(C5s) + 
           get_int_NA0(C5t) + 
           get_int_NA0(C5u) + 
           get_int_NA0(C5v),
         rejected = ifelse(is.na(rejected) & rejected_alt != 0, rejected_alt, rejected),
         submitted = ifelse(is.na(submitted), rejected + counted, submitted),
         population = F1a
         ) %>% 
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)
# CA ----------------------------------------------------------------------
eavs_ca <- eavs %>%
  filter(State == "CA") %>% 
  mutate(transmitted = C1a - get_int_NA0(C1c),
         submitted = ifelse(is.na(C1b), C4a + C4b, C1b),
         counted = C4a,
         rejected = ifelse(is.na(C4b), C1b - counted, C4b),
         submitted = ifelse(is.na(submitted), rejected + counted, submitted),
         population = F1a
  ) %>% 
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected)  %>%
  rename(JurisdictionName = JurisdictionName.x)
# SD ----------------------------------------------------------------------
eavs_sd <- eavs %>%
  filter(State == "SD") %>%
  mutate(transmitted = C1a,
         submitted = ifelse(is.na(C1b), C4a + C4b, C1b - get_int_NA0(C1c)),
         counted = C4a,
         rejected = ifelse(is.na(C4b), C1b - counted, C4b),
         submitted = ifelse(is.na(submitted), rejected + counted, submitted),
         population = F1a
  ) %>% 
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)
# eavs out ----------------------------------------------------------------
eavs <- eavs %>% 
  mutate(
    # rejected_alt
    C4b_alt = get_int_NA0(C5a) + get_int_NA0(C5a) + get_int_NA0(C5b) + get_int_NA0(C5c) + get_int_NA0(C5d) + get_int_NA0(C5e) + 
      get_int_NA0(C5f) + get_int_NA0(C5g) + get_int_NA0(C5h) + get_int_NA0(C5i) + get_int_NA0(C5j) + get_int_NA0(C5k) + get_int_NA0(C5l) + 
      get_int_NA0(C5m) + get_int_NA0(C5n) + get_int_NA0(C5o) + get_int_NA0(C5p) + get_int_NA0(C5q) + get_int_NA0(C5r) + get_int_NA0(C5s) + 
      get_int_NA0(C5t) + get_int_NA0(C5u) + get_int_NA0(C5v),
    # transmitted
    transmitted = C1a - get_int_NA0(C1c),
    
    counted = ifelse(is.na(C4a) & is.na(F1d), NA,
                   ifelse(is.na(C4a), F1d,
                   ifelse(is.na(F1d), C4a,
                   ifelse(C4a > F1d, C4a, F1d)))),
    submitted = C1b,
    rejected = C4b, 
    rejected = ifelse(is.na(rejected), C4b_alt, rejected),
    rejected = ifelse(is.na(rejected), submitted - counted, rejected),
    population = F1a,
    
  ) %>%
  dplyr::select(State, FIPSCode, JurisdictionName.x, population, transmitted, submitted, counted, rejected) %>%
  rename(JurisdictionName = JurisdictionName.x)

# add fixed states --------------------------------------------------------
eavs <- rbind(eavs %>% filter(State != "HI"), eavs_hi)
eavs <- rbind(eavs %>% filter(State != "TX"), eavs_tx)
eavs <- rbind(eavs %>% filter(State != "NM"), eavs_nm)
eavs <- rbind(eavs %>% filter(State != "AZ"), eavs_az)
eavs <- rbind(eavs %>% filter(State != "AL"), eavs_al)
eavs <- rbind(eavs %>% filter(State != "RI"), eavs_ri)
eavs <- rbind(eavs %>% filter(State != "WV"), eavs_wv)
eavs <- rbind(eavs %>% filter(State != "IL"), eavs_il)
eavs <- rbind(eavs %>% filter(State != "ME"), eavs_me)
eavs <- rbind(eavs %>% filter(State != "AR"), eavs_ar)
eavs <- rbind(eavs %>% filter(State != "CA"), eavs_ca)
eavs <- rbind(eavs %>% filter(State != "SD"), eavs_sd)



## Wisconsin
eavs_wi <- eavs  %>% 
  mutate(JurisdictionName = 
           ifelse(State == "WI",
                  gsub(pattern = ".*- ", "", x = JurisdictionName),
                  JurisdictionName)) %>%
  filter(State == "WI", JurisdictionName != "MULTIPLE COUNTIES") %>%
  group_by(State, JurisdictionName) %>%
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
  dplyr::select(State, FIPSCode, JurisdictionName, population, transmitted, submitted, counted, rejected)
  
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
  dplyr::select(State, FIPSCode, JurisdictionName, population, transmitted, submitted, counted, rejected)
eavs <- bind_rows(eavs %>% filter(State != "MA", State != "NH"), eavs_ma_nh)

## CT, IL, ME, RI, VT
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
  dplyr::select(State, FIPSCode, JurisdictionName, population, transmitted, submitted, counted, rejected)
eavs <- bind_rows(eavs %>% filter(State != "CT", 
                                  State != "IL", 
                                  State != "ME",
                                  State != "RI",
                                  State != "VT",), eavs_other)

# create rates ------------------------------------------------------------
eavs <- eavs %>%
  mutate(
    pr1 = transmitted/population,
    pr2 = submitted/transmitted,
    pr3 = rejected/submitted,
    pr1 = ifelse(State %in% c("OR", "WA", "CO", "UT", "HI"), 1, pr1),
    pr2 = ifelse(State %in% c("OR"), 1, pr2)
    )
# data quality checks
print("Obs by state")
eavs %>% 
  group_by(State) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  slice(1:51)


print("States with pr1 > 1")
eavs %>% 
  filter(pr1 > 1) %>%
  group_by(State) %>%
  summarize(count = n()) %>%
  print(.)
print("States with pr2 > 1")
eavs %>% 
  filter(pr2 > 1) %>%
  group_by(State) %>%
  summarize(count = n()) %>%
  print(.)
req_sub <- ggplot(data = eavs %>% filter(pr1 < 1, pr2 < 1, pr3 < 1), aes(x = pr1, y = pr2)) + 
  geom_point(size = 0.2)
sub_rej <- ggplot(data = eavs %>% filter(pr1 < 1, pr2 < 1, pr3 < 1), aes(x = pr2, y = pr3)) + 
  geom_point(size = 0.2)
req_rej <- ggplot(data = eavs %>% filter(pr1 < 1, pr2 < 1, pr3 < 1), aes(x = pr1, y = pr3)) + 
  geom_point(size = 0.2)
grid.arrange(req_sub, sub_rej, req_rej)
ggsave("plots/raw_viz/raw_rates.jpeg")







## eavs data
source("code/functions.R")
# load data
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

# C4b = rejected
# C4a = counted
# C1b = Returned by voters and submitted for counting
# F1d = Voted using a domestic civilian absentee ballot 
# F1f = Voted at an early vote center
# merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
#   filter(C4a >= (F1d + F1f), F1f != 0) %>% 
#   mutate(C1a_new = C1a - F1f,
#          C1b_new = C1b - F1f,
#          C4a_new = C4a - F1f,
#          C4b_new = C1b_new - C4a_new) %>%
#   filter(C4b != C4b_new) %>%
#   select(C1a, C1a_new, C1b, C1b_new, C4a, C4a_new, C4b, C4b_new, F1d, F1f, State) %>% 
#   View()

# combine eavs data
eavs <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE)
cat("Percentage missing rejected", mean(!is.na(eavs$C4b)), "\n")
cat("Percentage missing submitted", mean(!is.na(eavs$C1b)), "\n")
cat("Percentage missing transmitted", mean(!is.na(eavs$C1a)), "\n")
cat("Percentage missing rejected", mean(!is.na(eavs$rejected)), "\n")
print("first fix if submitted - counted is bigger than rejected")
eavs %>% transmute(rejected = C4b, 
                   rejected = 
                     ifelse(is.na(C4b) | is.na(C4a) | is.na(C1b), rejected,
                            ifelse(C4b < (C1b - C4a), C1b - C4a, rejected)),
                   change = ifelse(!is.na(C1b) & !is.na(C4a) & is.na(rejected), 1, 0)) %>% 
  pull(change) %>% mean(.) %>% print(.)


eavs <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
  mutate(
    rejected = C4b, 
    rejected =
      ifelse(is.na(C4b) | is.na(C4a) | is.na(C1b), rejected,
             ifelse(C4b < (C1b - C4a), C1b - C4a, rejected)),
    rejected = ifelse(is.na(rejected), C4b, rejected),
    rejected =
      ifelse(!is.na(C1b) & !is.na(C4b) & is.na(rejected),
             C1b - C4a, rejected)
  ) %>%
  rename(population = F1a, 
         submitted = C1b,
         transmitted = C1a,
         JurisdictionName = JurisdictionName.x) %>%
  mutate(
    counted = submitted - rejected,
    FIPSCode = as.integer(floor(as.numeric(as.character(FIPSCode))/1e5))
  ) %>% 
  dplyr::select(population, transmitted, submitted, counted, JurisdictionName, 
                rejected, State, FIPSCode, population) %>%
  filter(State != "PR", State != "VI", State != "GU") %>% as_tibble()

## Wisconsin
eavs_wi <- eavs  %>% 
  mutate(JurisdictionName = 
           ifelse(State == "WI",
                  gsub(pattern = ".*- ", "", x = JurisdictionName),
                  JurisdictionName)) %>%
  filter(State == "WI", JurisdictionName != "MULTIPLE COUNTIES") %>%
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
  dplyr::select(population, transmitted, submitted, counted, JurisdictionName, 
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
  dplyr::select(population, transmitted, submitted, counted, JurisdictionName, 
                rejected, State, FIPSCode, population)
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
  dplyr::select(population, transmitted, submitted, counted, JurisdictionName, 
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
    pr2      = ifelse(State %in% c("HI", "CT"), NA, pr2),
    pr1      = transmitted/total_votes_2016,
    pr1      = ifelse(State %in% c("OR", "WA", "CO", "UT", "HI"), 1, pr1),
    pr1 = ifelse(State %in% c("OR", "WA", "CO", "UT", "HI"), 1, pr1),
    pr1 = ifelse(pr1 >1 | pr1 < 0, NA, pr1),
    pr2 = ifelse(pr1 >1 | pr1 < 0, NA, pr2),
    pr3 = ifelse(pr1 >1 | pr1 < 0, NA, pr3)
  ) 




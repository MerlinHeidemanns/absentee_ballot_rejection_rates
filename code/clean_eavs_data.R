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
# F1d = Voted using a domestic civilian absentee ballot / counted

# C1b = Returned by voters and submitted for counting
# F1f = Voted at an early vote center

# F1d should be equal or 

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
mean_changes_C1b_C4a <- eavs %>% transmute(rejected = C4b, 
                   rejected = 
                     ifelse(is.na(C4b) | is.na(C4a) | is.na(C1b), rejected,
                            ifelse(C4b < (C1b - C4a), C1b - C4a, rejected)),
                   change = ifelse(!is.na(C1b) & !is.na(C4a) & is.na(rejected), 1, 0)) %>% 
  pull(change) %>% mean(.)
cat("first fix if submitted - counted is bigger than rejected", mean_changes_C1b_C4a)

merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
  ggplot(data = ., aes(x = C4a - F1d)) +
    geom_histogram(bins = 40) +
    facet_wrap(.~State, scales = "free") + 
    theme_bw()

merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
  dplyr::select(State, F1c, F1g, C1b, C4a, C4b) %>%
  filter(F1g != "-999999: Data Not Available",
         F1g != "-888888: Not Applicable",
         F1g != "0") %>%
  mutate(F1g = as.numeric(as.character(F1g))) %>%
    View() 

merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
  mutate(C1a_alt = get_int_NA0(C1b) +
           get_int_NA0(C1c) +
           get_int_NA0(C1d) +
           get_int_NA0(C1e) +
           get_int_NA0(C1f) +
           get_int_NA0(C1g) +
           get_int_NA0(C1h)
  ) %>%
  dplyr::select(State, C4a, C4b,C1a_alt,C1a, C1b, C1c, C1d, C1e, C1f, C1g, C1h) %>%
  View()
  
C1a
merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
  dplyr::select(State, C1b, C4a, C4b, C4Comments) %>%
  filter(C4Comments != "-999999: Data Not Available",
         C4Comments != "-999999: Data Not Available",
         C4Comments != "-888888: Not Applicable") %>%
  View()
  
eavs_f1df1f <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
  mutate(C1b_alt = F1d + F1f) %>%
  dplyr::select(State, C1b_alt, C1b, F1d, F1f, C4b) %>%
  mutate(counted_m_rejected = C1b - C4b,
         equal    = ifelse(submitted_m_rejected == F1d,1,0)) %>%
  filter(State == "TN") %>%
  View()
merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
  mutate(C1b_alt = F1d + F1f) %>%
  dplyr::select(State, C1b_alt, C1b, F1d, F1f, C4b) %>%
  mutate(counted_m_rejected = C1b - C4b,
        equal_na = ifelse(is.na(counted_m_rejected) | is.na(F1d), 1, 0),
        equal    = ifelse(counted_m_rejected == F1d,1,0)) %>%
  group_by(State) %>%
  summarize(mean_equal = mean(equal, na.rm = TRUE),
            mean_equal_na = mean(equal_na)) %>%
  View()



C1b = F1d + F1f
eavs_viz <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
  mutate(
    rejected = C4b, 
    rejected =
      ifelse(is.na(C4b) | is.na(C4a) | is.na(C1b), rejected,
             ifelse(C4b < (C1b - C4a), C1b - C4a, rejected)),
    rejected =
      ifelse(!is.na(C1b) & !is.na(C4a) & is.na(rejected),
             C1b - C4a, rejected)
  ) %>%
  rename(population = F1a, 
         submitted = C1b,
         transmitted = C1a,
         JurisdictionName = JurisdictionName.x) %>%
  mutate(
    pr1 = transmitted/population,
    pr2 = submitted/transmitted,
    pr3 = rejected/submitted,
    pr3_alt = C4b/submitted
  )
ggplot(data = eavs_viz %>%
         filter(pr3 != pr3_alt)) +
  geom_point(aes(x = pr3, y = pr3_alt, size = rejected), alpha = 0.3) +
  geom_abline() +
  facet_wrap(.~State) +
  theme_bw()

ggplot(data = eavs_viz %>% 
         dplyr::select(State, pr1, pr2, pr3) %>%
         group_by(State) %>%
         summarize_all(list(~ mean(., na.rm = TRUE), 
                            ~ sd(., na.rm = TRUE),
                            ~ max(., na.rm = TRUE),
                            ~ min(., na.rm = TRUE))) %>%
         pivot_longer(cols = c(-State),names_to = c("kind", ".value"),
                      names_pattern = "(.+)_(.+)")) + 
  geom_point(aes(x = State, y = mean), size = 0.8) +
  geom_point(aes(x = State, y = max), size = 0.8, shape = 4) +
  geom_point(aes(x = State, y = min), size = 0.8, shape = 4) +
  geom_errorbar(aes(x = State, ymax = mean + 2/3 * sd, ymin = mean - 2/3 * sd), width = 0) +
  coord_flip() + 
  ylim(c(0,1)) + 
  labs(x = "Share") + 
  facet_grid(~kind, scales = "free") + 
  theme_bw()

t <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>%
  mutate(counted = ifelse(is.na(C4a) & is.na(F1d), NA,
                   ifelse(is.na(C4a), F1d,
                   ifelse(is.na(F1d), C4a,
                   ifelse(C4a > F1d, C4a, F1d)))))


# CT ----------------------------------------------------------------------
merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
  filter(State == "CT") %>%
  dplyr::select(F1d, C1a, C4a, C1b, C4b) %>%
  View()

# HI ----------------------------------------------------------------------
merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
  filter(State == "HI") %>% View()
  dplyr::select(C1a, C1b, C4a, C4b, F1a, F1b, F1c, F1d) %>%
  mutate(submitted_alt = get_int_NA0(F1c) + C4a + C4b) %>%
  View()

# TX ----------------------------------------------------------------------
merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
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
         counted = submitted - rejected
  ) %>%
  dplyr::select(C1a, C1b, C4a, C4b, C4b_alt, rejected,counted, submitted, transmitted, F1a, F1b, F1c, F1d, C4c, C4d) %>%
  View()

# NM ----------------------------------------------------------------------
merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
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
         transmitted_submitted = C1a - C1b,
         rejected = ifelse(is.na(C4b) & C4b_alt == 0, NA, 
                    ifelse(is.na(C4b), C4b_alt, C4b )),
         rejected = ifelse(!is.na(C4b) & C4b_alt != 0,
                    ifelse(C4b > C4b_alt, C4b, 
                    ifelse(C4b < C4b_alt, C4b_alt, rejected)), rejected) ,
         rejected = ifelse(is.na(rejected) & C1b > C4a, C1b - C4a, rejected),
         counted = ifelse(is.na(C4a), C1b - rejected, 
                   ifelse(C4a > C1b, C1b - rejected, 
                   ifelse(C4a == 0, C1b - rejected, C4a)
                          ))
                           ) %>%
  dplyr::select(C1a, C1b, counted, C4a, rejected, C4b, C4b_alt, F1a, F1b, F1c, F1d, F1e, F1f, F1g, F2, F2_Other) %>%
  View()

# AZ ----------------------------------------------------------------------
eavs <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
  filter(State == "AZ") %>%
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
           get_int_NA0(C5v)) %>%
  dplyr::select(C1a, C1b, C4a, C4b, C4b_alt, F1a, F1b, F1c, F1d, F1e, F1f, F1g) %>%
  View()
    
  
  
  
  
# eavs out ----------------------------------------------------------------
eavs <- merge(df_c, df_f, by = c("State", "FIPSCode"), all.x = TRUE) %>% 
  mutate(
    C4b_alt = get_int_NA0(C5a) + get_int_NA0(C5a) + get_int_NA0(C5b) + get_int_NA0(C5c) + get_int_NA0(C5d) + get_int_NA0(C5e) + 
      get_int_NA0(C5f) + get_int_NA0(C5g) + get_int_NA0(C5h) + get_int_NA0(C5i) + get_int_NA0(C5j) + get_int_NA0(C5k) + get_int_NA0(C5l) + 
      get_int_NA0(C5m) + get_int_NA0(C5n) + get_int_NA0(C5o) + get_int_NA0(C5p) + get_int_NA0(C5q) + get_int_NA0(C5r) + get_int_NA0(C5s) + 
      get_int_NA0(C5t) + get_int_NA0(C5u) + get_int_NA0(C5v),
    
    
    
    
    
    
    transmitted = C1a - get_intNA(C1c),
    
    counted = ifelse(is.na(C4a) & is.na(F1d), NA,
                   ifelse(is.na(C4a), F1d,
                   ifelse(is.na(F1d), C4a,
                   ifelse(C4a > F1d, C4a, F1d)))),
         
    rejected = C4b, 
    rejected =
      ifelse(is.na(C4b) | is.na(C4a) | is.na(C1b), rejected,
             ifelse(C4b < (C1b - C4a), C1b - C4a, rejected)),
    rejected = ifelse(is.na(rejected), C4b, rejected),
    rejected =
      ifelse(!is.na(C1b) & !is.na(C4a) & is.na(rejected),
             C1b - C4a, rejected),
    # HI
    transmitted = ifelse(State == "HI", C1a - get_int_NA0(F1c), transmitted),
    submitted = ifelse(State == "HI", C1b - get_int_NA0(F1c), submitted),
    rejected = ifelse(State == "HI", C4b, rejected),
    # TX
    rejected = ifelse(!is.na(C4b), 
               ifelse((C4b + C4a) == C1a, C4b, C4b_alt)
                      , ifelse(C4b_alt != 0, C4b_alt, C4b))
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







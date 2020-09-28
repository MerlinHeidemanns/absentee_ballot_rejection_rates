simulation_function <- function(N_draws, N_iter, percentage_absentee, absentee_data, beta_draws){
  df_subset_collapsed <- df_subset %>% 
    group_by(State, group_id) %>% 
    summarize(voters_white = sum(voters_white, na.rm = TRUE),
              voters_black = sum(voters_black, na.rm = TRUE),
              voters_hispanic = sum(voters_hispanic, na.rm = TRUE),
              voters_asian = sum(voters_asian, na.rm = TRUE),
              voters_other = sum(voters_other, na.rm = TRUE),
              voters_count = sum(population, na.rm = TRUE)
              )
  states <- df_subset %>% distinct(State) %>% pull(State)
  n_states <- length(states)
  # voters
  # FIPSCode, State, n_absentee_voters, white, black, latino, asian, other,
  #                  n_submitted_voters, white, black, latino, asian, other
  #                  n_rejected_voters, white, black, latino,  asian, other
  df_subset_collapsed <- df_subset_collapsed %>% arrange(group_id)
  df_sim <- matrix(NA, nrow = 0, ncol = 25)
  count = 0
  for (id in sample(1:N_iter, N_draws, replace = FALSE)){
    count = count + 1
    print(count/N_draws)
    for (i in 1:nrow(df_subset_collapsed)) {
      n_absentee_voters = 0
      n_absentee_group = rep(0, 5)
      n_voters = df_subset_collapsed[i, "voters_count"]
      i_region = as.integer(df_subset_collapsed[i, "group_id"])
      if (region_crosswalk[i_region, "State"] %in% c("WA", "CA", "OR", "UT", "CO", "VT", "NJ", "HI" )){
        n_absentee_group = as.integer(df_subset_collapsed[i, c("voters_white", "voters_black", "voters_hispanic", "voters_asian", "voters_other")])
        n_absentee_voters = sum(n_absentee_group)
      } else {
        if (region_crosswalk[i_region, "State"] %in% absentee_data$State) {
          n_limit = absentee_data %>% 
            filter(State == region_crosswalk[i_region, "State"] %>% 
                                               pull(State) %>% 
                                               as.character(.)) %>% 
            pull(absentee_ballots_sent_out)
        } else {
          n_limit = floor(n_voters * percentage_absentee)
        }
        while (n_absentee_voters < n_limit) {
          n_group <- df_subset_collapsed[i, c("voters_white", "voters_black", "voters_hispanic", "voters_asian", "voters_other")] - n_absentee_group
          val = sapply(1:5, function(x) rbinom(1, as.integer(n_group[x]), prob = as.numeric(beta_draws[id, 1, i_region, x])))
          n_absentee_group = n_absentee_group + val
          n_absentee_voters = sum(n_absentee_group)
        }
      }
      if (region_crosswalk[i_region, "State"] %in% c("OR")){
        n_submitting_group = as.integer(df_subset_collapsed[i, c("voters_white", "voters_black", "voters_hispanic", "voters_asian", "voters_other")])
      } else {
        n_submitting_voters = 0
        n_submitting_group = rep(0, 5)
        n_submitting_group = sapply(1:5, function(x) rbinom(1, 
                                                            n_absentee_group[x], 
                                                            prob = as.numeric(beta_draws[id, 2, i_region, x])))
      }
      n_submitting_voters = sum(n_submitting_group)
      n_rejected_voters = 0
      n_rejected_group = rep(0, 5)
      n_rejected_group = sapply(1:5, function(x) rbinom(1, 
                                                        n_submitting_group[x], 
                                                        prob = as.numeric(beta_draws[id, 3, i_region, x])))
      n_rejected_voters = sum(n_rejected_group)
      data <- df_subset_collapsed[i, c("group_id", "voters_white", "voters_black", "voters_hispanic", "voters_asian", "voters_other")]
      add <-  c(n_absentee_voters, n_absentee_group, 
                n_submitting_voters, n_submitting_group,
                n_rejected_voters, n_rejected_group)
      df_sim <- rbind(df_sim, c(count,as.character(data), add))
    }
  }
  df_sim <- as.data.frame(df_sim)
  df_sim <- turn_numeric(df_sim, -1)
  region_crosswalk <- df_subset_collapsed %>% 
    dplyr::select(State, group_id) %>% 
    distinct() %>%
    arrange(group_id)
  colnames(df_sim) <- c("sim","State", "voters_white", "voters_black", "voters_hispanic", "voters_asian", "voters_other",
                        "n_requested", "n_requested_white", "n_requested_black", "n_requested_hispanic", "n_requested_asian", "n_requested_other",
                        "n_submitted", "n_submitted_white", "n_submitted_black", "n_submitted_hispanic", "n_submitted_asian", "n_submitted_other",
                        "n_rejected", "n_rejected_white", "n_rejected_black", "n_rejected_hispanic", "n_rejected_asian", "n_rejected_other")
  df_sim$State <- region_crosswalk[as.integer(df_sim$State),"State"] %>% pull(State)
  return(df_sim)
}




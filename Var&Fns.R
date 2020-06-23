#functions used with table1() to define what summary statistics we want from continuous & categorical variables
# my.render.cont <- function(x) {
#   with(stats.apply.rounding(stats.default(x), digits=2), c("",
#                                                            "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
# }
# my.render.cat <- function(x) {
#   c("", sapply(stats.default(x), function(y) with(y,
#                                                   sprintf("%d (%0.0f %%)", FREQ, PCT))))
# }


############################################################################################
#function returns Table 1 summary statistics for a given dataset, with a column name describing group 
t1.fn <- function(dt, 
                  column.name = ""
) {  
  
  #dt = pn_dem  
  #attach(dt)
  
  t1 <- dt %>%
    dplyr::summarize(
      N = n(),
      #age
      age_mean = round(mean(age, na.rm = T), 1), 
      age_sd = round(sd(age, na.rm = T), 1), 
      #gender
      gender_male_n = sum(gender==1),
      gender_male_pct = round(gender_male_n/N*100, 1),
      gender_female_n = sum(gender==0),
      gender_female_pct = round(gender_female_n/N*100, 1),
      gender_other_n = sum(gender %in% c(2:5)),
      gender_other_pct = round(gender_other_n/N*100, 1),
      gender_na_n = sum(gender == 999), 
      gender_na_pct = round(gender_na_n/N*100, 1),
      #race
      race_known_n = sum(race_all %in% c(1:7)),
      race_ame_indian_n = sum(race_all == 1, na.rm = T),
      race_ame_indian_pct = round(race_ame_indian_n/race_known_n*100, 1),
      race_asian_n = sum(race_all == 2, na.rm = T),
      race_asian_pct = round(race_asian_n/race_known_n*100, 1),
      race_black_n = sum(race_all == 3, na.rm = T),
      race_black_pct = round(race_black_n/race_known_n*100, 1),
      race_hawaiian_n = sum(race_all == 4, na.rm = T),
      race_hawaiian_pct = round(race_hawaiian_n/race_known_n*100, 1),
      race_white_n = sum(race_all == 5, na.rm = T),
      race_white_pct = round(race_white_n/race_known_n*100, 1),
      race_other_n = sum(race_all == 6, na.rm = T),
      race_other_pct = round(race_other_n/race_known_n*100, 1),
      race_multiple_n = sum(race_all == 7, na.rm = T),
      race_multiple_pct = round(race_multiple_n/race_known_n*100, 1),
      race_na_n = sum(race_all == 999, na.rm = T),
      race_na_pct = round(race_na_n/race_known_n*100, 1),
      #hispanic
      hisp_known_n = sum(hispanic %in% c(0:1), na.rm = T),
      hisp_yes_n = sum(hispanic ==1, na.rm = T),
      hisp_yes_pct = round(hisp_yes_n/hisp_known_n*100, 1),
      hisp_no_n = sum(hispanic == 0, na.rm = T),
      hisp_no__pct = round(hisp_no_n/hisp_known_n*100, 1),
      hisp_na_n = sum(hispanic == 999, na.rm = T),
      hisp_na_pct = round(hisp_na_n/hisp_known_n*100, 1),
      #education
      edu_known_n = sum(edu %in% c(1:5)),
      edu_less_hs_n = sum(edu == 0, na.rm = T),
      edu_less_hs_pct = round(edu_less_hs_n/edu_known_n*100, 1),
      edu_hs_n = sum(edu == 1, na.rm = T),
      edu_hs_pct = round(edu_hs_n/edu_known_n*100, 1),
      edu_some_college_n = sum(edu == 2, na.rm = T),
      edu_some_college_pct = round(edu_some_college_n/edu_known_n*100, 1),
      edu_finished_college_n = sum(edu == 3, na.rm = T),
      edu_finished_college_pct = round(edu_finished_college_n/edu_known_n*100, 1),
      edu_trade_n = sum(edu == 4, na.rm = T),
      edu_trade_pct = round(edu_trade_n/edu_known_n*100, 1),
      edu_other_n = sum(edu == 5, na.rm = T),
      edu_other_pct = round(edu_other_n/edu_known_n*100, 1),
      edu_na_n = sum(edu == 999, na.rm = T),
      edu_na_pct = round(edu_na_n/edu_known_n*100, 1),
      #tenure
      tenure_hca_mean =  round(mean(tenure_hca, na.rm = T), 1),
      tenure_hca_sd =  round(sd(tenure_hca, na.rm = T), 1),
      #injury
      injury_known_n = sum(injury %in% c(0:1), na.rm = T),
      injury_yes_n = sum(injury == 1, na.rm = T),
      injury_yes_pct = round(injury_yes_n/injury_known_n*100, 1),
      injury_no_n = sum(injury == 0, na.rm = T),
      injury_no_pct = round(injury_no_n/injury_known_n*100, 1),
      injury_na_n = sum(injury == 999, na.rm = T),
      injury_na_pct = round(injury_na_n/injury_known_n*100, 1),
      #worker's comp claim
      wc_known_n = sum(workers_comp %in% c(0:1), na.rm = T),
      wc_yes_n = sum(workers_comp == 1, na.rm = T),
      wc_yes_pct = round(wc_yes_n/wc_known_n*100, 1),
      wc_no_n = sum(workers_comp == 0, na.rm = T),
      wc_no_pct = round(wc_no_n/wc_known_n*100, 1),
      wc_na_n = sum(workers_comp == 999, na.rm = T),
      wc_na_pct = round(wc_na_n/wc_known_n*100, 1)
      
    ) %>%
    mutate(
      #age
      "Age (mean, SD)" = paste0(age_mean, " (", age_sd, ")"),
      #gender
      "Male (n, %)" = paste0(gender_male_n, " (", gender_male_pct, "%)"),
      "Female (n, %)" = paste0(gender_female_n, " (", gender_female_pct, "%)"),
      "Other gender (n, %)" = paste0(gender_other_n, " (", gender_other_pct, "%)"),
      "Gender missing (n, %)" = paste0(gender_na_n, " (", gender_na_pct, "%)"),
      #race
      "American Indian or Alaska Native" = paste0(race_ame_indian_n, " (", race_ame_indian_pct, "%)"),
      "Asian" = paste0(race_asian_n, " (", race_asian_pct, "%)"),
      "Black or African American" = paste0(race_black_n, " (", race_black_pct, "%)"),
      "Native Hawaiian or Other Pacific Islander" = paste0(race_hawaiian_n, " (", race_hawaiian_pct, "%)"),
      "White" = paste0(race_white_n, " (", race_white_pct, "%)"),
      "Other Race" = paste0(race_other_n, " (", race_other_pct, "%)"),
      "More Than One Race" = paste0(race_multiple_n, " (", race_multiple_pct, "%)"),
      "Race Missing" = paste0(race_na_n, " (", race_na_pct, "%)"),
      #hispanic
      "Hispanic (n, %)" = paste0(hisp_yes_n, " (", hisp_yes_pct, "%)"),
      "Hispanic missing (n, %)" = paste0(hisp_na_n, " (", hisp_na_pct, "%)"),
      #education
      "Less than high school" = paste0(edu_less_hs_n, " (", edu_less_hs_pct, "%)"),
      "High school or GED" = paste0(edu_hs_n, " (", edu_hs_pct, "%)"),
      "Some college" = paste0(edu_some_college_n, " (", edu_some_college_pct, "%)"),
      "Finished college" = paste0(edu_finished_college_n, " (", edu_finished_college_pct, "%)"),
      "Trade/vocational school" = paste0(edu_trade_n, " (", edu_trade_pct, "%)"),
      "Other education" = paste0(edu_other_n, " (", edu_other_pct, "%)"),
      "Education missing" = paste0(edu_na_n, " (", edu_na_pct, "%)"),
      #tenure
      "Years of experience working as a HCA (mean, SD)" = paste0(tenure_hca_mean, " (", tenure_hca_sd, ")"),
      "Have had an injury or health problem related to HCA work (n, %)" = paste0(injury_yes_n, " (", injury_yes_pct, "%)"),
      "Injury response missing (n, %)" = paste0(injury_na_n, " (", injury_na_pct, "%)"),
      "Have filed a worker's compensation claim (n, %)" = paste0(wc_yes_n, " (", wc_yes_pct, "%)"),
      "Worker's compensation claim response missing (n, %)" = paste0(wc_na_n, " (", wc_na_pct, "%)"),
      
    ) %>%
    #drop repeat columns
    select(
      N, 
      "Age (mean, SD)":"Worker's compensation claim response missing (n, %)"
    ) #%>%
  
  # transpose
  t1 <- t(t1) %>% 
    as.data.frame() %>%
    rownames_to_column() %>%
    mutate(V1 = as.character(V1))
  
  #rename "V1" column  
  names(t1) <- c("Measure", column.name)
  
  return(t1) 
}

##########################################################
#returns responses of particular categories
training.eval.fn <- function(dt) {  
  
  #dt = pn_eval  
  #attach(dt)
  
  #agree or strongly agree
  yes <- c(4:5)
  
  t1 <- dt %>%
    group_by(training) %>%
    dplyr::summarize(
      N = n(),
      recommend_n = sum(recommend_training %in% yes),
      recommend_pct = mean(recommend_training %in% yes)*100,
      concepts_n = sum(concepts %in% yes),
      concepts_pct = mean(concepts %in% yes)*100,  
      info_practice_n = sum(info_practice %in% yes),
      info_practice_pct = mean(info_practice %in% yes)*100,  
      teaching_n = sum(teaching_approach %in% yes),
      teaching_pct = mean(teaching_approach %in% yes)*100
    ) %>%
    round(1) %>%
    mutate(
      
      "I would recommend this training to another navigator doing similar work on the RENEW project." = paste0(recommend_n, " (", recommend_pct, "%)"),
      "The trainers explained concepts in a way that was understandable to me." = paste0(concepts_n, " (", concepts_pct, "%)"),
      "The amount of new information to learn and opportunity to review and practice was the right balance for me." = paste0(info_practice_n, " (", info_practice_pct, "%)"),
      "The trainers’ teaching approach supported my learning." = paste0(teaching_n, " (", teaching_pct, "%)"),
    ) %>%
    select(training, N,
           11:ncol(.)
    ) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()  
  
  names(t1) <- c("Measure", "Training 1", "Training 2")
  
  t1 <- t1 %>%
    #drop training row
    slice(-1)
  
  return(t1)  
  
}
####################################################################################################
# renames variables based on (grouping) variable values in 1st row (i.e., Q2.2)

row1_to_names <- function(dt) {
  dt2 <- dt %>%
    rename_at(vars(contains("V")), ~dt[1, .] %>% unlist() %>% as.vector()) %>%
    slice(-1)
  
  return(dt2)
  
}

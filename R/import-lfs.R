read_lfs_years <- memoise::memoise(function(parent_dir, years) {
  purrr::map_dfr(
    years, 
    function(x) {
      d <- bpcandata::read_lfs_pumf(here::here(parent_dir, x))
      bpcandata::encode_lfs_factors(d$records, d$codebook)
    }
  )
})

import_lfs_data <- memoise::memoise(function() {
  d <- read_lfs_years("data", 2015:2024)
  school_months <- c(
    "January", 
    "February", 
    "March", 
    "April", 
    "September",
    "October", 
    "November", 
    "December"
  )

  d |> 
    dplyr::mutate(
      school_month = SURVMNTH %in% school_months,
      working_age_lgl = !(AGE_12 %in% c("65 to 69 years", "70 and over")), 
      youth_lgl = AGE_12 %in% c(
        "15 to 19 years", 
        "20 to 24 years", 
        "25 to 29 years"
      ),
      youth_fct = factor(youth_lgl + 1, labels = c("Adults 30-64", "Youth 15-29")),
      has_pse_lgl = EDUC %in% c(
        "Postsecondary certificate or diploma", 
        "Bachelor's degree", 
        "Above bachelor's degree"
      ),
      participating_lgl = LFSSTAT != "Not in labour force",
      employed_lgl = participating_lgl & (LFSSTAT != "Unemployed"),
      unemployed_lgl = dplyr::case_when(
        participating_lgl ~ !employed_lgl,
        TRUE ~ NA
      ), 
      involuntary_leave_lgl = as.integer(WHYLEFTN) > 7,
      job_multiple_lgl = MJH == "Multiple jobholder",
      job_involuntary_pt_lgl = case_when(
        !employed_lgl ~ NA, 
        FTPTMAIN == "Full-time" ~ FALSE,
        TRUE ~ as.integer(WHYPT) > 6 
      ),
      job_temporary_lgl = PERMTEMP != "Permanent",
      job_gig_lgl = dplyr::case_when(
        is.na(COWMAIN) ~ NA, 
        TRUE ~ as.integer(COWMAIN) %in% c(4, 6)
      ),
      has_hs_lgl = as.integer(EDUC) > 2,
      enrolled_lgl = SCHOOLN != "Non-student",
      enrolled_hs_lgl = enrolled_lgl & (as.integer(EDUC) < 3), 
      hs_dropout_lgl = !has_hs_lgl & !enrolled_lgl,
      enrolled_pse_lgl = enrolled_lgl & (as.integer(EDUC) >= 3),
      pse_dropout_lgl = EDUC == "Some postsecondary" & !enrolled_lgl,
      neet_lgl = !employed_lgl & !enrolled_lgl,
      newcomer_lgl = as.numeric(IMMIG) == 1,
      z_hrlyearn = scale(HRLYEARN), 
      never_worked_lgl = !(employed_lgl) & EVERWORK %in% "No, never worked", 
      job_searching_lgl = dplyr::case_when(
        unemployed_lgl %in% c(1) ~ !is.na(PRIORACT),
        TRUE ~ NA
      ),
      across(
        matches("^LK"),
        ~dplyr::case_when(
          job_searching_lgl ~ as.numeric(.x == 1), 
          TRUE ~ NA_real_
        )
      ),
      segment = dplyr::case_when(
        !youth_lgl ~ "adult_30_plus",
        enrolled_hs_lgl ~ "student_hs",
        enrolled_pse_lgl ~ "student_pse",
        hs_dropout_lgl ~ "hs_dropout",
        pse_dropout_lgl ~ "pse_dropout",
        newcomer_lgl & has_pse_lgl ~ "newcomer_pse", 
        newcomer_lgl ~ "newcomer_no_pse", 
        has_pse_lgl ~ "lt_canadian_pse", 
        TRUE ~ "lt_canadian_no_pse" 
      ) |> forcats::fct_relevel(
        "adult_30_plus", 
        "student_hs", 
        "hs_dropout", 
        "student_pse",
        "pse_dropout", 
        "newcomer_pse",
        "newcomer_no_pse",
        "lt_canadian_pse",
        "lt_canadian_no_pse"
      )
    ) |> 
    dplyr::filter(working_age_lgl) |> 
    dplyr::mutate(
      across(matches("_lgl$"), as.numeric)
    )
})

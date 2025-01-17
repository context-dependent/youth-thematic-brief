library(bpcandata)
library(tidyverse)
library(srvyr)
source("R/scratch.R")
d_ <- import_lfs_data()

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

t00 <- d_ |> 
  filter(SURVMNTH %in% school_months) |> 
  group_by(SURVYEAR, SURVMNTH) |> 
  count(youth_fct, wt = FINALWT) |>
  mutate(p = n / sum(n)) |>
  group_by(youth_fct) |>
  mutate(idx_15 = round(p / first(p) * 100) - 100) |>
  ungroup()

t00 |> ggplot(aes(SURVMNTH, idx_15)) +
  facet_grid(.~SURVYEAR) +  
  geom_path(aes(group = youth_fct, color = youth_fct)) + 
  scale_x_discrete(labels = function(x) stringr::str_sub(x, end = 1L), expand = expansion(c(0.01, 0.01))) +
  bptheme::theme_blueprint()


bpcandata:::generate_lfs_bootstrap_weights

t01 <- d_ |> 
  group_by(SURVYEAR, youth_fct) |> 
  summarize(
    participation = weighted.mean(participating_lgl, FINALWT),
    employment = weighted.mean(employed_lgl, FINALWT),
    unemployment = weighted.mean(unemployed_lgl, FINALWT, na.rm = TRUE),
    enrollment = weighted.mean(enrolled_lgl, FINALWT),
    neet = weighted.mean(neet_lgl, FINALWT)
  )

t02 <- d_ |> 
  group_by(SURVYEAR, SURVMNTH) |> 
  count(segment, wt = FINALWT) |> 
  mutate(p = n / sum(n)) |> 
  group_by(segment) |> 
  mutate(idx_15 = round(p / first(p) * 100) - 100) |> 
  ungroup()

# Segmentation does not hold for spring / summer months
# ids summer breakers as dropouts
t02 |> 
  ggplot(aes(SURVMNTH, idx_15)) +
  facet_grid(segment~SURVYEAR) +  
  geom_path(aes(group = segment, color = segment)) + 
  scale_x_discrete(labels = function(x) stringr::str_sub(x, end = 1L), expand = expansion(c(0.01, 0.01))) +
  bptheme::theme_blueprint()

t03 <- d_ |> 
  filter(SURVMNTH %in% school_months) |> 
  group_by(SURVYEAR, SURVMNTH) |> 
  count(segment, wt = FINALWT) |>
  mutate(p = n / sum(n)) |>
  group_by(segment) |>
  mutate(idx_15 = round(p / first(p) * 100) - 100) |>
  ungroup()

t03 |> 
  ggplot(aes(SURVMNTH, idx_15)) +
  facet_grid(.~SURVYEAR) +  
  geom_path(aes(group = segment, color = segment)) + 
  scale_x_discrete(labels = function(x) stringr::str_sub(x, end = 1L), expand = expansion(c(0.01, 0.01))) +
  bptheme::theme_blueprint()


t04 <- d_ |> 
  filter(
    youth_lgl == 1,
    SURVMNTH %in% school_months
  ) |> 
  group_by(SURVYEAR, SURVMNTH) |> 
  count(segment, wt = FINALWT) |>
  mutate(p = n / sum(n)) |>
  group_by(segment) |>
  mutate(idx_15 = round(p / first(p) * 100) - 100) |>
  ungroup()

t04 |> 
  ggplot(aes(SURVMNTH, p)) +
  facet_grid(.~SURVYEAR) +  
  geom_path(aes(group = segment, color = segment)) + 
  scale_x_discrete(
    labels = function(x) stringr::str_sub(x, end = 1L), expand = expansion(c(0.01, 0.01))
  ) +
  bptheme::theme_blueprint()

# While Canada's working-age youth population has historically been, and continues to be 
# comprised mostly of long-term Canadians, the proportion of newcomers both with and without
# post-secondary education has increased dramatically since 2015.

t05 <- d_ |> 
  filter(youth_lgl == 1, SURVMNTH %in% school_months) |> 
  group_by(SURVYEAR, PROV) |> 
  count(segment, wt = FINALWT) |> 
  mutate(p = n / sum(n)) |>
  group_by(PROV, segment) |> 
  summarize(
    delta_abs = last(p) - first(p),
    delta_idx = round(last(p) / first(p) * 100) - 100,
    .groups = "drop"
  )

t05 |> 
  filter(segment %in% c("newcomer_pse", "newcomer_no_pse"))

# In absolute terms, representation of newcomers with PSE among working-age youth
# increased in all provinces.
# PEI experienced the highest absolute increase (+3.4 ppt), 
# while Saskatchewan experienced the lowest (+.5 ppt).

t05 |> 
  filter(segment %in% c("newcomer_pse", "newcomer_no_pse")) |>
  ggplot(aes(delta_abs, PROV)) + 
  geom_col(aes(fill = segment), position = "dodge") +
  facet_grid(segment~.)

# Indexed to 2015, the change was most dramatic in the Atlantic provinces.
# Newfoundland and Labrador experienced the greatest relative increase, 
# with the proportion of newcomers with PSE among working-age youth increasing
# by an order of magnitude (12.6 X)

t05 |> 
  filter(segment %in% c("newcomer_pse", "newcomer_no_pse")) |>
  ggplot(aes(delta_idx, PROV)) + 
  geom_col(aes(fill = segment), position = "dodge") +
  facet_grid(segment~.)




g01 <- d_ |> 
  modelr::data_grid(youth_lgl)

m01 <- d_ |> 
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        z_hrlyearn ~ youth_lgl + factor(SURVMNTH), 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(
      mod, 
      ~broom::tidy(.x, conf.int = TRUE) |>
        bind_rows(
          multcomp::glht(.x, "(Intercept) + youth_lgl = 0") |>
            broom::tidy() |>
            select(
                -null.value
            ) |>
            rename(
                term = contrast,
                p.value = adj.p.value
            ) |>
            mutate(
                conf.low = estimate - 1.96 * std.error,
                conf.high = estimate + 1.96 * std.error
            )
      )
    )
  )


m01 |> 
  select(SURVYEAR, tab) |> 
  unnest(tab) |> 
  filter(term == "youth_lgl")|>
  mutate(
    estimate = estimate * sd(d_$HRLYEARN, na.rm = TRUE), 
    std.error = std.error * sd(d_$HRLYEARN, na.rm = TRUE)
  ) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_halfeye(
    aes(ydist = distributional::dist_normal(estimate, std.error), fill = term)
  ) + 
  scale_y_continuous(
    labels = scales::dollar_format()
  ) + 
  scale_x_continuous(
    breaks = 2015:2024
  ) + 
  labs(
    title = "Youth wage disadvantage"
  )

m01 |> 
  select(SURVYEAR, tab) |> 
  unnest(tab) |> 
  filter(term %in% c("(Intercept)", "(Intercept) + youth_lgl")) |>
  mutate(
    estimate = mean(d$HRLYEARN, na.rm = TRUE) + estimate * sd(d_$HRLYEARN, na.rm = TRUE), 
    std.error = std.error * sd(d_$HRLYEARN, na.rm = TRUE)
  ) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_halfeye(
    aes(ydist = distributional::dist_normal(estimate, std.error), fill = term)
  ) + 
  scale_y_continuous(
    labels = scales::dollar_format()
  ) + 
  scale_x_continuous(
    breaks = 2015:2024
  ) + 
  labs(
    title = "A growing gap"
  )


m02 <- d_ |>
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        employed_lgl ~ youth_lgl + factor(SURVMNTH), 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(
      mod, 
      ~ broom::tidy(.x, conf.int = TRUE) |> 
        dplyr::bind_rows(
          multcomp::glht(.x, "(Intercept) + youth_lgl = 0") |>
            broom::tidy() |>
            dplyr::select(
                -null.value
            ) |>
            dplyr::rename(
                term = contrast,
                p.value = adj.p.value
            ) |>
            dplyr::mutate(
                conf.low = estimate - 1.96 * std.error,
                conf.high = estimate + 1.96 * std.error
            )
        )
    )
  )

m02 |> 
  select(SURVYEAR, tab) |> 
  unnest(tab) |> 
  filter(term == "youth_lgl") |>
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_halfeye(
    aes(ydist = distributional::dist_normal(estimate, std.error))
  ) + 
  scale_x_continuous(
    breaks = 2015:2024
  ) + 
  labs(
    title = "Youth employment rate disadvantage"
  )

m02 |> 
  select(SURVYEAR, tab) |> 
  unnest(tab) |> 
  filter(term %in% c("(Intercept)", "(Intercept) + youth_lgl")) |>
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_halfeye(
    aes(ydist = distributional::dist_normal(estimate, std.error), fill = term)
  ) + 
  scale_x_continuous(
    breaks = 2015:2024
  ) + 
  labs(
    title = "Youth lagging in pandemic recovery"
  )

g03 <- d_ |> 
  modelr::data_grid(
    youth_lgl, 
    newcomer_lgl
  )

m03 <- d_ |>
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        employed_lgl ~ youth_lgl * newcomer_lgl, 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(mod, ~broom::tidy(.x, conf.int = TRUE)), 
    fitted = map(
      mod, 
      ~  broom::augment(
          .x,
          newdata = g03, 
          se_fit = TRUE
        )
    )
  )

m03 |> 
  select(SURVYEAR, tab) |> 
  unnest(tab) |> 
  filter(term |> str_detect("youth_lgl|newcomer_lgl")) |>
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_halfeye(
    aes(ydist = distributional::dist_normal(estimate, std.error))
  ) +
  facet_wrap(~term, scales = "free_y")

m03 |> 
  select(SURVYEAR, fitted) |> 
  unnest(fitted) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_slab(
    aes(ydist = distributional::dist_normal(.fitted, .se.fit), 
    fill = youth_lgl)
  ) + 
  facet_wrap(~newcomer_lgl)

m04 <- d_ |>
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        neet_lgl ~ youth_lgl * newcomer_lgl, 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(mod, ~broom::tidy(.x, conf.int = TRUE)), 
    fitted = map(
      mod, 
      ~  broom::augment(
          .x,
          newdata = g03, 
          se_fit = TRUE
        )
    )
  )

m04 |> 
  select(SURVYEAR, fitted) |> 
  unnest(fitted) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_slab(
    aes(ydist = distributional::dist_normal(.fitted, .se.fit), 
    fill = youth_lgl)
  ) + 
  facet_wrap(~newcomer_lgl)

g05 <- d_ |> 
  modelr::data_grid(
    youth_lgl, 
    newcomer_lgl, 
    has_pse_lgl
  )

m05 <- d_ |> 
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        HRLYEARN ~ youth_lgl * newcomer_lgl * has_pse_lgl, 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(mod, ~broom::tidy(.x, conf.int = TRUE)), 
    fitted = map(
      mod, 
      ~  broom::augment(
          .x,
          newdata = g05, 
          se_fit = TRUE
        )
    )
  )

m05 |> 
  select(SURVYEAR, fitted) |> 
  unnest(fitted) |> 
  mutate(
    across(matches("lgl$"), ~.x==1),
    segment = case_when(
      newcomer_lgl & has_pse_lgl ~ "Newcomer with PSE", 
      newcomer_lgl ~ "Newcomer without PSE",
      has_pse_lgl ~ "Non-newcomer with PSE",
      TRUE ~ "Non-newcomer without PSE"
    )
  ) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_slab(
    aes(
      ydist = distributional::dist_normal(.fitted, .se.fit), 
      fill = youth_lgl
    ),
    normalize = "xy"
  ) + 
  facet_wrap(~segment, nrow = 1)



m06 <- d_ |> 
  filter(as.numeric(AGE_12) > 1) |> 
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        employed_lgl ~ youth_lgl * newcomer_lgl * has_pse_lgl, 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(mod, ~broom::tidy(.x, conf.int = TRUE)), 
    fitted = map(
      mod, 
      ~  broom::augment(
          .x,
          newdata = g05, 
          se_fit = TRUE
        )
    )
  )

m06 |> 
  select(SURVYEAR, fitted) |> 
  unnest(fitted) |> 
  mutate(
    across(matches("lgl$"), ~.x==1),
    segment = case_when(
      newcomer_lgl & has_pse_lgl ~ "Newcomer with PSE", 
      newcomer_lgl ~ "Newcomer without PSE",
      has_pse_lgl ~ "Non-newcomer with PSE",
      TRUE ~ "Non-newcomer without PSE"
    )
  ) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_slab(
    aes(
      ydist = distributional::dist_normal(.fitted, .se.fit), 
      fill = youth_lgl
    ),
    normalize = "xy"
  ) + 
  facet_wrap(~segment, nrow = 1)



m07 <- d_ |> 
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        unemployed_lgl ~ youth_lgl * newcomer_lgl * has_pse_lgl, 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(mod, ~broom::tidy(.x, conf.int = TRUE)), 
    fitted = map(
      mod, 
      ~  broom::augment(
          .x,
          newdata = g05, 
          se_fit = TRUE
        )
    )
  )

m07 |> 
  select(SURVYEAR, fitted) |> 
  unnest(fitted) |> 
  mutate(
    across(matches("lgl$"), ~.x==1),
    segment = case_when(
      newcomer_lgl & has_pse_lgl ~ "Newcomer with PSE", 
      newcomer_lgl ~ "Newcomer without PSE",
      has_pse_lgl ~ "Non-newcomer with PSE",
      TRUE ~ "Non-newcomer without PSE"
    )
  ) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_slab(
    aes(
      ydist = distributional::dist_normal(.fitted, .se.fit), 
      fill = youth_lgl
    ),
    normalize = "xy"
  ) + 
  facet_wrap(~segment, nrow = 1)

m08 <- d_ |> 
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        enrolled_lgl ~ youth_lgl * newcomer_lgl * has_pse_lgl, 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(mod, ~broom::tidy(.x, conf.int = TRUE)), 
    fitted = map(
      mod, 
      ~  broom::augment(
          .x,
          newdata = g05, 
          se_fit = TRUE
        )
    )
  )

m08 |> 
  select(SURVYEAR, fitted) |> 
  unnest(fitted) |> 
  mutate(
    across(matches("lgl$"), ~.x==1),
    segment = case_when(
      newcomer_lgl & has_pse_lgl ~ "Newcomer with PSE", 
      newcomer_lgl ~ "Newcomer without PSE",
      has_pse_lgl ~ "Non-newcomer with PSE",
      TRUE ~ "Non-newcomer without PSE"
    )
  ) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_slab(
    aes(
      ydist = distributional::dist_normal(.fitted, .se.fit), 
      fill = youth_lgl
    ),
    normalize = "xy"
  ) + 
  facet_wrap(~segment, nrow = 1)


g09 <- d_ |> 
  modelr::data_grid(
    youth_lgl, 
    has_pse_lgl, 
    enrolled_lgl
  )

m09 <- d_ |> 
  group_by(SURVYEAR) |> 
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat,
      ~ lm(
        HRLYEARN ~ youth_lgl * enrolled_lgl * has_pse_lgl, 
        data = .x,
        weights = FINALWT
      )
    ), 
    tab = map(mod, ~broom::tidy(.x, conf.int = TRUE)), 
    fitted = map(
      mod, 
      ~  broom::augment(
          .x,
          newdata = g09, 
          se_fit = TRUE
        )
    )
  )

m09 |> 
  select(SURVYEAR, fitted) |> 
  unnest(fitted) |> 
  mutate(
    across(matches("lgl$"), ~.x==1),
    segment = case_when(
      enrolled_lgl & has_pse_lgl ~ "Current Student (Post-grad)",
      enrolled_lgl ~ "Current Student (HS / BA)", 
      has_pse_lgl ~ "Non-student with PSE", 
      TRUE ~ "Non-student without PSE"
    )
  ) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_slab(
    aes(
      ydist = distributional::dist_normal(.fitted, .se.fit), 
      fill = youth_lgl
    ),
    normalize = "xy"
  ) + 
  facet_wrap(~segment, nrow = 1)


m10 <- d_ |> 
  group_by(SURVYEAR) |>
  filter(!employed_lgl) |>  
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat, 
      ~ lm(
        involuntary_leave_lgl ~ youth_lgl * has_pse_lgl,
        data = .x, 
        weights = FINALWT
      )
    ),
    tab = map(
      mod, 
      ~ broom::tidy(.x, conf.int = TRUE) |> 
        bind_rows(
          multcomp::glht(
            .x, 
            c(
              "youth_lgl + has_pse_lgl + youth_lgl:has_pse_lgl = 0"
            )) |>
            broom::tidy() |>
            select(
                -null.value
            ) |>
            rename(
                term = contrast,
                p.value = adj.p.value
            ) |>
            mutate(
                conf.low = estimate - 1.96 * std.error,
                conf.high = estimate + 1.96 * std.error
            )
      )
    )
  )


m10 |> 
  select(SURVYEAR, tab) |> 
  unnest(tab) |> 
  filter(term %in% c("youth_lgl", "youth_lgl + has_pse_lgl + youth_lgl:has_pse_lgl")) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_halfeye(
    aes(ydist = distributional::dist_normal(estimate, std.error), fill = term)
  ) + 
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title = "Involuntary job loss", 
    subtitle = "Effect of youth status and PSE education"
  )


m11 <- d_ |> 
  group_by(SURVYEAR) |>
  filter(!employed_lgl) |>  
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat, 
      ~ lm(
        DURJLESS ~ youth_lgl * has_pse_lgl,
        data = .x, 
        weights = FINALWT
      )
    ),
    tab = map(
      mod, 
      ~ broom::tidy(.x, conf.int = TRUE) |> 
        bind_rows(
          multcomp::glht(
            .x, 
            c(
              "youth_lgl + has_pse_lgl + youth_lgl:has_pse_lgl = 0"
            )) |>
            broom::tidy() |>
            select(
                -null.value
            ) |>
            rename(
                term = contrast,
                p.value = adj.p.value
            ) |>
            mutate(
                conf.low = estimate - 1.96 * std.error,
                conf.high = estimate + 1.96 * std.error
            )
      )
    )
  )


m11 |> 
  select(SURVYEAR, tab) |> 
  unnest(tab) |> 
  filter(term %in% c("youth_lgl", "youth_lgl + has_pse_lgl + youth_lgl:has_pse_lgl")) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_halfeye(
    aes(ydist = distributional::dist_normal(estimate, std.error), fill = term)
  ) + 
  scale_y_continuous(labels = scales::number_format(scale = .1)) + 
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title = "Jobless months", 
    subtitle = "Effect of youth status and PSE education"
  )


m12 <- d_ |> 
  group_by(SURVYEAR) |>
  filter(!employed_lgl) |>  
  group_nest(.key = "dat") |> 
  mutate(
    mod = map(
      dat, 
      ~ lm(
        never_worked_lgl ~ youth_lgl * has_pse_lgl,
        data = .x, 
        weights = FINALWT
      )
    ),
    tab = map(
      mod, 
      ~ broom::tidy(.x, conf.int = TRUE) |> 
        bind_rows(
          multcomp::glht(
            .x, 
            c(
              "youth_lgl + has_pse_lgl + youth_lgl:has_pse_lgl = 0"
            )) |>
            broom::tidy() |>
            select(
                -null.value
            ) |>
            rename(
                term = contrast,
                p.value = adj.p.value
            ) |>
            mutate(
                conf.low = estimate - 1.96 * std.error,
                conf.high = estimate + 1.96 * std.error
            )
      )
    )
  )


m12 |> 
  select(SURVYEAR, tab) |> 
  unnest(tab) |> 
  filter(term %in% c("youth_lgl", "youth_lgl + has_pse_lgl + youth_lgl:has_pse_lgl")) |> 
  ggplot(aes(SURVYEAR)) + 
  ggdist::stat_halfeye(
    aes(ydist = distributional::dist_normal(estimate, std.error), fill = term)
  ) + 
  scale_y_continuous(labels = scales::number_format(scale = .1)) + 
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title = "Rate of never worked", 
    subtitle = "Effect of youth status and PSE education"
  )

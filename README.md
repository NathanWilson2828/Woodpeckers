## 1. Load Libraries 
```{r}
library(tidyverse)
library(lubridate)
library(broom)
library(dplyr)
library(ggplot2)
library(kableExtra)
library(FSA)
library(purrr)
library(rstatix)
```

## 2. Define Weeks Function
```{r}
define_weeks <- function(d) {
  case_when(
    d >= as.Date("2024-05-05") & d <= as.Date("2024-05-12") ~ "1",
    d >= as.Date("2024-05-13") & d <= as.Date("2024-05-19") ~ "2",
    d >= as.Date("2024-05-20") & d <= as.Date("2024-05-26") ~ "3",
    d >= as.Date("2024-05-27") & d <= as.Date("2024-06-02") ~ "4",
    d >= as.Date("2024-06-03") & d <= as.Date("2024-06-09") ~ "5",
    d >= as.Date("2024-06-10") & d <= as.Date("2024-06-16") ~ "6",
    d >= as.Date("2024-06-17") & d <= as.Date("2024-06-23") ~ "7",
    d >= as.Date("2024-06-24") & d <= as.Date("2024-06-30") ~ "8",
    d >= as.Date("2024-07-01") & d <= as.Date("2024-07-07") ~ "9",
    d >= as.Date("2024-07-08") & d <= as.Date("2024-07-13") ~ "10",
    d >= as.Date("2024-07-14") & d <= as.Date("2024-07-21") ~ "11",
    d >= as.Date("2024-07-24") & d <= as.Date("2024-07-28") ~ "12",
    d >= as.Date("2024-07-29") & d <= as.Date("2024-08-05") ~ "13",
    d >= as.Date("2024-08-06") & d <= as.Date("2024-08-12") ~ "14",
    d >= as.Date("2024-08-13") & d <= as.Date("2024-08-19") ~ "15",
    d >= as.Date("2024-08-20") & d <= as.Date("2024-08-26") ~ "16",
    d >= as.Date("2024-08-27") & d <= as.Date("2024-09-02") ~ "17",
    TRUE ~ NA_character_)}
```

## 3. Import Data
```{r}
Birds <- read_csv("Birds11.csv") |>
  transmute(
    Species,
    UTC.Date   = ymd_hms(`UTC.Date`, tz = "UTC") |> with_tz("America/Chicago"),
    local.date = dmy(`Local Date`),
    Count,
    Week       = define_weeks(local.date)) 
Birds
```

## 4. Summarize Daily Totals and Fill Missing Days
```{r}
Birds2 <- Birds |>
  group_by(Species, local.date) |>
  summarise(Count = sum(Count), .groups = "drop") |>
  complete(
    Species,
    local.date = seq(min(local.date), max(local.date), by = "day"),
    fill = list(Count = 0)) %>%
  mutate(Week = define_weeks(local.date)) |>
  filter(!is.na(Week))

Birds2
```

## 5. Define Species Groups and Conditions
```{r}
woodpeckers <- c(
  "Pileated Woodpecker", "Downy Woodpecker",
  "Red-bellied Woodpecker", "Red-headed Woodpecker",
  "Hairy Woodpecker", "Northern Flicker")

species_groups <- c(woodpeckers, "All Woodpeckers", "All Species")

controls <- list(
  Week1      = "1",
  Weeks15_17 = c("15", "16", "17"))

conditions <- list(
  Food      = 2:5,
  Decoy     = 6:8,
  FoodDecoy = 9:10,
  Playback  = 11:14)
```

## 5.5 Woodpecker Species Abbreviations
```{r}
woodpecker_abbr <- c(
  "Downy Woodpecker"       = "DOWO",
  "Hairy Woodpecker"       = "HAWO",
  "Red-bellied Woodpecker" = "RBWO",
  "Red-headed Woodpecker"  = "RHWO",
  "Pileated Woodpecker"    = "PIWO",
  "Northern Flicker"       = "NOFI",
  "ALLW"                   = "ALLWP")
```

## 5.75 Total Woodpecker Calls per Species 
```{r}
# 1) Tally & plot in one chain
Birds2 %>%
  filter(Species %in% woodpeckers) %>%
  count(Species, wt = Count, name = "total_calls") %>%
  mutate(Species = recode(Species, !!!woodpecker_abbr)) %>%
  ggplot(aes(x = Species, y = total_calls)) +
    geom_col(fill = "darkgrey", width = 0.9) +
    theme_minimal() +
    labs(title = "Total woodpecker calls per species") +
    theme(
      axis.title    = element_blank(),
      axis.text.x   = element_text(angle = 45, hjust = 1, size = 14),
      plot.title    = element_text(size = 16, face = "bold", hjust = 0.5))
```

# 6. Organizing for Kruskal–Wallis and Dunn’s Tests by Condition
```{r}
# Define Conditions
condition_map <- list(
  Week1      = controls$Week1,
  Weeks15_17 = controls$Weeks15_17,
  Food       = conditions$Food,
  Decoy      = conditions$Decoy,
  FoodDecoy  = conditions$FoodDecoy,
  Playback   = conditions$Playback)

# Filter and tag data with Condition factor
Birds2_conditions <- Birds2 |>
  filter(
    Week    %in% unlist(condition_map),
    Species %in% woodpeckers) |>
  mutate(
    Condition = factor(
      case_when(
        Week %in% condition_map$Week1      ~ "Week1",
        Week %in% condition_map$Weeks15_17 ~ "Weeks15_17",
        Week %in% condition_map$Food       ~ "Food",
        Week %in% condition_map$Decoy      ~ "Decoy",
        Week %in% condition_map$FoodDecoy  ~ "FoodDecoy",
        Week %in% condition_map$Playback   ~ "Playback"),
      levels = names(condition_map)))

Birds2_conditions
```

## 7. Separate into different species. 
```{r}
data_list <- c(
  "All Woodpeckers" = list(Birds2_conditions),
  # use purrr::map (which understands the ~ shorthand)
  set_names(
    map(woodpeckers, ~ Birds2_conditions |>
          filter(Species == .x)),
    woodpeckers))
data_list
```

## 8. Do KW Test and Dunn Test for conditions and put into table. 
```{r}
# 2. Kruskal–Wallis omnibus tests
kw_results_condition <- imap_dfr(data_list, ~ {
  test <- kruskal.test(Count ~ Condition, data = .x)
  tibble(
    Species     = .y,
    H_statistic = unname(test$statistic),
    p_value     = test$p.value)})

# 3. Dunn’s post-hoc tests (Bonferroni)
dunn_results_condition <- imap_dfr(data_list, ~ {
  res <- dunnTest(Count ~ Condition, data = .x, method = "bonferroni")$res
  as_tibble(res) %>%
    rename(Dunn_p_adj = P.adj) %>%
    mutate(Species = .y) %>%
    select(Species, Comparison, Dunn_p_adj)})

# 4. Display nicely formatted tables
kw_results_condition %>%
  kable(
    caption = "Kruskal–Wallis: Count by Condition",
    digits  = 4
  ) %>%
  kable_styling(full_width = FALSE)

dunn_results_condition %>%
  pivot_wider(
    names_from  = Species,
    values_from = Dunn_p_adj
  ) %>%
  arrange(Comparison) %>%
  kable(
    caption = "Dunn’s Bonferroni-adjusted p-values",
    digits  = 4
  ) %>%
  kable_styling(full_width = FALSE)
```

## 9. Total Bird Calls Per Condition and Per species
```{r}
total_cond_calls <- Birds2_conditions %>%
  filter(Species %in% woodpeckers) %>%
  group_by(Condition, Species) %>%
  summarise(
    total_calls = sum(Count),
    .groups     = "drop"
  ) %>%
  pivot_wider(
    names_from   = Species,
    values_from  = total_calls,
    values_fill  = 0
  ) %>%
  mutate(
    All_Woodpeckers = rowSums(across(all_of(woodpeckers)))
  ) %>%
  # ensure conditions appear in your original order
  arrange(factor(Condition, levels = names(conditions)))

# Display the table
total_cond_calls %>%
  kable(
    caption = "Total woodpecker calls per condition by species and overall",
    digits  = 0
  ) %>%
  kable_styling(full_width = FALSE)
```

## 10. KW and Dunn Test on the individual weeks 
```{r}
# Kruskal–Wallis tests
kw_results_weekly <- map_dfr(species_groups, function(sp) {
  df <- switch(sp,
    "All Woodpeckers" = Birds2 %>% filter(Species %in% woodpeckers),
    "All Species"     = Birds2,
    Birds2 %>% filter(Species == sp))
  kw <- kruskal.test(Count ~ Week, data = df)
  tibble(
    Species     = sp,
    H_statistic = unname(kw$statistic),
    p_value     = kw$p.value)})

# 4. Dunn’s post‐hoc (Bonferroni) for each group across all 17 weeks
dunn_results_weekly <- map_dfr(species_groups, function(sp) {
  df <- switch(sp,
    "All Woodpeckers" = Birds2 %>% filter(Species %in% woodpeckers),
    "All Species"     = Birds2,
    Birds2 %>% filter(Species == sp))
  dt <- dunnTest(Count ~ Week, data = df, method = "bonferroni")$res
  as_tibble(dt) %>%
    rename(Dunn_Comparison = Comparison, Dunn_p_adj = P.adj) %>%
    mutate(Species = sp) %>%
    select(Species, Dunn_Comparison, Dunn_p_adj)})

# 5. Display Kruskal–Wallis results
kw_results_weekly %>%
  kable(
    caption   = "Kruskal–Wallis: Test of Count ∼ Week for Each Group",
    digits    = 3,
    col.names = c("Species", "H statistic", "p-value")
  ) %>%
  kable_styling(full_width = FALSE)

# 6. Display Dunn’s Bonferroni‐adjusted p-values in wide format
dunn_results_weekly %>%
  pivot_wider(
    names_from  = Species,
    values_from = Dunn_p_adj) %>%
  arrange(Dunn_Comparison) %>%
  kable(
    caption   = "Dunn’s Bonferroni‐adjusted p-values: Week‐pair Comparisons by Group",
    col.names = c("Week Pair", woodpeckers, "All Woodpeckers", "All Species"),
    digits    = 3) %>%
  kable_styling(full_width = FALSE)
```

## 11. Individual Woodpecker data per week. Table and Graphs. 
```{r}
# Compute both avg and total calls in one pass
weekly_stats <- Birds2 %>%
  filter(Species %in% woodpeckers) %>%
  mutate(Week = as.integer(Week)) %>%
  group_by(Species, Week) %>%
  summarise(
    avg_calls   = mean(Count),
    total_calls = sum(Count),
    .groups      = "drop"
  )

for (sp in woodpeckers) {
  df_sp <- weekly_stats %>% filter(Species == sp)
  print(
    ggplot(df_sp, aes(x = Week, y = avg_calls)) +
      geom_col() +
      scale_x_continuous(breaks = 1:17) +
      labs(
        title = paste0("Average Daily Calls per Week — ", sp),
        x     = "Week",
        y     = "Avg calls per day"
      ) +
      theme_minimal())}

# 2. Wide‐format table of total calls (including all woodpeckers)
weekly_stats %>%
  select(Week, Species, total_calls) %>%
  pivot_wider(
    names_from  = Species,
    values_from = total_calls
  ) %>%
  mutate(All_Woodpeckers = rowSums(across(-Week))) %>%
  kable(
    caption = "Weekly Total Woodpecker Calls by Species and Overall",
    digits  = 0
  ) %>%
  kable_styling(full_width = FALSE)
```

## 13. Condition Graphs with Error Bars. 
```{r}
# 1) Prepare list of groups: each species + combined
plot_groups <- c(woodpeckers, "All Woodpeckers")

# 2) Labels & order for Conditions
condition_labels <- c(
  "Week1"      = "Early control",
  "Weeks15_17" = "Late control",
  "Food"       = "Food",
  "Decoy"      = "Decoy",
  "FoodDecoy"  = "Food + Decoy",
  "Playback"   = "Playback"
)
cond_levels <- names(condition_labels)

# 3) Plotting function for a group
plot_cond_group <- function(sp) {
  # filter data
  df <- if (sp == "All Woodpeckers") {
    Birds2_conditions %>% filter(Species %in% woodpeckers)
  } else {
    Birds2_conditions %>% filter(Species == sp)
  }
  
  # compute mean ± SE by Condition
  stats <- df %>%
    group_by(Condition) %>%
    summarise(
      mean_calls = mean(Count),
      se_calls   = sd(Count) / sqrt(n()),
      .groups    = "drop"
    ) %>%
    mutate(Condition = factor(Condition, levels = cond_levels))
  
  # pull significant Dunn pairs for this group
  sigs <- dunn_results_condition %>%
    filter(Species == sp, Dunn_p_adj < 0.05) %>%
    separate(Comparison, into = c("c1","c2"), sep = " - ") %>%
    mutate(
      c1 = factor(c1, levels = cond_levels),
      c2 = factor(c2, levels = cond_levels),
      x1 = as.numeric(c1),
      x2 = as.numeric(c2)
    )
  
  # base plot
  p <- ggplot(stats, aes(x = Condition, y = mean_calls)) +
    geom_col(fill = "#56B4E9", width = 0.7) +
    geom_errorbar(aes(ymin = mean_calls - se_calls,
                      ymax = mean_calls + se_calls),
                  width = 0.2) +
    scale_x_discrete(labels = condition_labels) +
    labs(
      title = sp,
      subtitle = "Mean calls ± SE by Condition",
      x = NULL,
      y = "Mean calls"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # add significance brackets
  if (nrow(sigs) > 0) {
    y_max  <- max(stats$mean_calls + stats$se_calls)
    step_h <- y_max * 0.1
    
    for (i in seq_len(nrow(sigs))) {
      x1 <- sigs$x1[i]
      x2 <- sigs$x2[i]
      pv <- sigs$Dunn_p_adj[i]
      y  <- y_max + i * step_h
      y0 <- y - 0.05 * y_max
      
      p <- p +
        annotate("segment", x=x1, xend=x2, y=y,  yend=y) +
        annotate("segment", x=x1, xend=x1, y=y,  yend=y0) +
        annotate("segment", x=x2, xend=x2, y=y,  yend=y0) +
        annotate("text",
                 x     = (x1+x2)/2,
                 y     = y + 0.02*y_max,
                 label = paste0("p=", signif(pv,2)),
                 size  = 3)
    }
  }
  
  print(p)
}

# 4) Generate plots
walk(plot_groups, plot_cond_group)
```

## 13.5 Condition Graphs with Error Bars part 2
```{r}
# 1) Compute mean ± SE for each Condition × Species (woodpeckers only)
summary_stats <- Birds2_conditions %>%
  filter(Species %in% woodpeckers) %>%
  group_by(Condition, Species) %>%
  summarise(
    mean_calls = mean(Count),
    se         = sd(Count) / sqrt(n()),
    .groups    = "drop"
  )

# 2) Precompute the max bar height per species (for bracket placement)
y_max_df <- summary_stats %>%
  group_by(Species) %>%
  summarise(y_max = max(mean_calls + se), .groups = "drop")

# 3) Which Conditions are your “treatments”?
treatments <- names(conditions)

# 4) Build a table of *significant* (p < .05) Dunn comparisons vs each baseline
stat_table <- dunn_results_condition %>%
  filter(Species %in% woodpeckers) %>%
  separate(Comparison, into = c("group1","group2"), sep = " - ") %>%
  filter(
    group1 %in% treatments,
    group2 %in% c("Week1", "Weeks15_17"),
    Dunn_p_adj < 0.05
  ) %>%
  left_join(y_max_df, by = "Species") %>%
  mutate(y.position = y_max + 5)  # bump bracket 5 units above tallest bar

# 5) Loop over each treatment and plot
for (cond in treatments) {
  df_plot <- summary_stats %>%
    filter(Condition %in% c(cond, "Week1", "Weeks15_17")) %>%
    mutate(
  Condition = recode(Condition,
    "Week1"      = "Early Control",
    "Weeks15_17" = "Late Controls"),
  Condition = factor(Condition, levels = c(cond, "Early Control", "Late Control")))
  
  sigs <- stat_table %>% filter(group1 == cond)
  
  p <- ggplot(df_plot, aes(x = Species, y = mean_calls, fill = Condition)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    geom_errorbar(aes(ymin = mean_calls - se, ymax = mean_calls + se),
                  position = position_dodge(0.8), width = 0.2)
  
  # only add brackets if there *are* significant comparisons
  if (nrow(sigs) > 0) {
    p <- p + stat_pvalue_manual(
      sigs,
      label      = "Dunn_p_adj",
      x          = "Species",
      y.position = "y.position",
      tip.length = 0.01
    )
  }
  
  p + scale_x_discrete(labels = function(x) woodpecker_abbr[x]) +
    labs(fill = NULL, title = cond) +
    theme_minimal() +
    theme(
      axis.title.x    = element_blank(),
      axis.title.y    = element_blank(),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 14),
      legend.position = "bottom"
    ) -> plot_obj
  
  print(plot_obj)
}
```




## 14. Weekly Woodpecker Graphs with Error Bars & Significance
```{r}
# Define control weeks
ctrl_weeks <- paste0("Week", c(15, 16, 17))

# 1. Prepare individual species data
df_indiv <- Birds2 %>%
  filter(Species %in% woodpeckers) %>%
  mutate(
    Week         = factor(paste0("Week", Week), levels = paste0("Week", 1:17)),
    Species_abbr = recode(Species, !!!woodpecker_abbr)
  ) %>%
  select(Week, local.date, Count, Species_abbr)

# 2. Prepare “All Woodpeckers” aggregated data
df_all <- Birds2 %>%
  filter(Species %in% woodpeckers) %>%
  mutate(
    Week = factor(paste0("Week", Week), levels = paste0("Week", 1:17))
  ) %>%
  group_by(Week, local.date) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(Species_abbr = "ALLWP")

# 3. Combine into one data frame
df_weekly <- bind_rows(df_indiv, df_all)

# 4. Plotting function
plot_week <- function(wk) {
  wk_lab <- paste0("Week", wk)
  comps  <- map(ctrl_weeks, ~ c(wk_lab, .x))
  
  df_weekly %>%
    filter(Week %in% c("Week1", wk_lab, ctrl_weeks)) %>%
    ggplot(aes(x = Species_abbr, y = Count, fill = Week)) +
      stat_summary(
        fun = mean, geom = "col",
        position = position_dodge(0.8)
      ) +
      stat_summary(
        fun.data = mean_se, geom = "errorbar",
        position = position_dodge(0.8), width = 0.2
      ) +
      stat_compare_means(
        comparisons     = comps,
        method          = "wilcox.test",
        p.adjust.method = "bonferroni",
        label           = "p.signif",
        position        = position_dodge(0.8)
      ) +
      labs(
        title = paste("Week", wk, "vs Week 1 & Weeks 15–17"),
        x     = NULL,
        y     = "Mean ± SE calls"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
}

# 5. Draw plots for weeks 1–14
walk(1:14, ~ print(plot_week(.x)))
```

## 15. Summer Distribution of Calls per Week
```{r}
# Compute avg daily woodpecker calls per week
avg_wp_week <- Birds2 %>%
  filter(Species %in% woodpeckers) %>%
  group_by(Week, local.date) %>%
  summarise(daily_calls = sum(Count), .groups = "drop") %>%
  group_by(Week) %>%
  summarise(avg_daily_calls = mean(daily_calls), .groups = "drop") %>%
  mutate(Week = as.integer(Week))

# Compute avg daily calls per week for all species
avg_all_week <- Birds2 %>%
  group_by(Week, local.date) %>%
  summarise(daily_calls = sum(Count), .groups = "drop") %>%
  group_by(Week) %>%
  summarise(avg_daily_calls = mean(daily_calls), .groups = "drop") %>%
  mutate(Week = as.integer(Week))

# 1) Woodpeckers
ggplot(avg_wp_week, aes(x = Week, y = avg_daily_calls)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(breaks = 1:17) +
  labs(
    title = "Avg Daily Woodpecker Calls per Week",
    x     = "Week",
    y     = "Avg calls per day"
  ) +
  theme_minimal()

# 2) All Species
ggplot(avg_all_week, aes(x = Week, y = avg_daily_calls)) +
  geom_col(fill = "darkgreen") +
  scale_x_continuous(breaks = 1:17) +
  labs(
    title = "Avg Daily All-Species Calls per Week",
    x     = "Week",
    y     = "Avg calls per day"
  ) +
  theme_minimal()
```

## 16. All Woodpecker Species Weekly Calls with Significance Brackets and Error Bars 
```{r}
plot_weekly_species <- function(sp) {
  # 1. Weekly mean ± SE
  df_stats <- Birds2 %>%
    filter(Species == sp) %>%
    mutate(Week = as.integer(as.character(Week))) %>%
    group_by(Week) %>%
    summarise(
      mean_calls = mean(Count, na.rm = TRUE),
      se_calls   = sd(Count,  na.rm = TRUE) / sqrt(n()),
      .groups    = "drop"
    )
  
  # 2. Significant Dunn pairs for this species
  df_sig <- dunn_results_weekly %>%
    filter(Species == sp, Dunn_p_adj < 0.05) %>%
    separate(Dunn_Comparison, into = c("w1","w2"), sep = " - ") %>%
    mutate(
      w1 = as.integer(w1),
      w2 = as.integer(w2)
    )
  
  # 3. Base bar + error‐bar plot
  p <- ggplot(df_stats, aes(x = Week, y = mean_calls)) +
    geom_col(fill = "steelblue") +
    geom_errorbar(aes(ymin = mean_calls - se_calls,
                      ymax = mean_calls + se_calls),
                  width = 0.2) +
    scale_x_continuous(breaks = 1:17) +
    labs(
      title = paste(sp, "— Mean Calls per Week"),
      x     = "Week",
      y     = "Mean calls (± SE)"
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # 4. Add significance brackets
  if (nrow(df_sig) > 0) {
    y_max  <- max(df_stats$mean_calls + df_stats$se_calls)
    step_h <- y_max * 0.1
    
    for (i in seq_len(nrow(df_sig))) {
      w1 <- df_sig$w1[i]
      w2 <- df_sig$w2[i]
      pval <- df_sig$Dunn_p_adj[i]
      y     <- y_max + (i * step_h)
      y0    <- y - 0.05 * y_max
      
      p <- p +
        annotate("segment", x = w1, xend = w2, y = y,  yend = y) +
        annotate("segment", x = w1, xend = w1, y = y,  yend = y0) +
        annotate("segment", x = w2, xend = w2, y = y,  yend = y0) +
        annotate("text",    x = (w1 + w2)/2, y = y + 0.02*y_max,
                 label = paste0("p=", signif(pval,2)), size = 3)
    }
  }
  
  print(p)
}

# 5. Produce one plot per woodpecker species
walk(woodpeckers, plot_weekly_species)
```

## 17. Weather + Woodpecker Calls, Daily Summaries
```{r}
# 1) Read & summarise weather for May–Aug
weather_files <- c(
  "Tuscaloosa.2024-05-01---2024-05-31.csv",
  "Tuscaloosa.2024-06-01---2024-06-30.csv",
  "Tuscaloosa.2024-07-01---2024-07-31.csv",
  "Tuscaloosa.2024-08-01---2024-08-31.csv"
)
weather_summary <- weather_files %>%
  map_df(read_csv) %>%
  mutate(
    datetime = ymd_hms(datetime, tz = "America/Chicago"),
    date     = as_date(datetime)
  ) %>%
  group_by(date) %>%
  summarise(
    mean_temp    = mean(temp,    na.rm = TRUE),
    max_temp     = max(temp,     na.rm = TRUE),
    max_humidity = max(humidity, na.rm = TRUE),
    total_precip = sum(precip,   na.rm = TRUE),
    .groups      = "drop"
  )

# 2) Summarise daily woodpecker calls
df_wp <- Birds2 %>%
  filter(Species %in% woodpeckers) %>%
  group_by(local.date) %>%
  summarise(
    wp_calls = sum(Count, na.rm = TRUE),
    .groups  = "drop"
  )

# 3) Join calls + weather
df_wp <- df_wp %>%
  left_join(weather_summary, by = c("local.date" = "date")) %>%
  arrange(local.date)
```

## 18. Weather Graph 
```{r}
# compute weekly summaries for woodpecker calls
weekly_wp <- df_wp %>%
  # use local.date here
  mutate(Week = define_weeks(local.date)) %>%
  filter(!is.na(Week)) %>%
  group_by(Week) %>%
  summarise(
    avg_wp_calls = mean(wp_calls,     na.rm = TRUE),
    avg_max_temp = mean(max_temp,     na.rm = TRUE),
    avg_precip   = mean(total_precip, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(Week = as.integer(Week)) %>%
  arrange(Week)


# scale factors
sf_wp_temp   <- max(weekly_wp$avg_wp_calls, na.rm=TRUE) /
                max(weekly_wp$avg_max_temp, na.rm=TRUE)
sf_wp_precip <- max(weekly_wp$avg_wp_calls, na.rm=TRUE) /
                max(weekly_wp$avg_precip,     na.rm=TRUE)

ggplot(weekly_wp, aes(x = Week)) +
  geom_col(aes(y = avg_wp_calls, fill = "Woodpecker calls"), width = 0.8) +
  geom_line(aes(y = avg_max_temp * sf_wp_temp, color = "Max temp"), size = 1) +
  geom_line(aes(y = avg_precip   * sf_wp_precip, color = "Precipitation"),
            size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = 1:17) +
  scale_y_continuous(
    name     = "Avg woodpecker calls per day",
    sec.axis = sec_axis(~ . / sf_wp_temp,
                        name = "Avg max temp (°F)")
  ) +
  scale_fill_manual(values = c("Woodpecker calls" = "steelblue")) +
  scale_color_manual(values = c("Max temp" = "firebrick", "Precipitation" = "darkgreen")) +
  labs(
    x     = "Weeks",
    color = NULL,
    fill  = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_blank(),
    axis.title.x = element_text(size = 14),
    legend.position = "bottom",
    legend.text     = element_text(size = 12))
```

## 19. URCA Graph with Averages. 
```{r}
## 6. Average woodpecker calls per day by condition
# ── 1. Mark experimental conditions ───────────────────────────────────────────
Birds2 <- Birds2 %>%
  mutate(
    Condition = case_when(
      Week %in% controls$Week1 ~ "Early Control",
      Week %in% controls$Weeks15_17 ~ "Late Control",
      Week %in% as.character(conditions$Food) ~ "Food Only",
      Week %in% as.character(conditions$Decoy) ~ "Decoy Only",
      Week %in% as.character(conditions$FoodDecoy) ~ "Food and Decoy",
      Week %in% as.character(conditions$Playback) ~ "Playback",
      TRUE ~ "Other"
    )
  )

# ── 2. Summarise average calls ────────────────────────────────────────────────
Birds_Woodpeckers_Average <- Birds2 %>%
  filter(Species %in% woodpeckers, Condition != "Other") %>%
  group_by(Condition, Week) %>%
  summarise(Average_Calls = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Condition) %>%
  summarise(Average_Calls = mean(Average_Calls, na.rm = TRUE), .groups = "drop")

# ── 3. Order conditions ──────────────────────────────────────────────────────
condition_order <- c("Early Control", "Food Only", "Decoy Only",
                     "Food and Decoy", "Playback", "Late Control")

Birds_Woodpeckers_Average <- Birds_Woodpeckers_Average %>%
  mutate(Condition = factor(Condition, levels = condition_order))

# ── 4. Colors ────────────────────────────────────────────────────────────────
custom_colors <- c(
  "Early Control" = "#00A600",   # green
  "Food Only"     = "#63C600",   # lighter green
  "Decoy Only"    = "#E6E600",   # yellow
  "Food and Decoy"= "#EAB64E",   # orange
  "Playback"      = "#EEB99F",   # pink
  "Late Control"  = "#00A600"    # same green as Early Control
)

# ── 5. Plot ───────────────────────────────────────────────────────────────────
ggplot(Birds_Woodpeckers_Average,
       aes(x = Condition, y = Average_Calls, fill = Condition)) +
  geom_bar(stat = "identity", color = "black", width = 0.75) +
  labs(
    title = " ",
    y     = "Average calls per day"
  ) +
  theme_minimal() +
  theme(
    axis.text.x      = element_text(size = 9, angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x      = element_line(color = "black"),
    axis.line.y      = element_line(color = "black"),
    axis.title.x     = element_blank(),
    legend.position  = "none"
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, NA))
```

## 20. Big URCA Graph with Colors. 
```{r}
# ── 1. Summarise ──────────────────────────────────────────────────────────────
Birds_line <- Birds2 %>%
  filter(Species %in% woodpeckers, Week != "No Week") %>%
  group_by(Species, Week) %>%
  summarise(Average_Calls_Per_Day = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  mutate(Week = factor(Week, levels = as.character(1:17)))

# ── 2. Plot ───────────────────────────────────────────────────────────────────
ggplot(Birds_line,
       aes(x = Week, y = Average_Calls_Per_Day,
           color = Species, shape = Species, group = Species)) +
  # Background rectangles for conditions (using custom colors)
  geom_rect(aes(xmin = 0.5, xmax = 2, ymin = -Inf, ymax = Inf),
            fill = custom_colors["Early Control"], alpha = 0.25, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 2, xmax = 6, ymin = -Inf, ymax = Inf),
            fill = custom_colors["Food Only"], alpha = 0.25, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 6, xmax = 9, ymin = -Inf, ymax = Inf),
            fill = custom_colors["Decoy Only"], alpha = 0.25, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 9, xmax = 11, ymin = -Inf, ymax = Inf),
            fill = custom_colors["Food and Decoy"], alpha = 0.25, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 11, xmax = 15, ymin = -Inf, ymax = Inf),
            fill = custom_colors["Playback"], alpha = 0.25, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 15, xmax = 17.5, ymin = -Inf, ymax = Inf),
            fill = custom_colors["Late Control"], alpha = 0.25, inherit.aes = FALSE) +
  # Lines + points
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    x = "Week",
    y = "Average calls per day",
    title = "Average woodpecker calls per week"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(size = 12),
    axis.text.y     = element_text(size = 12),
    legend.title    = element_blank(),
    panel.grid.major= element_line(color = "white"),
    panel.grid.minor= element_blank(),
    legend.position = "bottom",
    legend.margin   = margin(-10, 0, 0, 0)
  ) +
  scale_color_grey(start = 0, end = 0.8,
                   guide = guide_legend(nrow = 3, byrow = TRUE)) +
  scale_shape_manual(values = c(16, 17, 18, 15, 8, 3)) +
  coord_fixed(ratio = 0.4)
```












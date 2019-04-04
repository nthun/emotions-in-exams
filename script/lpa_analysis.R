
# WARNING: This code is outdated ----------------------------------------------------


library(tidyverse)
install.packages("tidyLPA")
library(tidyLPA)
theme_set(theme_light())

# Before emotions
# Using arousal and the 20 distinct emotions
df %>% 
    distinct(id, .keep_all = TRUE) %>% 
    # Fill NAs with 0s
    mutate_at(vars(before_anger:before_interest), ~if_else(is.na(.), 0, .)) %>%
    estimate_profiles(before_arousal, before_anger:before_interest) %>% 
    compare_solutions()

before_prof <- 
    df %>% 
    distinct(id, .keep_all = TRUE) %>% 
    # Fill NAs with 0s
    mutate_at(vars(before_anger:before_interest), ~if_else(is.na(.), 0, .)) %>%
    select(before_arousal, before_anger:before_interest) %>%
    drop_na() %>% 
    estimate_profiles(n_profiles = 1:10)

before_prof %>% 
    compare_solutions()

before_prof_8 <- 
    before_prof[[8]] %>% 
    .[["dff"]]


before_prof <-
    df %>% 
    distinct(id, .keep_all = TRUE) %>%
    mutate_at(vars(before_anger:before_interest), ~if_else(is.na(.), 0, .)) %>%
    select(before_arousal, before_anger:before_interest) %>%
    estimate_profiles(n_profiles = 7, 
                      variances = "equal",
                      covariances = "equal",
                      return_orig_df = TRUE)


before_prof <- 
    df %>% 
    mutate(profile = sample(1:8, nrow(.), replace = TRUE))

before_prof %>% 
    group_by(profile) %>% 
    summarise(n = n(),
              mean_prob = mean(posterior_prob))

before_prof_gathered <-    
    before_prof %>% 
    select(id, gender, age, starts_with("before"), profile, grade) %>% 
    gather(emotion, intensity, c(before_arousal, before_anger:before_interest)) %>% 
    mutate(emotion = str_remove(emotion, "before_")) %>% 
    full_join(emotion_df, by = "emotion")

# Emotion profile
before_prof_gathered %>% 
    group_by(profile, emotion, emotion_direction) %>% 
    summarise(intensity_mean = mean(intensity, na.rm = TRUE),
              intensity_se = sd(intensity, na.rm = TRUE)/sqrt(n())) %>% 
    ggplot() +
    aes(x = fct_reorder(emotion, intensity_mean), y = intensity_mean, 
        ymin = intensity_mean - intensity_se, ymax = intensity_mean + intensity_se,
        fill = emotion_direction) +
    geom_point() +
    geom_errorbar() +
    coord_flip() +
    facet_wrap(~profile)


before_prof_gathered %>% 
    group_by(profile, emotion, emotion_direction) %>% 
    summarise(intensity = mean(intensity)) %>% 
    ggplot() +
    aes(x = fct_reorder(emotion, intensity), y = intensity, fill = emotion_direction) +
    geom_col() +
    coord_flip() +
    facet_wrap(~profile)


# Performance profile
before_prof_gathered %>% 
    distinct(id, .keep_all = TRUE) %>% 
    group_by(profile) %>% 
    summarise(mean_after_perf = mean(grade, na.rm = TRUE),
              se_after_perf = sd(grade, na.rm = TRUE) / sqrt(n()),
              N = n()) %>% 
    ggplot() +
    aes(x = profile, y = mean_after_perf, 
        ymin = mean_after_perf - se_after_perf,
        ymax = mean_after_perf + se_after_perf,
        label = str_glue("N = {N}")) +
    geom_col() +
    coord_cartesian(ylim = c(1,5)) +
    geom_errorbar(width = .5) +
    geom_text(aes(y = 5))


# Demographics profile
before_prof_gathered %>% 
    distinct(neptun_id, .keep_all = TRUE) %>% 
    count(profile, gender) %>% 
    ggplot() +
    aes(x = profile, y = n, fill = gender,
        label = str_glue("N = {n}")) + 
    geom_col(position = "stack") +
    scale_y_continuous() +
    geom_text(aes(y = n))


before_prof_gathered %>% 
    distinct(neptun_id, .keep_all = TRUE) %>% 
    count(profile, age) %>% 
    ggplot() +
    aes(x = profile, y = n, fill = fct_rev(factor(age)),
        label = str_glue("N = {n}")) + 
    geom_col(position = "fill") +
    scale_fill_viridis_d("Age", direction = -1, option = "inferno") +
    scale_y_continuous(labels = scales::percent_format())
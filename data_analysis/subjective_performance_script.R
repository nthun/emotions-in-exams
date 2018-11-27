library(tidyverse)
library(googlesheets)
library(lme4)
library(lmerTest)
library(skimr)

theme_set(theme_minimal())

positive <- c("compassion", "relief", "admiration", "love", "contentment", "pleasure", "joy", "pride", "amusement", "interest")

negative <- c("anger", "hate", "contempt", "disgust", "fear", "disappointment", "shame", "regret", "guilt", "sadness")

emotion_df <-
    data_frame( emotion = positive,
                emotion_direction = "positive") %>% 
    bind_rows(
        data_frame(emotion = negative,
                   emotion_direction = "negative"
        )
    )

hu_courses <-
    gs_title("2018-2019_hungarian_courses") %>% 
    gs_read(1)

en_courses <-
    gs_title("2018-2019_english_courses") %>% 
    gs_read(1)

raw_df <-
    gs_title("data_collection") %>% 
    gs_read(1)

courses <- 
    bind_rows(hu_courses,
              en_courses)

df <-
    raw_df %>% 
    left_join(courses, by = "course_name") %>% 
    mutate(before_positive = rowMeans(select(., one_of(str_glue("before_{positive}"))), na.rm = TRUE),
           before_negative = rowMeans(select(., one_of(str_glue("before_{negative}"))), na.rm = TRUE),
           after_positive = rowMeans(select(., one_of(str_glue("after_{positive}"))), na.rm = TRUE),
           after_negative = rowMeans(select(., one_of(str_glue("after_{negative}"))), na.rm = TRUE)
           )

perf_lm <- 
    lm(after_subjective_performance ~ before_positive * before_arousal + gender + age + course_level, data = df)               

summary(perf_lm) 

perf_lm_i <- 
    lmer(after_subjective_performance ~ before_positive + before_arousal + gender + age + course_level + (1|course_name), data = df)

summary(perf_lm_i)
    
ggplot(df) + 
    aes(x = before_positive, 
        y = after_subjective_performance,
        color = before_arousal) +
    geom_jitter(width = 0, height = .1)

df %>% 
    select(before_positive:after_negative) %>% 
    gather(variable, value) %>% 
    ggplot() +
    aes(x = value) +
    geom_histogram() +
    facet_wrap(~variable)

count(df, course_name, gender)

emotion_data <- 
    df %>% 
    select(neptun_id,
           one_of(str_glue("before_{positive}")),
           one_of(str_glue("after_{positive}")),
           one_of(str_glue("before_{negative}")),
           one_of(str_glue("after_{negative}"))) %>% 
    gather(when_emotion, intensity, -neptun_id) %>% 
    drop_na() %>% 
    separate(when_emotion, c("when","emotion")) %>% 
    mutate(when = factor(when, c("before", "after"))) %>% 
    left_join(emotion_df, by = "emotion")

# What emotions did participant feel before and after the exam
emotion_data %>% 
    count(when, emotion, emotion_direction, sort = TRUE) %>% 
    ggplot() +
    aes(x = fct_reorder(emotion, n), 
        y = n) +
    geom_col() +
    facet_grid(emotion_direction ~ when, scales = "free_y") +
    coord_flip() +
    labs(title = "What emotions did participant feel before and after the exam",
         x = NULL,
         y = "# of participants feeling the emotion")

# Intensity of emotions before and after the exam
emotion_data %>% 
    group_by(when, emotion, emotion_direction) %>% 
    summarise(n = sum(intensity, na.rm = TRUE)) %>% 
    ggplot() +
    aes(x = fct_reorder(emotion, n), 
        y = n) +
    geom_col() +
    facet_grid(emotion_direction ~ when, scales = "free_y") +
    coord_flip() +
    labs(title = "Intensity of emotions before and after the exam",
         x = NULL,
         y = "Summarised intensity specific emotions")
    

df %>% 
    select(one_of(str_glue("before_{positive}")),
           one_of(str_glue("after_{positive}")),
           one_of(str_glue("before_{negative}")),
           one_of(str_glue("after_{negative}"))
           ) %>% 
    mutate()

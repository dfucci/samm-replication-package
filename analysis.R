options(warn = -1)
library(readxl)
library(tidyverse)
library(magrittr)
library(fmsb)
library(HH)

SAMM_answers <- read_csv("results/dataset/answers.csv")
workshop_path <- "workshop"
figure_path <- "results/figure"
dataset_path <- "results/dataset"
role_labels <-
  c("Architects",
    "Developers",
    "Management",
    "Operations",
    "Verification")

fix_rating <- function(df) {
  df %<>% mutate(rating = ifelse(
    as.numeric(rating) > 3,
    as.numeric(rating / 1000),
    as.numeric(rating)
  )) # fixing quirkiness with double in Excel
  return(df)
}
SAMM_answers <- fix_rating(SAMM_answers)

add_pratices_to_dataframe <- function(df) {
  SAMM_area_labels <-
    c(
      "Strategy&Metrics",
      "Policy&Compliance",
      "Education&Guidance",
      "Threat Assessment",
      "Security Requirements",
      "Secure Architecture",
      "Secure Build",
      "Secure Deployment",
      "Defect Management",
      "Architecture Assessment",
      "Requirements Testing",
      "Security Testing",
      "Incident Management",
      "Environment Management",
      "Operational Management"
    )
  
  respondents <-
    length(unique(df$respondent))
  questions_per_area <- 6
  SAMM_areas <- length(SAMM_area_labels)
  
  data_with_practices <- df %>%
    mutate(
      area = gl(
        SAMM_areas,
        questions_per_area,
        SAMM_areas * questions_per_area * respondents,
        SAMM_area_labels
      )
    )
  return(data_with_practices)
}
prepare_spiderchart_all <- function(df) {
  df %<>%
    group_by(area) %>%
    summarise(avg_rate_area = mean(rating)) %>%
    mutate_if(is.numeric, round, 2) %>%
    spread(key = area, value = avg_rate_area)
  return(df)
}
prepare_spiderchart_roles <- function(df) {
  df %<>%
    group_by(role, area) %>%
    summarise(avg_rating_role = mean(rating)) %>%
    spread(key = area, value = avg_rating_role) %>%
    mutate_if(is.numeric, round, 2) %>%  # rounding otherwise spider chart may be weird
    ungroup() %>%
    dplyr::select(-role) %>%  # discard roles as the radarchart function does not like factors
    mutate(across(everything(), ~ replace_na(.x, 0)))
  return(df)
}
spider_chart <- function(df, filename, weighted = F) {
  # preparing data for plotting
  df$answer_avg <-
    as.numeric(sub(",", ".", df$answer_avg, fixed = TRUE))
  n_respondents <- length(unique(df$respondent))
  if (weighted) {
    df %<>% mutate(answer_weighted =
                     if_else(
                       confidence == "very unconfident",
                       answer_avg * 0.2,
                       if_else(
                         confidence == "unconfident",
                         answer_avg * 0.5,
                         if_else(confidence == "confident", answer_avg * 0.75, answer_avg)
                       )
                     )) %>%
      group_by(role, respondent, area) %>%
      summarise(rating_weighted = sum(answer_weighted, na.rm = T)) %>%
      ungroup() %>%
      group_by(role, area) %>%
      summarise(average_rate = mean(rating_weighted)) %>%
      spread(key = area, value = average_rate) %>%
      mutate_if(is.numeric, round, 2) %>%
      ungroup() %>%
      dplyr::select(-role)
  }
  else{
    df <- prepare_spiderchart_roles(df)
  }
  df <-
    rbind(rep(0, 15), df) # radarchart function expects top row with MAX (3)and second row with MIN (0)
  df <- rbind(rep(3, 15), df)
  # plotting
  par(or)
  op <- par(mar = c(1, 2, 2, 2)) # using more screen estate to plot
  png(
    filename = glue::glue("{figure_path}/{filename}.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
  )
  
  p <- df %>%
    radarchart(
      seg = 3,
      axistype = 0,
      pty = 32,
      plty = rep(1, 5),
      vlcex = 0.5
    )
  legend(
    "topright",
    legend = role_labels,
    bty = "n",
    pch = 20,
    text.col = "black",
    cex = 0.5,
    pt.cex = 1.5,
    col = c(1:8)
  )
  title <- if_else(grepl("_", filename),
                   toString(glue::glue("Spiderchart for confident answers.")),
                   "Spiderchart for all answers.")
  sub <-
    if_else(weighted, "Weighted (20%, 50%, 75%, 100%)", "Unweighted")
  title(main = glue::glue("{title} N={n_respondents}"),
        sub = sub)
  
  dev.off()
  
}
spider_chart_all <- function(df, filename) {
  df <- prepare_spiderchart_all(df)
  df <- rbind(rep(3, 15), rep(0, 15), df)
  par(or)
  op <- par(mar = c(1, 2, 2, 2)) # using more screen estate to plot
  png(
    filename = glue::glue("{figure_path}/{filename}.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
  )
  
  p <- df %>% radarchart(
    seg = 3,
    axistype = 0,
    pty = 32,
    vlcex = 0.6
  )
  title(main = "Spiderchart for all answers to SAMM across roles")
  dev.off()
}
spiderchart_roles_all <- function(df) {
  df_roles <- prepare_spiderchart_roles(df)
  df_all <- prepare_spiderchart_all(df)
  df <- rbind(rep(3, 15), rep(0, 15), df_all, df_roles)
  par(or)
  op <- par(mar = c(1, 2, 2, 2)) # using more screen estate to plot
  png(
    filename = glue::glue("{figure_path}/spiderchart_all+roles.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
  )
  
  p <- df %>% radarchart(
    seg = 3,
    axistype = 0,
    pty = 32,
    plty = 1,
    vlcex = 0.6,
    pfcol = c(
      scales::alpha("black", 0.2), 
      scales::alpha("green", 0.0), 
      scales::alpha("yellow", 0.0), 
      scales::alpha("red", 0.0), 
      scales::alpha("magenta", 0.0), 
      scales::alpha("gray", 0.0) 
    )
  )
  legend(
    "topright",
    legend = c("All", role_labels),
    bty = "n",
    pch = 20,
    text.col = "black",
    cex = 0.5,
    pt.cex = 1.5,
    col = c(1:8)
  )
  title(main = "Spiderchart for all answers to SAMM (roles and overall)")
  dev.off()
}


spiderchart_all_confident <- function(df) {
  df_all <- prepare_spiderchart_all(df)
  df_confident <- data %>% filter(confidence == "confident" | confidence == "very confident")
  df_unconfident <- data %>% filter(confidence == "unconfident" | confidence == "very unconfident")
  df_confident <- prepare_spiderchart_all(df_confident)
  df_unconfident <- prepare_spiderchart_all(df_unconfident)
  df <- rbind(rep(3, 15), rep(0, 15), df_confident, df_unconfident, df_all)
  par(or)
  op <- par(mar = c(1, 2, 2, 2)) # using more screen estate to plot
  png(
    filename = glue::glue("{figure_path}/spiderchart_all_confident.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
  )
  
  p <- df %>% radarchart(
    seg = 3,
    axistype = 0,
    pty = 32,
    plwd = 0.5,
    plty = 1,
    vlcex = 0.6,
    pcol = c("navy", "pink", "yellow"),
    pfcol = c(
      scales::alpha("navy", 0.7),
      scales::alpha("pink", 0.7),
      scales::alpha("yellow", 0.4)
    )
  )
  legend(
    "bottom",
    legend = c("Confident", "Unconfindent", "All"),
    bty = "n",
    pch = 20,
    text.col = "black",
    cex = 0.5,
    horiz = T,
    pt.cex = 1.5,
    col = c("navy", "pink", "yellow")
  )
  title(main = "Spiderchart for Survey and Confident")
  dev.off()
}

spiderchart_all_easiness <- function(df) {
  df_all <- prepare_spiderchart_all(df)
  df_easiness <- data %>% filter(difficulty == "easy" | difficulty == "very easy")
  df_difficult <- data %>% filter(difficulty == "difficult" | difficulty == "very difficult")
  df_easiness <- prepare_spiderchart_all(df_easiness)
  df_difficult <- prepare_spiderchart_all(df_difficult)
  df <- rbind(rep(3, 15), rep(0, 15),  df_easiness, df_difficult, df_all)
  par(or)
  op <- par(mar = c(1, 2, 2, 2)) # using more screen estate to plot
  png(
    filename = glue::glue("{figure_path}/spiderchart_all_easiness.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
  )
  
  p <- df %>% radarchart(
    seg = 3,
    axistype = 0,
    pty = 32,
    plwd = 0.5,
    plty = 1,
    vlcex = 0.6,
    pcol = c("navy", "pink", "yellow"),
    pfcol = c(
      scales::alpha("navy", 0.7),
      scales::alpha("pink", 0.7),
      scales::alpha("yellow", 0.4)
    )
  )
  legend(
    "bottom",
    legend = c("Easy", "Difficult", "All"),
    bty = "n",
    pch = 20,
    text.col = "black",
    cex = 0.5,
    horiz = T,
    pt.cex = 1.5,
    col = c("navy", "pink", "yellow")
  )
  title(main = "Spiderchart for Survey and Easiness")
  dev.off()
}


spiderchart_workshop <- function(df) {
  df_all <- prepare_spiderchart_all(df)
  df_workshop <-
    read.csv2("clean/workshop/SAMM.csv", sep = ",") %>% dplyr::select(-X)
  df_workshop <- add_pratices_to_dataframe(df_workshop)
  df_workshop <- fix_rating(df_workshop)
  df_workshop <- prepare_spiderchart_all(df_workshop)
  df <- rbind(rep(3, 15), rep(0, 15), df_all, df_workshop)
  par(or)
  op <- par(mar = c(1, 2, 2, 2)) # using more screen estate to plot
  png(
    filename = glue::glue("{figure_path}/spiderchart_workshop.png"),
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4
  )
  
  p <- df %>% radarchart(
    seg = 3,
    axistype = 0,
    pty = 32,
    plwd = 0.5,
    plty = 1,
    vlcex = 0.6,
    pfcol = c(
      scales::alpha("#00AFBB", 0.7),
      scales::alpha("#E7B800", 0.7)
    )
  )
  legend(
    "bottom",
    legend = c("Survey", "Focus group"),
    bty = "n",
    pch = 20,
    text.col = "black",
    cex = 0.5,
    horiz = T,
    pt.cex = 1.5,
    col = c("#00AFBB", "#E7B800")
  )
  title(main = "Spiderchart for Survey and Workshop")
  dev.off()
}
answer_confidence <- function(df) {
  n_respondents <- length(unique(df$respondent))
  par(or)
  op <- par(mar = c(1, 2, 2, 2)) # using more screen estate to plot
  df$confidence <-
    fct_relevel(
      as_factor(df$confidence),
      "very unconfident",
      "unconfident",
      "confident",
      "very confident"
    )
  # df %<>% filter(!is.na(confidence)) 
  p <- ggplot(df, aes(confidence, fill = role)) +
    geom_bar() +
    facet_wrap( ~ area, nrow = 5) +
    theme(
      axis.text.x = element_text(
        angle = 55,
        hjust = 1,
        size = 6
      ),
      strip.text.x = element_text(size = 5.5),
      axis.text.y = element_text(size = 5.5),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 4.5),
      legend.key.height = unit(.2, 'cm')
    )
  ggtitle(glue::glue("Answers confidence according to roles. N = {n_respondents}"))
  ggsave(
    glue::glue("{figure_path}/confidence.png"),
    height = 5,
    width = 7,
    units = "in"
  )
  dev.off()
}
answer_easiness <- function(df) {
  n_respondents <- length(unique(df$respondent))
  par(or)
  op <- par(mar = c(1, 2, 2, 2)) # using more screen estate to plot
  df$difficulty <-
    fct_relevel(
      as_factor(df$difficulty),
      "very difficult",
      "difficult",
      "easy",
      "very easy"
    )
  # df %<>% filter(!is.na(difficulty))
  p <- ggplot(df, aes(difficulty, fill = role)) +
    geom_bar() +
    facet_wrap( ~ area, nrow = 5) +
    xlab("easiness") + 
    theme(
      axis.text.x = element_text(angle = 55, hjust = 1, size = 6),
      strip.text.x = element_text(size = 5.5),
      axis.text.y = element_text(size = 5.5),
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 4.5),
      legend.key.height = unit(.2, 'cm')
    )
  ggtitle(glue::glue("Answers easiness according to roles. N = {n_respondents}"))
  ggsave(
    glue::glue("{figure_path}/easiness.png"),
    height = 5,
    width = 7,
    units = "in"
  )
  dev.off()
}

prepare_data_likert <- function(data) {
  dfl <- data %>%
    count(role, confidence) %>%
    spread(confidence, n, fill = 0) %>%
    relocate(`very confident`, .before = confident) %>%
    rename("No answer" = `<NA>`)
  return(dfl)
}
likert_confidence <- function(data) {
  png(
    file = glue::glue("{figure_path}/general_confidence.png"),
    width = 6.5,
    height = 4,
    res = 1200,
    units = "in",
    pointsize = 150
  )
  p <-
    likert(
      role ~ .,
      data,
      as.percent = T,
      positive.order = T,
      main = "Perceived confidence"
    )
  dev.off()
}

# Plotting the full data
data <- add_pratices_to_dataframe(SAMM_answers)
spider_chart(data, "spiderchart")
spider_chart_all(data, "spiderchart_all_answers")
spiderchart_roles_all(data)
spiderchart_workshop(data)
answer_confidence(data)
answer_easiness(data)

# Plotting only high-confidence answers
data.confident <- data %>% filter(confidence == "confident" | confidence=="very confident")
spider_chart(data.confident, "spiderchart_confident")
spider_chart(data, "spiderchart_weighted", T)

# Plotting likert
df.likert <- prepare_data_likert(data)
likert_confidence(df.likert)

# spiderchart_all_confident
spiderchart_all_confident(data)

spiderchart_all_easiness(data)
# cleanup ggsave crap
file.remove("Rplots.pdf")

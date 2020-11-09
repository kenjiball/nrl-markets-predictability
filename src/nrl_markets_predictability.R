#' NRL Market Predictability
#' 
#' @author Kenji Ball - October 2020
#' 
#' This script is used to test the hypothesis:
#'   "Was the 2020 NRL season the most predictable season in recent history?"
#' 
#' With data to other league it can be extended to test similar hypthosis on other leagues


# Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(implied
              , janitor
              , lubridate
              , poibin
              , readxl
              , tidyverse
              , zoo)

# Download the NRL Odds data from the following link:
# The spreadhsheet shoyld be saved in the data directory
# and named 'nrl_odds.xlsx'
# http://www.aussportsbetting.com/data/ 

# Import from project directory csv
nrl_odds_data <- read_excel(path = "./data/nrl_odds.xlsx", sheet = 1, skip = 1 )

# Data Preparation
nrl_odds_df <- nrl_odds_data %>% 
  # Rename and clean up column names
  rename(kick_off_time = `Kick-off (local)`) %>% 
  clean_names() %>% 
  mutate(
    # Fix up date formats
    kick_off_time = as.character(gsub(".* ","",kick_off_time)),
    date = ymd(date),
    season = year(date),
    # Add in win/loss flags
    home_win = case_when(home_score > away_score ~ 1,
                         home_score < away_score ~ 0,
                         home_score == away_score ~ 0.5,
                         TRUE ~ NA_real_),
    away_win = case_when(away_score > home_score ~ 1,
                         away_score < home_score ~ 0,
                         away_score == home_score ~ 0.5,
                         TRUE ~ NA_real_),
    home_fav = if_else(home_odds_close <= away_odds_close, TRUE, FALSE),
    away_fav = if_else(home_odds_close > away_odds_close, TRUE, FALSE),
    fav_team = if_else(home_odds_close <= away_odds_close, "home", "away"),
    fav_odds = pmin(home_odds_close, away_odds_close, na.rm = TRUE),
    fav_win = case_when(fav_team == "home" ~ home_win,
                        fav_team == "away" ~ away_win,
                        TRUE ~ NA_real_),
    underdog_odds = pmax(home_odds_close, away_odds_close, na.rm = TRUE),
    underdog_win = 1 - fav_win,
    abs_margin = abs(home_score - away_score)
  )

# Basic Summary statistics
nrl_summary_df <- nrl_odds_df %>%
  group_by(season) %>% 
  summarise(games = n(),
            home_fav = sum(home_fav, na.rm = TRUE),
            away_fav = sum(away_fav, na.rm = TRUE),
            home_win = sum(home_win, na.rm = TRUE),
            away_win = sum(away_win, na.rm = TRUE),
            fav_win = sum(fav_win, na.rm = TRUE),
            fav_win_rate = fav_win/games,
            mean_abs_close_line = mean(abs(home_line_close), na.rm = TRUE ),
            sd_abs_close_line = sd(abs(home_line_close), na.rm = TRUE ),
            mean_abs_margin = mean(abs_margin, na.rm = TRUE ),
            sd_abs_margin = sd(abs_margin, na.rm = TRUE )
  ) 

# Quick plot of line vs Fav win rate
nrl_summary_df %>%
  filter(!is.na(mean_abs_close_line)) %>% 
  ggplot(aes(x = fav_win_rate, y = mean_abs_close_line, label = season)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  geom_label() +
  labs(x = "Favourite Win Rate",
       y = "Mean Abs. Closing Line",
       title = "Favourite Win Rate Vs. Mean Absolute Closing Line",
       subtitle = "NRL Seasons 2013-2020",
       caption = "Data Sourced from: http://www.aussportsbetting.com/") 


# Quick plot of Mean Line vs. Mean Margin
nrl_summary_df %>%
  filter(!is.na(mean_abs_close_line)) %>% 
  ggplot(aes(x = mean_abs_margin, y = mean_abs_close_line, label = season)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  geom_label() +
  labs(y = "Mean Abs. Closing Line",
       x = "Mean Abs. Margins",
       title = "Mean Abs. Margin Vs. Mean Abs. Closing Line",
       subtitle = "NRL Seasons 2013-2020",
       caption = "Data Sourced from: http://www.aussportsbetting.com/") 


# Quick plot of SD Line vs. SD Margin
nrl_summary_df %>%
  filter(!is.na(sd_abs_close_line)) %>% 
  ggplot(aes(x = sd_abs_close_line, y = sd_abs_margin, label = season)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  geom_label() +
  labs(x = "SD Abs. Closing Line",
       y = "SD Abs. Margins",
       title = "SD Abs. Closing Line Vs. SD Abs. Margin",
       subtitle = "NRL Seasons 2013-2020",
       caption = "Data Sourced from: http://www.aussportsbetting.com/")



# Lets use log loss to score the closing odds
# We use log loss as it a way to classify the error in an event with a binary outcome

# Convert odds using implied package 
# https://rdrr.io/github/opisthokonta/implied/f/README.md

# Prepare Data Frame
odds_df <- nrl_odds_df %>% 
  select(home_odds_close, away_odds_close) %>% 
  filter(!is.na(home_odds_close) & !is.na(away_odds_close))

# Convert the odds using the power method
odds_converted_df <- implied_probabilities(odds = odds_df, method = 'power' )

# Append results back to main nrl_odds_df data frame
odds_results_df <- nrl_odds_df %>% 
  filter(!is.na(home_odds_close) & !is.na(away_odds_close)) %>% 
  mutate( home_prob_close = odds_converted_df$probabilities[,1],
          away_prob_close = odds_converted_df$probabilities[,2])

# Calculate the log loss of each match, then summarise over season
nrl_logloss_df <- odds_results_df %>%
  mutate(
    y_true = home_win,
    y_pred = home_prob_close,
    log_loss = y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)
  ) %>% 
  group_by(season) %>% 
  summarise(
    mean_log_loss = -mean(log_loss, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_log_loss)) %>% 
  ungroup() 


# Plot the mean log loss results
nrl_logloss_df %>% 
  ggplot(aes(x = reorder(season,-mean_log_loss), y = mean_log_loss)) +
  geom_col(fill = measurem_cols("blue")) +
  coord_flip() + 
  geom_label(aes(label = season)) +
  labs(x = "Season",
       y = "Mean Log Loss",
       title = "Mean Log Loss by Season - NRL 2013-2020",
       subtitle = "Smaller log loss represents a more predictable season based on closing head to head match odds",
       caption = "Data Sourced from: http://www.aussportsbetting.com/") +
  theme_measurem +
  theme(axis.text.y = element_blank())

# Top 5 Upsets from 2020
odds_results_df %>%
  filter(season == 2020) %>% 
  mutate(
    y_true = home_win,
    y_pred = home_prob_close,
    log_loss = y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)
  )  %>% 
  arrange(log_loss) %>% 
  top_n(n=5, wt = -log_loss) %>% 
  select(home_team, away_team, home_score, away_score, home_odds_close, away_odds_close, home_prob_close, away_prob_close, log_loss) %>% View(.)


# Betting the Favs vs. Dogs in 2020
betting_df <- odds_results_df %>%
  filter(season == 2020) %>% 
  mutate( stake = 100,
          Fav_Rev = case_when(fav_win == 1 ~ (fav_odds-1)*stake,
                              fav_win == 0.5 ~ 0,
                              fav_win == 0 ~ -stake,
                              TRUE ~ 0),
          Dog_Rev = case_when(underdog_win == 1 ~ (underdog_odds -1)*stake,
                              underdog_win == 0.5 ~ 0,
                              underdog_win == 0 ~ -stake,
                              TRUE ~ 0)
  )

betting_df %>% 
  arrange(date) %>% 
  ggplot(aes(x = date , y = cumsum(Fav_Rev))) +
  geom_line()

# Correlation Plot of Observed Probability vs. Betting Market Estimate
odds_results_df %>% 
  select(season, home_win ,home_prob_close) %>% 
  mutate( home_prob_close_bands = cut(home_prob_close, breaks = (0:10)*0.1 ) ) %>% 
  group_by(season, home_prob_close_bands) %>% 
  summarise(home_win_observed = sum(home_win, na.rm = TRUE)/n(),
            count = n()) %>% 
  filter( count >= 5) %>% 
  ggplot( aes(x = home_prob_close_bands, y = home_win_observed, size = count, alpha = 0.5)) +
  geom_point() +
  facet_wrap(season~.) +
  geom_abline(aes(intercept = 0, slope = 1/9, colour = 'red')) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme_measurem
  

# Poisson Binomial Model to see how many tips is a good result


# Get 2020 fav win probs
odds_results_df_2020 <- odds_results_df %>% filter(season == 2020) %>% mutate(fav_prob = pmax(home_prob_close, away_prob_close, na.rm = TRUE))

pp <- odds_results_df_2020$fav_prob

# Define intervals
kk <- 1:length(pp)

# Plot the distribution
data_frame( correct_tips = kk, probability = ppoibin(kk=kk, pp=pp) ) %>%
  ggplot( aes( x = correct_tips, y = probability)) +
  geom_point() +
  geom_vline(xintercept = 130, color = 'red') +
  geom_text(x = 130, y = ppoibin(kk=130, pp=pp) , label = "Tipping 130 Favs", color="orange", size=7, fontface="bold", hjust = 1.1 ) +
  labs(x = "Correct Tips",
       y = "Cumulative Percentage",
       title = "Cumulative Poisson Binomial Distrubtion - NRL 2020 Season",
       subtitle = "Shows the nth percentile of getting X number of correct tips.",
       caption = "Data Sourced from: http://www.aussportsbetting.com/") +
  theme_measurem






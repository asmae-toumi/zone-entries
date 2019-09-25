############################

## ---------------------- ##
##   Adding stathletes    ##
## ---------------------- ##

############################

# required libraries
library(tidyverse)
library(readxl)
library(seismicRoll)


# import datasets 
Points <- read_csv("~/zone-entries/data/Points.csv")
df_zone <- read_excel("~/zone-entries/data/Zone Entries NHL 1718&1819.xlsx")
pbp_data <- readRDS("~/zone-entries-nhl/data/pbp_corsi_1719.rds")



### DATA CLEANING POINTS DATA ###

# select variables of interest 
Points <- Points %>% select(season, Entry.Player, Position, GP, P_60) 

# convert entry.player names to upper case 
Points <- Points %>% mutate(Entry.Player = str_to_upper(Entry.Player))

# Convert Position to a factor
Points <- Points %>% 
  mutate(Position = as.factor(Position))

# Examine Position levels
Points %>% 
  pull(Position) %>% 
  levels() #"Center" "Defense" "Left Wing" "Right Wing"

# Convert positions to either F or D
Points <- Points %>% 
  mutate(Position = recode_factor(Position, 
                             "Defense" = "Defenseman",
                             .default = "Forward"))


### DATA CLEANING STATHLETES DATA ###

# names
names(df_zone) <- make.names(names(df_zone))
head(df_zone)

# 5 v 5 only
df_zone <- df_zone %>% filter(period <= 3) %>% 
  filter(!is.na(def6), !is.na(def5), !is.na(off5), !is.na(off6))

# seconds into game
df_zone <- df_zone %>% 
  separate(Time, into = c("preamble", "minutes", "seconds"), sep = ":") %>% 
  select(-preamble) 
df_zone <- df_zone %>% 
  mutate(seconds = as.numeric(seconds), minutes = as.numeric(minutes)) %>% 
  mutate(seconds_left_period = 60*minutes + seconds, 
         seconds_left_game = seconds_left_period + (3-period)*20*60, 
         seconds_into_game = 3600-seconds_left_game)
df_zone <- df_zone %>% 
  mutate(game_minute = floor(seconds_into_game/60))

# convert entry.player names to upper case
df_zone <- df_zone %>% mutate(Entry.Player = str_to_upper(Entry.Player))

# creating season 
df_zone <- df_zone %>%
  # format as a date
  mutate(Date = lubridate::ymd(Date),
         # Choose a middle date between the season
         # then if between the date range of the season call it that season
         season = case_when(Date > "2017-08-01" & Date < "2018-08-01" ~ "20172018",
                            Date > "2018-08-01" & Date < "2019-08-01" ~ "20182019"
         )) 

# double checking it worked 
df_zone %>%
  group_by(nhl_game_id, season) %>% 
  summarise() %>% 
  ungroup() %>%
  count(season)

# convert season to numeric for merging purposes
df_zone <- df_zone %>% 
  mutate(season = as.numeric(season))

# left join df_zone to Points df by season and Entry.Player
df_zone <- left_join(df_zone, Points, by = c("season", "Entry.Player"))

# rearranging column order 
df_zone <- df_zone %>% 
  select(season, everything()) %>% 
  select(1:7, Position, everything()) %>%
  select(1:8, GP, everything()) %>%
  select(1:9, P_60, everything())



### CREATING GRID DF ###

# create df with variables of interest 
grid_ml <- pbp_data %>%
  filter(game_period < 4) %>% 
  select(season, game_id, game_period, game_seconds, game_strength_state, 
         event_team, home_team, away_team, home_score, away_score, event_type, pred_XGB_7) %>%
  mutate(event_type = tolower(event_type)) 

# fixed factors for each game
team_factors <- grid_ml %>% 
  select(season, game_id, home_team, away_team) %>%
  unique()

# factors that will vary at each event
play_factors <- grid_ml %>% 
  select(season, game_id, game_seconds, game_period, game_strength_state, event_team, home_score, away_score, event_type, pred_XGB_7)


grid_ml_long <- grid_ml %>% 
  select(game_id) %>% 
  ungroup() %>% 
  unique() %>% 
  group_by(game_id) %>% 
  expand(game_seconds = 1:3600)

grid_ml_long <- grid_ml_long %>%
  inner_join(team_factors)

grid_ml_merged <- grid_ml_long %>% 
  left_join(play_factors)

grid_ml_merged <- grid_ml_merged %>% 
  mutate(game_strength_state = ifelse(game_seconds == 1, "5v5", game_strength_state), 
         home_score = ifelse(game_seconds == 1, 0, home_score), 
         away_score = ifelse(game_seconds == 1, 0, away_score))

grid_ml_merged <- grid_ml_merged %>% 
  mutate(game_period = ifelse(game_seconds <= 1200, 1, ifelse(game_seconds <= 2400, 2, 3))) %>% 
  group_by(game_id) %>% 
  fill(game_strength_state, home_score, away_score) 

grid_ml_merged <- grid_ml_merged %>% 
  replace_na(list(pred_XGB_7 = 0, event_type = "no shot", event_team = "no event")) %>% # replace all the NAs as 0's (no xGs) or "no shot"
  mutate(is_shot_on_goal = event_type %in% c("goal", "shot"), 
         is_event_home = home_team == event_team, 
         is_event_away = away_team == event_team)


grid_ml_merged <- grid_ml_merged %>% 
  mutate(home_xg = ifelse(is_event_home, pred_XGB_7, 0), 
         away_xg = ifelse(is_event_away, pred_XGB_7, 0), 
         net_xg = home_xg - away_xg,  # second level data, netXgs relative to home team
         net_sog = as.numeric(is_shot_on_goal)*ifelse(is_event_home, 1, -1))  # shot_on_goal data, each second


grid_ml_merged_roll <- grid_ml_merged %>% 
  arrange(game_id, game_seconds) %>% 
  group_by(game_id, game_period) %>% 
  mutate(xg_home_next30 = roll_mean(home_xg, 30, align = "left")*30, 
         xg_away_next30 = roll_mean(away_xg, 30, align = "left")*30, 
         net_xg_next30 = roll_mean(net_xg, 30, align = "left")*30, # sum of next 30 seconds of xGs
         net_sog_next30 = roll_mean(net_sog, 30, align = "left")*30) %>%  # next 30 seconds of shots on goal
  filter(game_seconds <= 1170 | (game_seconds >= 1201 & game_seconds <= 2370)|
           (game_seconds >= 2401 & game_seconds <= 3570)) # drop last 30 seconds of each period


# Check random time periods
grid_ml_merged %>% filter(game_id == 2017020702, game_seconds >= 2000, game_seconds <= 2030) %>% print.data.frame()
grid_ml_merged_roll %>% filter(game_id == 2017020702, game_seconds == 2000)

grid_ml_merged %>% filter(game_id == 2017020886, game_seconds >= 860, game_seconds <= 890) %>% print.data.frame()
grid_ml_merged_roll %>% filter(game_id == 2017020886, game_seconds == 860) %>% print.data.frame()

grid_final <- grid_ml_merged_roll %>% 
  ungroup() %>% 
  select(game_id, game_seconds, game_period, home_team, away_team, game_strength_state, home_score, away_score, 
         xg_home_next30:net_sog_next30)

grid_ml_merged %>% 
  filter(home_score == away_score, game_strength_state == "5v5") %>% 
  mutate(game_minute = floor(game_seconds/60)) %>% 
  group_by(game_minute) %>%  
  summarise(ave_score = mean(net_xg)) %>% 
  ggplot(aes(game_minute, ave_score)) + 
  geom_point() + 
  geom_line() 


### JOINING GRID_FINAL WITH DF_ZONE ###

# renaming variables for merging 
df_zone <- df_zone %>% rename(game_seconds = seconds_into_game) 
df_zone <- df_zone %>% rename(game_id = nhl_game_id)

# convert df_zone game_id into character 
df_zone <- df_zone %>% 
  mutate(game_id = as.character(game_id))

# merge
df_zone_merged <- left_join(df_zone, grid_final, by = c("game_id", "game_seconds"))

grid_ml_merged_roll %>% filter(game_id == 2017020001, game_seconds == 36) %>% print.data.frame()
grid_ml_merged %>% filter(game_id == 2017020001, game_seconds >= 36, game_seconds <= 66) %>% print.data.frame()
df_zone_merged %>% filter(game_id == 2017020001, game_seconds == 36) %>% print.data.frame()
pbp_data %>% filter(game_id == 2017020001) %>% slice(1:20)


grid_ml_merged_roll %>% filter(game_id == 2017020004, game_seconds == 21) %>% print.data.frame()
grid_ml_merged %>% filter(game_id == 2017020004, game_seconds >= 21, game_seconds <= 51) %>% print.data.frame()
df_zone_merged %>% filter(game_id == 2017020004, game_seconds == 21) %>% print.data.frame()
pbp_data %>% filter(game_id == 2017020004) %>% slice(1:20)


# write to new data set 
write_csv(df_zone_merged, "~/zone-entries/data/stathletes_merged.csv")


  


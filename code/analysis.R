###################################

## ----------------------------- ##
##   Analysis + Initial Charts   ##
## ----------------------------- ##

###################################

# Required libraries
library(tidyverse)
library(readxl)
library(splines)
library(scico)
library(Matching)
library(gtools)
library(cowplot)
library(BART)

# import tracking data
df_zone_init <- read_csv("~/zone-entries/data/stathletes_merged.csv")

# Change net metrics to compare to team that entered the zone
df_zone_adj <- df_zone %>% 
  mutate(score_diff = ifelse(Entry.Team.Venue == "home", home_score - away_score, away_score - home_score), #score diff, carry-in team
         xg_30_for = ifelse(Entry.Team.Venue == "home", xg_home_next30, xg_away_next30), #xg for, carry-in team
         xg_30_against = ifelse(Entry.Team.Venue == "home", xg_away_next30, xg_home_next30), #xg against, carry-in team
         xg_30_net = ifelse(Entry.Team.Venue == "home", net_xg_next30, -1*net_xg_next30), #net xg, carry-in team
         sog_30_net = ifelse(Entry.Team.Venue == "home", net_sog_next30, -1*net_sog_next30))

df_zone_adj <- df_zone_adj %>% 
  dplyr::select(-xg_home_next30, -xg_away_next30, -net_sog_next30, -net_xg_next30)

df_zone_adj %>% 
  filter(game_strength_state== "5v5", home_score == away_score) %>% 
  ggplot(aes(game_minute, fill = Type)) + 
  geom_bar(position = "fill") + 
  labs(title = "Proportion of entry type by game minute") + 
  facet_wrap(~period, scales = "free")

plot(df_zone_adj, aes(game_minute, color = Type)) +
  geom_freqpoly(binwidth = 1) +
  facet_wrap(~period, scales = "free")

zone_med <- df_zone_adj %>% 
  group_by(Type) %>% 
  summarise(med_entry = median(length_of_shift)) 

zone_med

df_zone_adj %>% 
  ggplot() + 
  geom_density(alpha = 0.2, aes(length_of_shift, fill = Type)) + 
  geom_vline(data=zone_med, aes(xintercept=med_entry, colour=Type),
             linetype="dashed", size=0.5) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  xlim(c(0, 100)) + 
  theme_classic(14) + 
  labs(x = "Shift length (seconds)", y = "", title = "Distribution of shift length") + 
  scale_y_continuous(labels = scales::percent)

top_10_dump <- df_zone_adj %>% 
  group_by(Entry.Player) %>% 
  summarise(ave_entry_dumped = mean(Type == "Dumped"), n_entries = n()) %>% 
  filter(n_entries >= 100) %>% 
  arrange(-ave_entry_dumped) %>% 
  slice(1:10)


top_10_carry <- df_zone_adj %>% 
  group_by(Entry.Player) %>% 
  summarise(ave_entry_dumped = mean(Type == "Carried"), n_entries = n()) %>% 
  filter(n_entries >= 100) %>% 
  arrange(-ave_entry_dumped) %>% 
  slice(1:10)

df_zone_adj %>% 
  count(Entry.Player) %>% 
  arrange(-n) %>% 
  slice(1:10) %>% 
  inner_join(df_zone) %>% 
  ggplot(aes(x = length_of_shift, colour = Type)) + 
  geom_density() + 
  xlim(0, 60) + 
  facet_wrap(~ Entry.Player)


df_zone_adj %>% 
  ggplot(aes(x_entry, y_entry, fill = Type, colour = Type)) + 
  geom_density_2d() + 
  labs(title = "Location of entry")

########################

## ---------------- ##
##   Outcomes       ##
## ---------------- ##

########################


df_zone_adj <- df_zone_adj %>% filter(game_strength_state == "5v5")

df_zone_adj %>% count(Type)

df_zone_adj %>% 
  group_by(Type) %>% 
  summarise(ave_any_shot_created = mean(Shot.Created), 
            ave_shots_for = mean(Total.Shots), 
            ave_zone_time = mean(Zone.Time), 
            ave_xg_for = mean(xg_30_for), 
            ave_xg_against = mean(xg_30_against), 
            ave_xg_net = mean(xg_30_net), 
            ave_sog_net = mean(sog_30_net), 
            n_entries = n()) %>% 
  print.data.frame()


df_zone_adj %>% 
  filter(!is.na(P_60)) %>% 
  mutate(skill_entry_player = cut_number(P_60, 5)) %>% 
  group_by(Type, skill_entry_player, Position) %>% 
  summarise(ave_any_shot_created = mean(Shot.Created), 
            ave_shots_for = mean(Total.Shots), 
            ave_zone_time = mean(Zone.Time), 
            ave_xg_for = mean(xg_30_for), 
            ave_xg_against = mean(xg_30_against), 
            ave_xg_net = mean(xg_30_net), 
            ave_sog_net = mean(sog_30_net), 
            n_entries = n()) %>% 
  print.data.frame()

df_zone_adj %>% 
  ggplot(aes(x = P_60)) + geom_histogram()

df_zone_adj %>% 
  group_by(Entry.Player, P_60) %>%
  count() %>% 
  filter(n > 300) %>% 
  unique() %>%
  arrange(-P_60) 

# add score_diff variable
df_zone_adj <- df_zone_adj %>% 
  mutate(score_diff = away_score - home_score) 

# converting entry type to factor
df_zone_adj <- df_zone_adj %>%
  mutate(Type = as.factor(Type))

# convert entry type "played" into "carried"
df_zone_adj <- df_zone_adj %>%
  mutate(Type = fct_collapse(Type, 
                             "Carried" = c("Played", "Carried"),
                             "Dumped" = "Dumped"))

# count carried vs dumped
df_zone_adj %>% group_by(Type) %>% tally() 

# adding RAPM metrics from evolving-hockey.com
df_RAPM <- read_csv("~/zone-entries/data/RAPM skaters.csv")

### CLEANING ###

df_RAPM <- df_RAPM %>% dplyr::select(player, season, position, GF_60, GPM_60, CPM_60)

df_RAPM <- df_RAPM %>%
  mutate(player = recode_factor(player, 
                                "5EBASTIAN.AHO" = "SEB.AHO"))

# replace periods between names with empty space
df_RAPM <- df_RAPM %>% mutate(player = str_replace(player, "\\.", " ")) 

# rename player to "Entry.Player"
df_RAPM <- df_RAPM %>% rename(Entry.Player = player)

# rename position to Position with capital P
df_RAPM <- df_RAPM %>% rename(Position_twins = position)

# left join df_zone to Points df by season and Entry.Player
df_zone_adj <- left_join(df_zone_adj, df_RAPM, by = c("season", "Entry.Player"))

##################################

## ---------------------------- ##
##   Propensity Score Matching  ##
## ---------------------------- ##

##################################

### PROPENSITY SCORE MODEL ###

# initial model
ps.1 <- glm(Type ~ Position + ns(GPM_60, 5)*score_diff + 
               ns(length_of_shift, 5) + ns(game_seconds, 3) + 
              ns(x_entry, 1)*ns(y_entry, 3), 
             data = df_zone_adj_model, family = "binomial") 

AIC(ps.1)

# find best spline terms  
for( i in 1:10) {
  for(j in 1:10) {
    for(k in 1:10){
      model <- glm(formula = Type ~ Position + ns(P_60, i) + ns(length_of_shift,
                                                                j) + ns(game_seconds, k) + score_diff + home_team + x_entry +
                     y_entry, family = "binomial", data = df_zone_adj)
      print(AIC(model))
      print(c(i,j,k))
      
    }}}

# final model 
ps.2 <- glm(is_carry ~ Position + ns(GPM_60, 5)*score_diff + 
               ns(length_of_shift, 10) + ns(game_seconds, 5) + ns(x_entry, 5)*ns(y_entry, 5), 
            data = df_zone_adj_model, family = "binomial") 

AIC(ps.2)


df_zone_adj_model$predict.fitms <- predict(ps.2, df_zone_adj_model, type = "response")

df_zone_adj_model <- df_zone_adj_model %>% filter(!is.na(GPM_60)) # remove NA's



# what do the propensity scores look like?
p1 <- df_zone_adj_model %>%
  ggplot(aes(x = predict.fitms)) +
  geom_histogram(aes(color = Type, fill = Type)) + 
  scale_fill_manual(values = c("#FFA3AF", "#132f3c")) +
  facet_wrap(~Type) +
  labs(title = "What do the Propensity Scores look like?",
       x = "Probability of Carry-in", 
       y = "Number of entries") +
  scale_x_continuous(breaks = scales::pretty_breaks())
  
p1

p2 <- df_zone_adj_model %>%
  filter(predict.fitms > 0.50) %>%
  ggplot(aes(x = predict.fitms, group = Type, fill = Type)) + 
  geom_density(color = "white", alpha = 0.9) +
  scale_fill_manual(values = c("#FFA3AF", "#132f3c")) +
  labs(title = "",
       x = "Probability of a Carry-in", 
       y = "Density") +
  theme_minimal()

p2

# filter for the common support interval
df_zone_adj_model <- df_zone_adj_model %>% 
  filter(!is.na(predict.fitms))

limits <- df_zone_adj_model %>% 
  group_by(Type) %>%
  summarise(min.score = min(predict.fitms), max.score = max(predict.fitms)) 

limits


p3 <- df_zone_adj_model %>%
  filter(predict.fitms > 0.50) %>%
  ggplot(aes(x = predict.fitms, group = Type, fill = Type)) + 
  geom_density(color = 'white', alpha = 0.9) +
  scale_fill_manual(values = c("#FFA3AF", "#132f3c")) +
  labs(title = "",
       x = "Probability of a Carry-in", 
       y = "Density") +
  theme_minimal()

p3


### MATCHING ###


df_zone_adj_model <- df_zone_adj_model %>% 
  mutate(Trt = as.numeric(Type))

set.seed(0)

x <- data.frame(x1 = (df_zone_adj_model$x_entry),
                x2 = (df_zone_adj_model$y_entry), 
                x3 = df_zone_adj_model$length_of_shift, 
                x4 = df_zone_adj_model$game_seconds)

match.att <- Matchby(Y = df_zone_adj_model$xg_30_net, by = c(df_zone_adj_model$Entry.Player),
                   estimand = "ATT",
                   Tr = df_zone_adj_model$Trt,
                   M = 1,  # number of matches
                   caliper = c(0.25),
                   ties = FALSE,
                   X = df_zone_adj_model$predict.fitms, 
                   replace=T, 
                   Weight = 1)

summary(match.att)

pairs.att <- cbind(match.att$index.treated, match.att$index.control)
pairs.att %>% head()
dim(pairs.att)


# Treated and control data sets
treated.att <- df_zone_adj_model[match.att$index.treated,]
control.att <- df_zone_adj_model[match.att$index.control,]

# Define the paired id which is the row number 
treated.att <- cbind(treated.att, pairs.att[,2])
colnames(treated.att)[colnames(treated.att)== "pairs.att[, 2]"] <- "paired.id"
control.att <- cbind(control.att, pairs.att[,1])
colnames(control.att)[colnames(control.att)== "pairs.att[, 1]"] <- "paired.id"

# Define the matched pair for the team plot
control.att$off.pair <- control.att$off
treated.att$off.pair <- control.att$off
matched.att <- rbind(control.att, treated.att)


p4 <- matched.att %>%
  filter(predict.fitms > 0.6) %>%
  ggplot(aes(x = predict.fitms)) +
  geom_density(color = "white", alpha = 0.7, aes(fill = Type)) + 
  scale_fill_manual(values = c("#132f3c", "#FFA3AF")) +
  labs(title = "After matching",
       x = "Probability of a Carry-in", 
       y = "Density") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme_minimal_hgrid() 
  
p4

p5 <- df_zone_adj_model %>%
  filter(predict.fitms > 0.6) %>%
  ggplot(aes(x = predict.fitms)) +
  geom_density(color = "white", alpha = 0.7, aes(fill = Type)) + 
  scale_fill_manual(values = c("#132f3c", "#FFA3AF")) +
  labs(title = "Before matching",
       x = "Probability of a Carry-in", 
       y = "Density") +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.05))) +
  theme_minimal_hgrid() +
  theme(legend.position = "none")

p5


p_both <- plot_grid(p4, p5)
save_plot("p_both.png", p_both, ncol = 2, base_asp = 1.1) # save_plot better than gg_save with cowplot theme

# compare outcomes before and after matching 
matched.att %>% 
  group_by(Type) %>% 
  summarise(ave_net_goals = mean(xg_30_net), 
            ave_ga = mean(xg_30_against),
            ave_shots_net = mean(sog_30_net), 
            n_plays = n())

df_zone_adj %>% 
  group_by(Type) %>% 
  summarise(ave_net_goals = mean(xg_30_net), 
            ave_ga = mean(xg_30_against), 
            ave_shots_net = mean(sog_30_net), 
            n_plays = n()) 


# contour plot to check matching by location 
matched.att %>% 
  ggplot(aes(x_entry, y_entry, fill = Type, colour = Type)) + 
  geom_density_2d() + 
  labs(title = "Location of entry")

##################################

## ---------------------------- ##
##           BART               ##
## ---------------------------- ##

##################################

set.seed(0)

df_bart <- df_zone_adj_model %>% 
  filter(!is.na(GPM_60)) # remove NA's

## trt is a carry-in
trt <- df_bart$is_carry    

# x variables are the ones we want to condition on
x <- df_bart %>% 
  mutate(is_defender = as.numeric(Position == "Defenseman")) %>% 
  dplyr::select(is_defender, GPM_60, score_diff, length_of_shift, 
                game_seconds, x_entry, y_entry)

# outcome variable
y <- df_bart$xg_30_net

# combining treatment variable with x
xt = cbind(trt, x)

# among all entries, select carry-ins
xp1 = xt[trt,]

# force characteristics of dump-ins to reflect carry-ins
xp0 = xp1

# change the treatment to dump-ins
xp0[,1] = FALSE  # switch treatment label TRUE to FALSE

# run BART (note: use wbart for continuous outcomes)
library(BART); n.imps = 1000 # number of imputations
bart_mod = wbart(x.train = xt, 
                 y.train = y, 
                 k=2, 
                 ntree=100, 
                 ndpost=n.imps, 
                 nskip=500, 
                 printevery=100L)

# posterior predictions
bart_pred1 = pwbart(xp1, bart_mod$treedraws)
bart_pred0 = pwbart(xp0, bart_mod$treedraws)
dim(bart_pred0)  


# Average treatment effects on the treatment group 1 (ATTs)
n1 = nrow(xp1)
att10.est = NULL
for (m in 1:n.imps) {
  # potential outcomes for treatment group 1
  y11.hat = bart_pred1[m,]
  y10.hat = bart_pred0[m,]
  
  # att
  print(m)
  att10.est[m] = mean(y11.hat) - mean(y10.hat)
}

att10.bart = mean(att10.est)

# estimated ATT
att10.bart

Posterior_Summary = function(RD.est) {
  # risk difference
  RD.mean = mean(RD.est)
  RD.se = sd(RD.est)
  RD.lower = quantile(RD.est, probs=0.025)
  RD.upper = quantile(RD.est, probs=0.975)
  res = c(RD.mean, RD.se, RD.lower, RD.upper)
  names(res) = c("EST","SE","LOWER","UPPER")
  return(res)
}

att10 = Posterior_Summary(att10.est)
list(ATT10 = round(att10, digits = 3))


### HETEROGENEOUS TREATMENT EFFECTS ###

ci.fun <- function(a){
  c(quantile(a,.025),quantile(a,.975))
}

att.sim = bart_pred1 - bart_pred0 # Posterior distributions for each individual
atts.individual = apply(att.sim, 2, mean) # average for individual
atts.ci = apply(att.sim, 2, ci.fun) # 95% CI
df.att.hetero <- data.frame(att.est = atts.individual, low.bound = atts.ci[1,], upp.bound = atts.ci[2,], 
                            is_defender = xp1[,2], GPM_60 = xp1[,3], score_diff = xp1[,4],
                            length_of_shift = xp1[,5], game_seconds = xp1[,6], 
                            x_entry = xp1[,7], y_entry = xp1[,8])

df.att.hetero %>% 
  group_by(is_defender) %>% 
  summarise(ave_att = mean(att.est), 
            se_att = sd(att.est)/sqrt(n()))


# chart varying treatment effects
span = 0.5

# GPM by position
CATT_pos <- df.att.hetero %>% 
  filter(abs(GPM_60) <= 0.3) %>% 
  ggplot(aes(GPM_60, att.est, colour = as.factor(is_defender))) + 
  geom_smooth(method = "loess", se = FALSE, span = span, size = 0.8) + 
  geom_smooth(aes(GPM_60, low.bound), method = "loess", lty = 3, se = FALSE, span = span, size = 0.75) + 
  geom_smooth(aes(GPM_60, upp.bound), method = "loess", lty = 3, se = FALSE, span = span, size = 0.75) + 
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal() + 
  scale_color_scico_d(palette = "berlin", begin = 0.3, labels = c("Forward", "Defense")) +
  labs(title = "Conditional ATT's",
       subtitle = "by position",
       x = "GPM per 60",
       y = "Estimated ATT") +
  theme(legend.title = element_blank()) + 
  scale_y_continuous(breaks = scales::pretty_breaks())

CATT_pos 

ggsave("CATT_pos.png")

# all x variables
CATT_covs <- df.att.hetero %>%
  filter(abs(GPM_60) < 0.3, 
         length_of_shift < 75, 
         abs(score_diff <= 5)) %>% 
  gather(covar.att, value, GPM_60:y_entry) %>% 
  ggplot(aes(value, att.est)) +
  geom_smooth(method = "loess", se = FALSE, span = span, size = 0.8,  color = "#FFA3AF") + 
  geom_smooth(aes(value, low.bound), method = "loess", lty = 3, se = FALSE, span = span, size = 0.75, color = "#FFA3AF") + 
  geom_smooth(aes(value, upp.bound), method = "loess", lty = 3, se = FALSE, span = span, size = 0.75,  color = "#FFA3AF") + 
  facet_wrap(~covar.att, scales = "free") + 
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal() +
  labs(title = "Conditional ATT's", 
       subtitle = "for different covariates", 
       x = NULL,
       y = "Estimated ATT") 

CATT_covs

ggsave("CATT_covs.png")

# spatial plot 
spatial_plot <- df.att.hetero %>%
  mutate(xcoords = x_entry/5.4, ycoords = y_entry - 42.5) %>%
  ggplot() +
  gg_rink(side = "right", specs = "NHL") + #see nhl_rink.R for code
  geom_raster(aes(x=xcoords, y=ycoords, fill = att.est)) +
  labs(title = "",
       subtitle = "",
       x = NULL,
       y = NULL, 
       fill = "Δ net xG") +
  theme_void() +
  scale_fill_viridis_c(lim = c(-0.01, 0.03), paste0(expression(Delta), "\n NetXG")) 

spatial_plot

ggsave("spatial_plot.png")


### TEAM-LEVEL TREATMENT EFFECTS ###

# entry team distribution of ATT

team_vector <- c(unique(df_zone_adj_model$Entry.Team))
summary_table <- NULL
df_team_i <- NULL
df_team_all <- NULL

for(i in 1:length(team_vector)) {
  
  df_bart <- df_zone_adj_model %>%
    filter(Entry.Team == team_vector[i]) %>%
    filter(!is.na(GPM_60)) 
  
  trt <- df_bart$is_carry    
  
  ## x variables are the ones we want to condition on. Factors instead of characters
  xa <- df_bart %>% 
    mutate(is_defender = as.numeric(Position == "Defenseman")) %>% 
    dplyr::select(is_defender, GPM_60, score_diff, length_of_shift, 
                  game_seconds, x_entry, y_entry) 
  
  ## outcome variable
  ya <- df_bart$xg_30_net 
  
  ## combining treatment variable with x
  xat = cbind(trt, xa) 
  
  ## among all entries, select carry-ins
  xap1 = xat[trt,] 
  
  ## force characteristics of dump-ins to reflect carry-ins
  xap0 = xap1 
  
  ## change the treatment to dump-ins
  xap0[,1] = FALSE  # switch treatment label TRUE to FALSE
  
  ## run BART (wbart for continuous outcomes)
  library(BART); n.imps = 1000 # number of imputations
  bart_mod = wbart(x.train = xat, y.train = ya, 
                   k=2, ntree=100, ndpost=n.imps, 
                   nskip=500, printevery=100L)
  
  ## posterior predictions
  bart_pred1 = pwbart(xap1, bart_mod$treedraws)
  bart_pred0 = pwbart(xap0, bart_mod$treedraws)
  
  dim(bart_pred0)  
  
  
  # Average treatment effects on the treatment group 1 (ATTs)
  n1 = nrow(xap1)
  att10.est.a = NULL
  for (m in 1:n.imps) {
    # potential outcomes for treatment group 1
    y11.hat.a = bart_pred1[m,]
    y10.hat.a = bart_pred0[m,]
    
    # att
    print(m)
    att10.est.a[m] = mean(y11.hat.a) - mean(y10.hat.a)
  }
  
  att10.bart = mean(att10.est.a)
  
  
  ## ESTIMATED ATT
  print(att10.bart)
  
  Posterior_Summary = function(RD.est) {
    # risk difference
    RD.mean = mean(RD.est)
    RD.se = sd(RD.est)
    RD.lower = quantile(RD.est, probs=0.025)
    RD.upper = quantile(RD.est, probs=0.975)
    res = tibble(EST = RD.mean, SE = RD.se, LOWER = RD.lower, UPPER = RD.upper)
    names(res) = c("EST","SE","LOWER","UPPER")
    return(res)
  }
  att10 = Posterior_Summary(att10.est.a)
  
  print(att10)
  
  df_team_i <- data.frame(team = team_vector[i], est_team = att10.est.a)
  
  df_team_all <- bind_rows(df_team_all, df_team_i)
  
  summary_table <- rbind(summary_table, cbind(att10.bart, att10))
  
} 


# plot for entry team distribution of ATT

df_team_all %>%
  mutate(team_fct = fct_rev(as.factor(team))) %>%
  ggplot(aes(y= team_fct)) +
  geom_density_ridges(
    aes(x=est_team))

df_med <- df_team_all %>% 
  group_by(team) %>% 
  summarise(med_team = median(est_team))

df_team_all_1 <- df_team_all %>% inner_join(df_med)

p1 <- df_team_all_1 %>%
  ggplot(aes(y= fct_reorder(team, med_team), x=est_team)) +
  geom_density_ridges(color = "white", fill = "#132f3c", quantile_lines = TRUE, quantiles = 2) +
  scale_y_discrete(expand = c(0.03, 0)) +
  scale_x_continuous(limits = c(-0.005, 0.04), breaks = seq(0.00, 0.03, 0.01), expand=c(0,0)) +
  labs(x = "ATT", y = "") +
  theme_ridges()

p1


## defending team distribution of ATT

dteam_vector <- c(unique(df_zone_adj_model$Defending.Team))
summary_table_d <- NULL
df_team_i_d <- NULL
df_team_all_d <- NULL

for(i in 1:length(dteam_vector)) {
  
  df_bart <- df_zone_adj_model %>%
    filter(Defending.Team == dteam_vector[i]) %>%
    filter(!is.na(GPM_60)) 
  
  trt <- df_bart$is_carry    
  
  ## x variables are the ones we want to condition on. Factors instead of characters
  xa <- df_bart %>% 
    mutate(is_defender = as.numeric(Position == "Defenseman")) %>% 
    dplyr::select(is_defender, GPM_60, score_diff, length_of_shift, 
                  game_seconds, x_entry, y_entry) 
  
  ## outcome variable
  ya <- df_bart$xg_30_net 
  
  ## combining treatment variable with x
  xat = cbind(trt, xa) 
  
  ## among all entries, select carry-ins
  xap1 = xat[trt,] 
  
  ## force characteristics of dump-ins to reflect carry-ins
  xap0 = xap1 
  
  ## change the treatment to dump-ins
  xap0[,1] = FALSE  # switch treatment label TRUE to FALSE
  
  ## run BART (wbart for continuous outcomes)
  library(BART); n.imps = 1000 # number of imputations
  bart_mod = wbart(x.train = xat, y.train = ya, 
                   k=2, ntree=100, ndpost=n.imps, 
                   nskip=500, printevery=100L)
  
  ## posterior predictions
  bart_pred1 = pwbart(xap1, bart_mod$treedraws)
  bart_pred0 = pwbart(xap0, bart_mod$treedraws)
  
  dim(bart_pred0)  
  
  
  # Average treatment effects on the treatment group 1 (ATTs)
  n1 = nrow(xap1)
  att10.est.a = NULL
  for (m in 1:n.imps) {
    # potential outcomes for treatment group 1
    y11.hat.a = bart_pred1[m,]
    y10.hat.a = bart_pred0[m,]
    
    # att
    print(m)
    att10.est.a[m] = mean(y11.hat.a) - mean(y10.hat.a)
  }
  
  att10.bart = mean(att10.est.a)
  
  
  ## ESTIMATED ATT
  print(att10.bart)
  
  Posterior_Summary = function(RD.est) {
    # risk difference
    RD.mean = mean(RD.est)
    RD.se = sd(RD.est)
    RD.lower = quantile(RD.est, probs=0.025)
    RD.upper = quantile(RD.est, probs=0.975)
    res = tibble(EST = RD.mean, SE = RD.se, LOWER = RD.lower, UPPER = RD.upper)
    names(res) = c("EST","SE","LOWER","UPPER")
    return(res)
  }
  att10 = Posterior_Summary(att10.est.a)
  
  print(att10)
  
  df_team_i_d <- data.frame(team = dteam_vector[i], est_team = att10.est.a)
  
  df_team_all_d <- bind_rows(df_team_all_d, df_team_i_d)
  
  summary_table_d <- rbind(summary_table_d, cbind(att10.bart, att10))
  
  
} 


# plot for defending team
df_team_all_d %>%
  mutate(team_fct = fct_rev(as.factor(team))) %>%
  ggplot(aes(y= team_fct)) +
  geom_density_ridges(
    aes(x=est_team))

df_med_d <- df_team_all_d %>% 
  group_by(team) %>% 
  summarise(med_team = median(est_team))

df_team_all_1_d <- df_team_all_d %>% inner_join(df_med_d)

p2 <- df_team_all_1_d %>%
  ggplot(aes(y= fct_reorder(team, med_team), x=est_team)) +
  geom_density_ridges(color = "white", fill = "#132f3c", quantile_lines = TRUE, quantiles = 2) +
  scale_y_discrete(expand = c(0.03, 0)) +
  scale_x_continuous(limits = c(-0.005, 0.04), breaks = seq(0.00, 0.03, 0.01), expand=c(0,0)) +
  labs(
    x = "ATT", 
    y = "",
    title = "Defending Team Distribution of ATT"
  ) +
  theme_ridges()

p2

plot_grid(p1, p3)

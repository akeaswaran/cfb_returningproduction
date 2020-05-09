#devtools::install_github("meysubb/cfbscrapR")
easypackages::libraries("tidyverse", "ggplot2", "ggimage", "cfbscrapR")

## Load in Returning Production Data from my github (data from ESPN's Bill Connelly)
retprod <- data.frame()
for (i in 2015:2019){
  csv <- paste0("https://raw.githubusercontent.com/spfleming/cfb_returningproduction/master/data/", i, "%20retprod.csv")
  dat <- read.csv(csv) %>% mutate(year = i)
  retprod <- bind_rows(retprod, dat)
}

###### TEAM STATS (ALL FOR REGULAR SEASON ONLY) ###### 

## POINTS PER GAME, POINTS ALLOWED, POINTS SCORED
# Define function to pull game info and create points for and points against
      ### Note: 2019 Air Force vs New Mexico got rescheduled, so data is null. Filter out game ID "401117539".

points.for <- function(year){
  data <- cfb_game_info(year) %>% filter(id != 401117539)
  a <- data %>% group_by(home_team) %>% summarise(points.for = sum(home_points))
  b <- data %>% group_by(away_team) %>% summarise(points.for = sum(away_points))
  left_join(a, b, by = c("home_team" = "away_team")) %>% 
    mutate(points.for = points.for.x + points.for.y) %>% select(-points.for.x, -points.for.y)
}

points.against <- function(year){
  data <- cfb_game_info(year) %>% filter(id != 401117539)
  a <- data %>% group_by(home_team) %>% summarise(points.against = sum(away_points))
  b <- data %>% group_by(away_team) %>% summarise(points.against = sum(home_points))
  left_join(a, b, by = c("home_team" = "away_team")) %>% 
    mutate(points.against = points.against.x + points.against.y) %>% select(-points.against.x, -points.against.y)
}

# Points for and Points against

off <- data.frame()
def <- data.frame()
for(i in 2014:2019){
 dat <- points.for(i) %>% mutate(year = i)
 off <- bind_rows(off, dat)
 dat <- points.against(i) %>% mutate(year = i)
 def <- bind_rows(def, dat)
} 

# Merge these and we have a nice balanced panel.
    ## Note: two FCS teams are somehow included, so I filtered for NAs to get rid of them, 
    ## then I selected to clean up the columns and created the point differential variable I'll use later.

season.points <- off %>% left_join(def, by = c("home_team", "year")) %>% filter(!is.na(points.for)) %>% 
  select(team = home_team, year, points.for, points.against) %>% mutate(point.diff = points.for - points.against)


## Per Play Stats

# Get play-by-play data 
  ## (THIS TAKES APPROX 25 MINUTES TO RUN, I RECOMMEND DOING IT ONCE, SAVING PBP IN YOUR ENVIRONMENT, THEN COMMENTING IT OUT)

pbp <- data.frame()
for(i in 2014:2019){
  dat <- cfb_pbp_data(year = i, week = NULL, epa_wpa = TRUE) %>% mutate(year = i)
  pbp <- bind_rows(pbp, dat)
}

# Filter for all rushes and passes and add garbage time, success rates. 

plays <- pbp %>% filter(rush == 1 | pass == 1) %>% 
  mutate(epa.success = ifelse(EPA > 0, 1, 0),
         yards.success = ifelse(down == 1 & yards_gained >= .5*distance, 1,
                                ifelse(down == 2 & yards_gained >= .7*distance, 1, 
                                       ifelse((down == 3 | down == 4) * yards_gained >= distance, 1, 0))),
         garbage_time = ifelse(period == 1 & abs(score_diff) > 43, 1, 
                                         ifelse(period == 2 & abs(score_diff) > 37, 1,
                                                ifelse(period == 3 & abs(score_diff) > 27, 1,
                                                       ifelse(period == 4 & abs(score_diff) > 22, 1, 0)))))

# Optional: Can filter out garbage time, replace all interceptions with the average value. (Might do this later.)

# Calculate EPA, YPP, and success rates for offense and defense, then merge and create variables for margins.

off.play.stats <- plays %>%
  group_by(year, offense_play) %>% 
  summarise(epa.off = mean(EPA), ypp.off = mean(yards_gained), 
            success.rte.off = mean(yards.success), epa.success.rte.off = mean(epa.success), num.plays.off = n()) %>% 
  filter(num.plays.off>300)

def.play.stats <- plays %>%
  group_by(year, defense_play) %>% 
  summarise(epa.def = mean(EPA), ypp.def = mean(yards_gained),
            success.rte.def = mean(yards.success), epa.success.rte.def = mean(epa.success), num.plays.def = n()) %>% 
  filter(num.plays.def>300)

play.stats <- left_join(off.play.stats, def.play.stats, by = c("offense_play" = "defense_play", "year")) %>% 
  ungroup() %>% mutate(
    epa.margin = epa.off-epa.def, ypp.margin = ypp.off-ypp.def,
    sr.margin = success.rte.off-success.rte.def, 
    epa.sr.margin = epa.success.rte.off - epa.success.rte.def) %>%
  select(team = offense_play, year, epa.off, epa.def, epa.margin, ypp.off, ypp.def, ypp.margin, 
         success.rte.off, success.rte.def, sr.margin, epa.success.rte.off, epa.success.rte.def, epa.sr.margin)

## Per Drive Stats

# Get drive data
drive_level <- data.frame()
for(i in 2014:2019){
  dat <- cfb_pbp_data(year = i, week = NULL, epa_wpa = TRUE, drive = TRUE) %>% mutate(year = i)
  drive_level <- bind_rows(drive_level, dat)
}

# Calculate Points Per Drive and success rates, then merge and calculate margins.
drives <- drive_level %>% 
  mutate(
  drive.pts = case_when(drive_result == "TD" ~ 6, 
                        drive_result == "FG" ~ 3,
                        drive_result == "INT TD" ~ -6,
                        drive_result == "FUMBLE TD" ~ -6,
                        drive_result == "FUMBLE RETURN TD" ~ -6,
                        TRUE ~ 0),
  drive.success = ifelse(drive_result %in% c("TD", "FG"), 1, 0),
  drive.success.td = ifelse(drive_result == "TD", 1, 0))

off.drive.stats <- drives %>% group_by(offense, year) %>% 
  summarise(ppd.off = mean(drive.pts),
            success.rte.off = mean(drive.success),
            td.rte.off = mean(drive.success.td))

def.drive.stats <- drives %>% group_by(defense, year) %>% 
  summarise(ppd.def = mean(drive.pts),
            success.rte.def = mean(drive.success),
            td.rte.def = mean(drive.success.td))

drive.stats <- left_join(off.drive.stats, def.drive.stats, by = c("offense" = "defense", "year")) %>% 
  mutate(ppd.margin = ppd.off-ppd.def,
         success.rte.margin = success.rte.off - success.rte.def,
         td.rte.margin = td.rte.off - td.rte.def) %>%
  select(team = offense, year, ppd.off, ppd.def, ppd.margin, success.rte.off, success.rte.def, success.rte.margin,
         td.rte.off, td.rte.def, td.rte.margin)






                                   
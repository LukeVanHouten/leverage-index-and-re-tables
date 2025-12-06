library(RPostgres)
library(tidyverse)
library(tools)
library(reshape2)

All <- c(2015:2019, 2021:2023)

conn <- dbConnect(Postgres(), dbname = "drpstatcast", host = "localhost",
                  port = 5432, user = "postgres", password = "drppassword")

stats_query <- "
SELECT *
FROM statcast
WHERE game_date NOT BETWEEN '2015-03-28' AND '2015-04-04'
   AND game_date NOT BETWEEN '2015-10-06' AND '2015-11-01'
   AND game_date NOT BETWEEN '2016-03-28' AND '2016-04-02'
   AND game_date NOT BETWEEN '2016-10-04' AND '2016-11-02'
   AND game_date NOT BETWEEN '2017-03-28' AND '2017-04-01'
   AND game_date NOT BETWEEN '2017-10-03' AND '2017-11-01'
   AND game_date NOT BETWEEN '2018-10-02' AND '2018-10-28'
   AND game_date NOT BETWEEN '2019-10-01' AND '2019-10-30'
   AND game_date NOT BETWEEN '2021-03-01' AND '2021-03-31'
   AND game_date NOT BETWEEN '2021-10-05' AND '2021-11-02'
   AND game_date NOT BETWEEN '2022-03-28' AND '2022-04-06'
   AND game_date NOT BETWEEN '2022-10-07' AND '2022-11-05'
   AND game_date NOT BETWEEN '2023-03-28' AND '2023-03-29'
   AND game_date NOT BETWEEN '2023-10-03' AND '2023-11-01'
   AND game_year != 2020
"

stats_df <- dbGetQuery(conn, stats_query)

re_df <- stats_df %>%
    select(game_date, game_pk, game_year, batter, pitcher, events, home_team, 
           away_team, on_1b, on_2b, on_3b, outs_when_up, inning, inning_topbot, 
           at_bat_number, batting_team, fielding_team, bat_score, 
           post_bat_score, home_score, away_score, delta_home_win_exp) %>%
    filter(!(events %in% c("", "ejection", "game_advisory", "stolen_base_2b", 
                           "stolen_base_3b"))) %>%
    mutate(across(c(on_1b, on_2b, on_3b), ~ ifelse(is.na(.), 0, 1)), 
           half_inning = paste0(inning_topbot, "_", inning),
           rbi = post_bat_score - bat_score, 
           bases = paste0(ifelse(on_1b == 1, "1", "_"), 
                          ifelse(on_2b == 1, "2", "_"),
                          ifelse(on_3b == 1, "3", "_")),
           play = paste0(outs_when_up, ":", bases)) %>%
    select(-inning_topbot, -on_1b, -on_2b, -on_3b) %>%
    arrange(game_date, game_pk, at_bat_number) %>%
    group_by(game_pk, half_inning) %>%
    mutate(play_score = max(post_bat_score) - bat_score) %>%
    ungroup()

get_table <- function(moment, stadium, year, ...) {
    bat_pitch <- list(...)
    year_all <- "2015-2023"
    if (stadium != "All") {
        games_df <- filter(re_df, home_team == stadium)
    } else {
        games_df <- re_df
    }
    if (paste0(year, collapse = ", ") != "All") {
        games_df <- filter(games_df, game_year %in% year)
        year_all <- paste0(year, collapse = ", ")
    }
    if ("batting" %in% names(bat_pitch)) {
        games_df <- filter(games_df, batting_team == bat_pitch$batting)
    }
    if ("pitching" %in% names(bat_pitch)) {
        games_df <- filter(games_df, fielding_team == bat_pitch$pitching)
    }
    
    run_expectancies <- games_df %>%
        filter(inning <= 9, half_inning != "Bot_9") %>%
        group_by(play) %>%
        summarize(re = ifelse(!!moment == "var", round(var(play_score), 3), 
                              round(mean(play_score), 3)), .groups = "drop") %>%
        mutate(bases = substr(play, 3, 5), outs = substr(play, 1, 1)) %>%
        select(-play) %>%
        pivot_wider(names_from = outs, values_from = re) %>%
        as.data.frame() %>%
        arrange(match(bases, c("___", "1__", "_2_", "__3", "12_", "1_3", "_23",
                               "123"))) %>%
        rbind(., c(stadium, ifelse(moment == "var", "RV", "RE"), "Table", 
                   year_all)) %>%
        `rownames<-`(.$bases) %>%
        select(-bases)
    
    if (length(bat_pitch) > 0) {
        run_expectancies <- rbind(run_expectancies, 
                                  c(ifelse(is.null(bat_pitch[1][[1]]), NA, 
                                           bat_pitch[1][[1]]), 
                                    toTitleCase(names(bat_pitch)[2]), 
                                    ifelse(is.null(bat_pitch[2][[1]]), NA, 
                                           bat_pitch[2][[1]])))
        rownames(run_expectancies)[[10]] <- toTitleCase(names(bat_pitch)[1])
    }
    return(run_expectancies)
}

re_table <- get_table(moment = "mean", stadium = "All", year = 2022, 
                      batting = "SEA")
rv_table <- get_table(moment = "var", stadium = "All", year = 2022, 
                      batting = "SEA")
# View(re_table)
# View(rv_table)

team_deviations <- function(moment, stadium, year, ...) {
    bat_pitch <- list(...)
    if (paste0(year, collapse = ", ") != "All") {
        year_all <- paste0(year, collapse = ", ")
    } else {
        year_all <- "2015-2023"
    }
    baseline_df <- get_table(moment, stadium, year)[1:8, ]
    baseline_matrix <- matrix(as.matrix(unname(baseline_df)), 
                              ncol = 3, dimnames = NULL) %>%
        `class<-`("numeric")
    
    if (paste(names(bat_pitch), collapse = "") == "batting") {
        team_df <- get_table(moment, stadium, year, batting = bat_pitch$batting)
    } else if (paste(names(bat_pitch), collapse = "") == "pitching") {
        team_df <- get_table(moment, stadium, year, 
                             pitching = bat_pitch$pitching)
    } else if (paste(names(bat_pitch), collapse = "") == "battingpitching") {
        team_df <- get_table(moment, stadium, year, batting = bat_pitch$batting, 
                             pitching = bat_pitch$pitching)
    } else {
        team_df <- baseline_df
    }
    team_matrix <- matrix(as.matrix(unname(team_df[1:8, ])), ncol = 3,
                          dimnames = NULL) %>%
        `class<-`("numeric")

    difference_matrix <- round(team_matrix - baseline_matrix, 3) %>%
        as.data.frame() %>%
        `colnames<-`(c("0", "1", "2"))
    difference_df <- rbind(difference_matrix, 
                           c(ifelse(moment == "var", "RV", "RE"), "Table",
                             year_all)) %>%
        `rownames<-`(c("___", "1__", "_2_", "__3", "12_", "1_3", "_23", "123",
                       stadium))
    
    if (length(bat_pitch) > 0) {
        difference_df <- rbind(difference_df, 
                               c(ifelse(is.null(bat_pitch[1][[1]]), NA, 
                                        bat_pitch[1][[1]]), 
                                 toTitleCase(names(bat_pitch)[2]), 
                                 ifelse(is.null(bat_pitch[2][[1]]), NA, 
                                        bat_pitch[2][[1]])))
        rownames(difference_df)[[10]] <- toTitleCase(names(bat_pitch)[1])
    }
    return(difference_df)
}

re_difference_table <- team_deviations(moment = "mean", stadium = "All", 
                                       year = 2022, batting = "SEA")
rv_difference_table <- team_deviations(moment = "var", stadium = "All", 
                                       year = 2022, batting = "SEA")
# View(re_difference_table)
# View(rv_difference_table)

no_extras_df <- filter(re_df, inning <= 9, half_inning != "Bot_9")
year_df <- no_extras_df %>%
    group_by(game_year, play) %>%
    summarize(re_year = round(mean(play_score), 3), 
              rv_year = round(var(play_score), 3), .groups = "drop")
home_df <- no_extras_df %>%
    group_by(game_year, home_team, play) %>%
    summarize(re_home = round(mean(play_score), 3), 
              rv_home = round(var(play_score), 3), .groups = "drop")
team_df <- no_extras_df %>%
    pivot_longer(cols = c(home_team, away_team), names_to = "location", 
                 values_to = "team") %>%
    group_by(game_year, team, play) %>%
    summarize(re_team = round(mean(play_score), 3), 
              rv_year = round(var(play_score), 3), .groups = "drop") %>%
    `colnames<-`(c("game_year", "home_team", "play", "re_team", "rv_team"))
team_home_df <- right_join(home_df, team_df, by = c("game_year", "home_team", 
                                                    "play")) %>%
    arrange(game_year, home_team, play)
team_home_play_df <- left_join(team_home_df, year_df, by = c("game_year", 
                                                             "play"))
play_df <- left_join(re_df, team_home_play_df, by = c("game_year", "home_team", 
                                                      "play")) %>%
    select(-inning) %>%
    group_by(game_pk, half_inning) %>%
    mutate(inning_pa = row_number(), 
           field_score = ifelse(bat_score == home_score, away_score, 
                                home_score))

get_re24 <- function(re_type, ...) {
    dots <- list(...)
    type <- as.name(paste0("re_", re_type))
    columns <- c("game_pk", "game_date", "batting_team", "fielding_team", 
                 "half_inning", "batter", "pitcher", "play", "events", "rbi", 
                 "re24", "inning_pa")
    
    if (all(c("batter", "year") %in% names(dots))) {
        re24_scores_batter <- play_df %>%
            group_by(game_pk, half_inning) %>%
            filter(game_year %in% dots$year, any(batter == dots$batter)) %>%
            mutate(re24 = lead(!!type, 
                               default = last(!!type)) - !!type + rbi) %>%
            mutate(re24 = ifelse(re24 == 0, -!!type, re24)) %>%
            ungroup() %>%
            filter(batter == dots$batter)
        
        cat(c(dots$batter, "RE24", sum(re24_scores_batter$re24)), sep = "_")
        return(re24_scores_batter[, c(columns[2:11], as.character(type))])
    
    } else if (all(c("pitcher", "year") %in% names(dots))) {
        re24_scores_pitcher <- play_df %>%
            group_by(game_pk, half_inning) %>%
            filter(game_year %in% dots$year, any(pitcher == dots$pitcher)) %>%
            mutate(re24 = lead(!!type, 
                               default = last(!!type)) - !!type + rbi) %>%
            mutate(re24 = ifelse(re24 == 0, -!!type, re24)) %>%
            ungroup() %>%
            filter(pitcher == dots$pitcher)
        
        cat(c(dots$pitcher, "RE24", -sum(re24_scores_pitcher$re24)), sep = "_")
        return(re24_scores_pitcher[, c(columns[2:11], as.character(type))])
        
    } else if ("game" %in% names(dots)) {
        re24_scores_game <- play_df %>%
            filter(game_pk == dots$game) %>%
            group_by(game_pk, half_inning) %>%
            mutate(re24 = lead(!!type, 
                               default = last(!!type)) - !!type + rbi) %>%
            mutate(re24 = ifelse(re24 == 0, -!!type, re24)) %>%
            ungroup()
        
        if (!(all(c("half_inning", "pa") %in% names(dots)))) {
            cat(unlist(unique(re24_scores_game[, columns[1:3]]))[c(1, 4:6)], 
                sep = "-")
            return(re24_scores_game[, columns[3:12]])
        } else {
            scores_game <- filter(re24_scores_game, 
                                  half_inning == dots$half_inning)
            
            if ("pa" %in% names(dots)) {
                cat(unlist(unique(scores_game[, columns[1:5]])), sep = "_")
                return(filter(scores_game, 
                              inning_pa %in% dots$pa)[, columns[6:12]])
            } else {
                cat(unlist(unique(scores_game[, columns[1:5]])), sep = "_")
                return(scores_game[, columns[6:12]])
            }
        }
    } else {
        return("Please input either a player or game ID")
    }
}

# re24_df <- get_re24(re_type = "year", batter = 518692, year = All)

get_linear_weights <- function(re_type, year) {
    type <- as.name(paste0("re_", re_type))
    columns <- c("game_pk", "game_date", "batting_team", "fielding_team", 
                 "home_team", "half_inning", "score", "score_diff", "batter",
                 "pitcher", "play", "events", "rbi", "delta_win_exp", 
                 as.character(type), "re24", "inning_pa", "linear_weight")
    
    linear_weights <- play_df %>%
        filter(game_year %in% year) %>%
        group_by(game_pk, half_inning) %>%
        mutate(re24 = lead(!!type, 
                           default = last(!!type)) - !!type + rbi) %>%
        mutate(re24 = ifelse(re24 == 0, -!!type, re24)) %>%
        ungroup() %>%
        group_by(events) %>%
        mutate(linear_weight = mean(re24), score_diff = bat_score - field_score,
               score = paste0(field_score, "-", bat_score)) %>%
        ungroup() %>%
        mutate(delta_win_exp = ifelse(fielding_team == home_team, 
                                      -delta_home_win_exp, 
                                      delta_home_win_exp)) %>%
        select(-bat_score, -field_score) %>%
        mutate(events = ifelse(events == "intent_walk", "walk", events)) %>%
        filter(!(events %in% c("catcher_interf", "field_error", "wild_pitch",
                               "caught_stealing_2b", "caught_stealing_home",
                               "pickoff_caught_stealing_2b", "stolen_base_home",
                               "pickoff_1b", "pickoff_2b", "caught_stealing_3b",
                               "pickoff_3b", "passed_ball", "pickoff_error_2b",
                               "pickoff_caught_stealing_3b", "pickoff_error_3b",
                               "pickoff_caught_stealing_home", "intent_walk",
                               "other_out"))) %>%
        mutate(events = ifelse(events %in% c(
            "grounded_into_double_play", "runner_double_play",
            "sac_bunt_double_play", "sac_fly_double_play", 
            "strikeout_double_play"), "double_play", events)) %>%
    return(linear_weights[, columns])
}

linear_weights_df <- get_linear_weights(re_type = "home", year = All)
# View(linear_weights_df)

play_delta_win_exp_df <- linear_weights_df %>%
    mutate(score_diff = case_when(score_diff >= 5 ~ "5 plus", 
                                  score_diff <= -5 ~ "-5 minus", 
                                  TRUE ~ as.character(score_diff))) %>%
    group_by(score_diff, play, events) %>%
    summarize(mean_delta_win_exp = round(mean(delta_win_exp), 3), 
              .groups = "drop") %>%
    arrange(play, events)
# View(play_delta_win_exp_df)

missing_situations_df <- play_delta_win_exp_df %>% 
    mutate(score_diff_num = case_when(score_diff == "5 plus" ~ "5", 
                                      score_diff == "-5 minus" ~ "-5", 
                                      TRUE ~ score_diff)) %>%
    mutate(score_diff_num = as.numeric(score_diff_num)) %>%
    group_by(play, events) %>% 
    complete(score_diff_num = -5:5) %>%
    mutate(score_diff = ifelse(score_diff_num %in% -4:4, score_diff_num, 
                               score_diff)) %>%
    mutate(score_diff = case_when(
        as.character(score_diff_num) == "5" ~ "5 plus", 
        as.character(score_diff_num) == "-5" ~ "-5 minus", 
        TRUE ~ score_diff)) %>%
    mutate(score_diff_next = lead(score_diff_num), mean_delta_win_exp = ifelse(
               play == "0:12_" & events == "fielders_choice_out" &
               score_diff_num == 0, -0.01, mean_delta_win_exp)) %>%
    filter(sum(!(is.na(mean_delta_win_exp))) > 2)
# View(missing_situations_df)

no_baseouts_df <- missing_situations_df %>%
    select(-score_diff) %>%
    group_by(play, events) %>%
    filter(!any(is.na(mean_delta_win_exp))) %>%
    mutate(delta_mdwe = lead(mean_delta_win_exp) - mean_delta_win_exp) %>%
    filter(score_diff_num != 5) %>%
    mutate(delta_score = paste0(score_diff_num, "->", score_diff_next)) %>%
    select(-mean_delta_win_exp, -score_diff_num, -score_diff_next) %>% 
    group_by(events, delta_score) %>%
    summarize(delta_mdwe_no_baseouts = round(mean(delta_mdwe), 3), 
              .groups = "drop") %>%
    separate(delta_score, into = c("score_diff_num", "score_diff_next"), 
             sep = "->", convert = TRUE) %>%
    arrange(events, score_diff_num)
# View(no_baseouts_df)

no_score_df <- missing_situations_df %>%
    select(-score_diff_num, -score_diff, -score_diff_next) %>%
    group_by(play, events) %>%
    summarize(mdwe_no_score = round(
        mean(abs(mean_delta_win_exp), na.rm = TRUE), 3), .groups = "drop") %>%
    group_by(events) %>%
    mutate(mdwe_no_score = mdwe_no_score / mean(mdwe_no_score)) %>%
    filter(events != "triple_play")
# View(no_score_df)
    
events_score_df <- left_join(missing_situations_df, no_baseouts_df, 
                       by = c("events", "score_diff_num", 
                              "score_diff_next")) %>%
    filter(sum(!(is.na(mean_delta_win_exp))) > 2, events != "triple_play") %>%
    select(-score_diff_next)
all_situations_df <- left_join(events_score_df, no_score_df, 
                               by = c("play", "events")) %>%
    mutate(delta_mdwe = round(delta_mdwe_no_baseouts * mdwe_no_score, 3)) %>%
    select(-delta_mdwe_no_baseouts, -mdwe_no_score) %>%
    group_by(play, events) %>%
    mutate(na_group = cumsum(!is.na(mean_delta_win_exp)),
           base = ave(mean_delta_win_exp, na_group, FUN = \(x) x[!is.na(x)][1]),
           cumulative = ave(replace_na(delta_mdwe, 0), na_group, 
                            FUN = cumsum)) %>%
    group_by(play, events, na_group) %>%
    mutate(cumulative = ifelse(na_group == 0, -rev(cumsum(rev(delta_mdwe))), 
                               cumulative)) %>%
    ungroup() %>%
    group_by(play, events) %>%
    mutate(base = ifelse(na_group == 0, base[na_group == 1][1], base),
           out = base + cumulative,
           mean_delta_win_exp = ifelse(is.na(mean_delta_win_exp),
                                       ifelse(na_group != 0, lag(out), out),
                                       mean_delta_win_exp)) %>%
    select(-delta_mdwe, -na_group, -base, -cumulative, -out)
select(all_situations_df, -score_diff_num) %>% View()

triple_play_df <- missing_situations_df %>%
    filter(events == "triple_play") %>%
    select(-score_diff_next)
fixed_triple_play_df <- triple_play_df
for (i in unique(fixed_triple_play_df$play)) {
    df <- filter(fixed_triple_play_df, play == i)
    model <- lm(mean_delta_win_exp ~ I(score_diff_num ^ 2), data = df, 
                na.action = na.exclude)
    pred <- predict(model, newdata = df)
    fixed_triple_play_df <- mutate(fixed_triple_play_df, 
                                   mean_delta_win_exp = round(ifelse(
        play == i, ifelse(is.na(mean_delta_win_exp), pred, mean_delta_win_exp),
        mean_delta_win_exp), 3)) %>%
        mutate(mean_delta_win_exp = ifelse(
            mean_delta_win_exp > 0, -mean_delta_win_exp, mean_delta_win_exp))
}
delta_win_exp_df <- rbind(all_situations_df, fixed_triple_play_df) %>%
    ungroup()
# View(delta_win_exp_df)

mean_we_swing_df <- linear_weights_df %>%
    group_by(game_year) %>%
    summarize(mean_we_swing = mean(abs(delta_win_exp)), .groups = "drop") %>%
    mutate(standardized_mean_we_swing = mean_we_swing + 1 - mean(mean_we_swing))
mean_we_swing <- mean(mean_we_swing_df$mean_we_swing)

no_runners_baseouts <- c("0:___", "1:___", "2:___")
no_runners_events <- c("single", "double", "triple", "home_run", "walk", 
                       "hit_by_pitch", "strikeout", "field_out")
no_runners_event_rate_df <- linear_weights_df %>%
    filter(play %in% no_runners_baseouts) %>%
    group_by(events) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(event_rate = round(n / sum(n), 4)) %>%
    select(-n)
no_runners_li_df <- left_join(delta_win_exp_df, no_runners_event_rate_df, 
                              by = "events") %>%
    filter(play %in% no_runners_baseouts, events %in% no_runners_events) %>%
    group_by(play, score_diff_num, score_diff) %>%
    summarize(li = round(((abs(mean_delta_win_exp) %*% event_rate) / 
                          mean_we_swing)[[1]], 3), .groups = "drop")

fc_2_outs_baseouts <- c("2:_2_", "2:__3", "2:_23")
fc_2_outs_events <- c(no_runners_events, "fielders_choice",
                      "fielders_choice_out")
fc_2_outs_event_rate_df <- linear_weights_df %>%
    filter(play %in% fc_2_outs_baseouts) %>%
    group_by(events) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(event_rate = round(n / sum(n), 4)) %>%
    select(-n)
fc_2_outs_li_df <- left_join(delta_win_exp_df, fc_2_outs_event_rate_df, 
                             by = "events") %>%
    filter(play %in% fc_2_outs_baseouts, events %in% fc_2_outs_events) %>%
    group_by(play, score_diff_num, score_diff) %>%
    summarize(li = round(((abs(mean_delta_win_exp) %*% event_rate) / 
                          mean_we_swing)[[1]], 3), .groups = "drop")

force_2_outs_baseouts <- c("2:1__", "2:12_", "2:1_3", "2:123")
force_2_outs_events <- c(no_runners_events, "force_out", "fielders_choice",
                         "fielders_choice_out")
force_2_outs_event_rate_df <- linear_weights_df %>%
    filter(play %in% force_2_outs_baseouts) %>%
    group_by(events) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(event_rate = round(n / sum(n), 4)) %>%
    select(-n)
force_2_outs_li_df <- left_join(delta_win_exp_df, force_2_outs_event_rate_df,
                                by = "events") %>%
    filter(play %in% force_2_outs_baseouts, events %in% force_2_outs_events) %>%
    group_by(play, score_diff_num, score_diff) %>%
    summarize(li = round(((abs(mean_delta_win_exp) %*% event_rate) / 
                          mean_we_swing)[[1]], 3), .groups = "drop")

force_no_sf_baseouts <- c("0:1__", "1:1__", "1:12_")
force_no_sf_events <- c(force_2_outs_events, "double_play", "sac_bunt")
force_no_sf_event_rate_df <- linear_weights_df %>%
    filter(play %in% force_no_sf_baseouts) %>%
    group_by(events) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(event_rate = round(n / sum(n), 4)) %>%
    select(-n)
force_no_sf_li_df <- left_join(delta_win_exp_df, force_no_sf_event_rate_df,
                               by = "events") %>%
    filter(play %in% force_no_sf_baseouts, events %in% force_no_sf_events) %>%
    group_by(play, score_diff_num, score_diff) %>%
    summarize(li = round(((abs(mean_delta_win_exp) %*% event_rate) / 
                          mean_we_swing)[[1]], 3), .groups = "drop")

fc_baseouts <- c("0:_2_", "0:__3", "1:_2_", "1:__3", "1:_23")
fc_events <- c(fc_2_outs_events, "double_play", "sac_bunt", "sac_fly")
fc_event_rate_df <- linear_weights_df %>%
    filter(play %in% fc_baseouts) %>%
    group_by(events) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(event_rate = round(n / sum(n), 4)) %>%
    select(-n)
fc_li_df <- left_join(delta_win_exp_df, fc_event_rate_df, by = "events") %>%
    filter(play %in% fc_baseouts, events %in% fc_events) %>%
    group_by(play, score_diff_num, score_diff) %>%
    summarize(li = round(((abs(mean_delta_win_exp) %*% event_rate) / 
                              mean_we_swing)[[1]], 3), .groups = "drop")

force_sf_baseouts <- c("1:1_3", "1:123")
force_sf_events <- c(force_no_sf_events, "sac_fly")
force_sf_event_rate_df <- linear_weights_df %>%
    filter(play %in% force_sf_baseouts) %>%
    group_by(events) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(event_rate = round(n / sum(n), 4)) %>%
    select(-n)
force_sf_li_df <- left_join(delta_win_exp_df, force_sf_event_rate_df, 
                            by = "events") %>%
    filter(play %in% force_sf_baseouts, events %in% force_sf_events) %>%
    group_by(play, score_diff_num, score_diff) %>%
    summarize(li = round(((abs(mean_delta_win_exp) %*% event_rate) / 
                          mean_we_swing)[[1]], 3), .groups = "drop")

tp_baseouts <- c("0:12_", "0:1_3", "0:_23", "0:123")
tp_events <- c(force_sf_events, "triple_play")
tp_event_rate_df <- linear_weights_df %>%
    filter(play %in% tp_baseouts) %>%
    group_by(events) %>%
    summarize(n = n(), .groups = "drop") %>%
    mutate(event_rate = round(n / sum(n), 4)) %>%
    select(-n)
tp_li_df <- left_join(delta_win_exp_df, tp_event_rate_df, by = "events") %>%
    filter(play %in% tp_baseouts, events %in% tp_events) %>%
    group_by(play, score_diff_num, score_diff) %>%
    summarize(li = round(((abs(mean_delta_win_exp) %*% event_rate) / 
                          mean_we_swing)[[1]], 3), .groups = "drop")

li_df <- rbind(no_runners_li_df, fc_2_outs_li_df, force_2_outs_li_df, 
               force_no_sf_li_df, fc_li_df, force_sf_li_df, tp_li_df) %>%
    arrange(play)
select(li_df, -score_diff_num) %>% View()

all_leads_df <- li_df %>%
    select(-score_diff) %>%
    group_by(play) %>%
    mutate(arg_max = which.max(li) - 6) %>%
    complete(score_diff_num = -26:26) %>%
    mutate(arg_max = ifelse(is.na(arg_max), max(arg_max, na.rm = TRUE), 
                            arg_max), trend = case_when(
        score_diff_num < arg_max ~ "up", score_diff_num == arg_max ~ "maxima",
        score_diff_num > arg_max ~ "down")) %>%
    select(-arg_max) %>%
    mutate(trend = ifelse(trend == "maxima", list(c("up", "down")), trend)) %>%
    unnest(trend) %>%
    group_by(play, trend) %>%
    mutate(model = list({
        g <- pick(everything())
        y <- g$li
        x <- g$score_diff_num
        lm(log(y) ~ x, na.action = na.exclude)})) %>% 
    mutate(pred = round(exp(predict(model[[1]], 
                            newdata = data.frame(x = score_diff_num))), 5), 
           model = ifelse(row_number() != 1, "", model), 
           li = ifelse(is.na(li), ifelse(trend == "down", lead(pred), 
                                         lag(pred)), li)) %>%
    filter(abs(score_diff_num) != 26) %>%
    ungroup() %>%
    mutate(score_diff = score_diff_num) %>%
    distinct(play, score_diff, .keep_all = TRUE) %>%
    select(play, score_diff, li)
View(all_leads_df)

innings_factor <- factor(unique(linear_weights_df$half_inning))
innings_df_1 <- linear_weights_df %>%
    filter(half_inning %in% as.vector(innings_factor)[1:17]) %>%
    group_by(half_inning) %>%
    summarize(inning_mdwe = mean(delta_win_exp), .groups = "drop") %>%
    mutate(half_inning = innings_factor[1:17], 
           inning_mdwe = inning_mdwe + (1 - mean(inning_mdwe)), n = 1:n())
innings_model_1 <- lm(inning_mdwe ~ n, data = innings_df_1)
# summary(innings_model_1)

innings_df_2 <- linear_weights_df %>%
    filter(half_inning %in% as.vector(innings_factor)[17:38]) %>%
    group_by(half_inning) %>%
    summarize(inning_mdwe = mean(delta_win_exp), .groups = "drop") %>%
    mutate(half_inning = innings_factor[17:38], 
           inning_mdwe = inning_mdwe + (1 - mean(inning_mdwe)), n = 1:n())
innings_model_2 <- lm(inning_mdwe ~ n, data = innings_df_2)
# summary(innings_model_2)

bot_9_weight_delta <- innings_model_1$fitted.values[17] - 
                      innings_model_2$fitted.values[1]
corrected_model_2_preds <- unname(innings_model_2$fitted.values[1:22]) +
                           bot_9_weight_delta
inning_weights_df <- c(unname(innings_model_1$fitted.values[1:16]), 
                       corrected_model_2_preds) %>%
    as.data.frame() %>%
    mutate(inning = innings_factor) %>%
    `colnames<-`(c("inning_weight", "half_inning")) %>%
    select(half_inning, inning_weight) %>%
    mutate(inning_weight = 1)
View(inning_weights_df)

complete_li_df <- left_join(linear_weights_df, all_leads_df,
                               by = c("play", "score_diff"))
year_li_df <- left_join(complete_li_df, mean_we_swing_df, by = "game_year") %>%
    select(-mean_we_swing)
leverage_index_df <- left_join(year_li_df, inning_weights_df,
                               by = "half_inning") %>%
    mutate(li = (li * inning_weight) / standardized_mean_we_swing) %>%
    select(game_pk, game_date, batting_team, fielding_team, half_inning, 
           inning_pa, score, score_diff, batter, pitcher, play, re_year, li, 
           events, rbi, re24, delta_win_exp) %>%
    mutate(li = round(li / mean(li), 4))
View(leverage_index_df)

tex_sea_df <- read.csv("tex_sea_09_29_22.csv") %>%  # 661128  93  75
    filter(!(PA %in% c(16, 78, 83, 90, 97)))
cle_sea_df <- read.csv("cle_vs_sea_08_26_25.csv") %>%  # 662143  79  63
    filter(!(row_number() %in% c(7, 69, 76, 77, 79))) %>%
    mutate(inning = paste0(topbot, "_", gsub("\\? ", "", Inn))) %>%
    select(-Inn, -topbot)

compare_li_df <- cle_sea_df %>%
    mutate(my_li = filter(leverage_index_df, game_pk == 662143)$li) %>%
    select(inning, my_li, LI) %>%
    `colnames<-`(c("inning", "my_li", "fangraphs_li")) %>%
    mutate(my_li = round(my_li, 2), li_diff = my_li / fangraphs_li, 
           n = row_number())
trend_compare_li_df <- compare_li_df %>%
    select(-my_li, -fangraphs_li, -n) %>%
    mutate(inning = factor(inning, levels = unique(compare_li_df$inning))) %>%
    group_by(inning) %>%
    summarize(li_diff = mean(li_diff), .groups = "drop") %>%
    mutate(n = row_number())
plot_compare_li_df <- compare_li_df %>%
    select(-inning) %>%
    melt(id = "n")

# tex_sea_model <- lm(li_diff ~ n, data = compare_li_df)
# summary(tex_sea_model)
cle_sea_model <- lm(li_diff ~ n, data = trend_compare_li_df)
summary(cle_sea_model)

ggplot(data = plot_compare_li_df, mapping = aes(x = n, y = value, 
                                                color = variable)) + 
    geom_point() + 
    geom_path() +
    geom_hline(yintercept = 1, colour = "black") +
    geom_vline(xintercept = 63, colour = "red") +
    scale_x_continuous(breaks = 1:nrow(compare_li_df)) +
    scale_y_continuous(breaks = seq(0, 7, length.out = 29))

ggplot(data = trend_compare_li_df, mapping = aes(x = n, y = li_diff)) +
    geom_point() +
    geom_path() +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE, col = "red") +
    geom_hline(yintercept = 1, color = "black") +
    geom_vline(xintercept = mean(1:nrow(trend_compare_li_df)), 
               color = "black") +
    scale_y_continuous(limits = c(0.25, 1.75), breaks = seq(0.25, 1.75, 
                                                            length.out = 7))

ggplot(data = delta_win_exp_df) +
    geom_line(aes(x = score_diff_num, y = mean_delta_win_exp,
                  group = interaction(play, events), color = events)) +
    scale_x_continuous(breaks = -5:5) +
    ylim(min(delta_win_exp_df$mean_delta_win_exp), 
         max(delta_win_exp_df$mean_delta_win_exp))

ggplot(data = all_leads_df) +
    geom_line(aes(x = score_diff, y = li, color = play)) +
    scale_x_continuous(breaks = -25:25) +
    scale_y_continuous(breaks = 0:6)

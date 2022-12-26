library(shiny)
library(scrollytell)
library(plotly)
library(tidyverse)
library(readr)
library(glue)
library(ggrepel)
library(lubridate)
library(naniar)
library(broom)

# loading the datasets:
data_URLs <- paste("https://raw.githubusercontent.com/JeffSackmann/tennis_wta/master/wta_matches_",
                   1968:2021,
                   ".csv", 
                   sep = "")
wtaresults <- read_csv(data_URLs)

data_URLs2 <- paste("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_",
                    1968:2021,
                    ".csv", 
                    sep = "")
atpresults <- read_csv(data_URLs2)

library(tidyverse)
# adding column to prepare to combine datasets
wtaresults <- wtaresults %>%
  mutate(tour = "WTA") 

atpresults <- atpresults %>%
  mutate(tour = "ATP")

# moving tour column to front for ease
wtaresults <- wtaresults[,c(50,1:49)]
atpresults <- atpresults[,c(50,1:49)]

# combining the datasets
tennis_results <- rbind(wtaresults, atpresults)

# making date objects from date
library(lubridate)
tennis_results <- tennis_results %>%
  mutate(tourney_date = ymd(tourney_date)) %>%
  mutate(year = year(tourney_date)) %>%
  mutate(month = month(tourney_date))

# reorganizing date columns together
tennis_results <- tennis_results[,c(1:7,51:52,8:50)]

## Data Cleaning

# making the alternate abbreviation consistent
tennis_results$winner_entry[tennis_results$winner_entry=="Alt"] <- "ALT"

# ensuring that `winner_seed` is of numeric type instead of character
tennis_results <- tennis_results %>% 
  mutate(winner_seed = as.integer(winner_seed)) %>%
  mutate(loser_seed = as.integer(loser_seed))

# fixing `loser_entry` typos
tennis_results <- tennis_results %>% 
  mutate(loser_entry = case_when(
    loser_entry == 'A' ~ 'ALT',
    loser_entry == 'Alt' ~ 'ALT',
    loser_entry == 'wc' ~ 'WC',
    loser_entry == 'S' ~ 'SE',
    TRUE ~ loser_entry
  ))

# cleaning datasets with duplicated information
clean_1973_surbiton <- tennis_results %>%
  filter(tourney_id == '1973-1098',
         match_num != 32,
         match_num != 33)

clean_1981_johannesburg <- tennis_results %>% 
  filter(tourney_id == '1981-1099') %>%
  slice_tail(n = 11)

clean_1990_taranto <- tennis_results %>% 
  filter(tourney_id == '1990-W-WT-ITA-01A-1990',
         !(match_num == 29 & round == 'R32'),
         !(match_num == 30 & round == 'R32'),
         !(match_num == 31 & round == 'R32'),
         match_num <= 31)

clean_1991_stpetersburg <- tennis_results %>% 
  filter(tourney_id == '1991-W-WT-URS-01A-1991',
         !(match_num == 29 & round == 'R32'),
         !(match_num == 30 & round == 'R32'),
         !(match_num == 31 & round == 'R32'),
         match_num <= 31)

clean_1991_oakland <- tennis_results %>% 
  filter(tourney_id == '1991-W-WT-USA-19A-1991') %>%
  slice_head(n = 27)

clean_1992_oklahoma <- tennis_results %>% 
  filter(tourney_id == '1992-W-WT-USA-02A-1992',
         !(match_num == 28 & round == 'R32'),
         !(match_num == 29 & round == 'R32'),
         !(match_num == 30 & round == 'R32'),
         !(match_num == 31 & round == 'R32'),
         match_num <= 31)

# vector of RR tourneys or tourneys with duplicated information
duplicated_tourneys <- c('1973-1098', '1970-9205', '1981-1099',
                         '1990-W-WT-ITA-01A-1990', '1991-W-WT-URS-01A-1991', 
                         '1991-W-WT-USA-19A-1991', '1992-W-WT-USA-02A-1992')

tennis_results <- tennis_results %>% 
  filter(!(str_detect(tourney_id, "-615") | str_detect(tourney_id, "-8888")), 
         !(tourney_id %in% duplicated_tourneys)) %>%
  rbind(clean_1973_surbiton, clean_1981_johannesburg, 
        clean_1990_taranto, clean_1991_stpetersburg, 
        clean_1991_oakland, clean_1992_oklahoma)

# replacing mistaken entries
tennis_results[26765,13] = NA
tennis_results[26765,12] = 6
tennis_results[43756,12] = 9
# from looking at the original draw
# https://wtafiles.blob.core.windows.net/pdf/draws/archive/1983/702.pdf
# it can be deduced that they meant seed 9 instead of seed 96

# removing junior, challenger, exho results
# they are not the main pro tour
tennis_results <- tennis_results %>%
  filter(tourney_level != "J" & tourney_level != "CC" & tourney_level != "E")

# standardizing heights to cm
tennis_results <- tennis_results %>%
  mutate(winner_ht = ifelse(winner_ht < 100, winner_ht * 100, winner_ht),
         loser_ht = ifelse(loser_ht < 100, loser_ht * 100, loser_ht))

# adding a column of overall tournament winners for each match
winners <- tennis_results %>%
  filter(round == 'F') %>%
  mutate(tourney_winner = winner_name) %>%
  select(tour:month, tourney_winner)
tennis_results <- tennis_results %>%
  left_join(winners)

rank_point_results <- tennis_results %>%
  filter(round == "F",
         tourney_level == "G") %>%
  mutate(unseeded_winner = ifelse(winner_rank > 33, "Y", "N"))


# CLUSTER ANALYSIS (in one not-very-readable code chunk)

tennis_results <- tennis_results %>%
  replace_with_na(replace = list(w_SvGms = 0, l_SvGms = 0, minutes = 0))

playerstyle_WTA_cluster <- tennis_results %>%
  filter(year >= 2011, tour == "WTA", !is.na(w_1stIn), !is.na(w_svpt), !is.na(l_1stIn), !is.na(l_svpt), !is.na(w_1stWon), !is.na(w_2ndWon), !is.na(l_1stWon), !is.na(l_2ndWon), !is.na(w_ace), !is.na(l_ace), !is.na(w_df), !is.na(l_df), !is.na(w_SvGms), !is.na(l_SvGms), !is.na(w_bpSaved), !is.na(w_bpFaced), !is.na(l_bpSaved), !is.na(l_bpFaced), !is.na(minutes)) %>%
  mutate(w_1stsvpct = w_1stIn/w_svpt, 
         l_1stsvpct = l_1stIn/l_svpt,
         w_svpctWon = (w_1stWon+w_2ndWon)/w_svpt,
         l_svpctWon = (l_1stWon+l_2ndWon)/l_svpt,
         w_1stsvWon = w_1stWon/w_1stIn,
         l_1stsvWon = l_1stWon/l_1stIn,
         w_2ndsvWon = w_2ndWon/(w_svpt-w_1stIn),
         l_2ndsvWon = l_2ndWon/(l_svpt-l_1stIn),
         w_acepct = w_ace/w_svpt,
         l_acepct = l_ace/l_svpt,
         w_dfpct = w_df/w_svpt,
         l_dfpct = l_df/l_svpt,
         w_ptspersvgame = w_svpt/w_SvGms,
         l_ptspersvgame = l_svpt/l_SvGms,
         w_bpSavepct = w_bpSaved/w_bpFaced,
         l_bpSavepct = l_bpSaved/l_bpFaced,
         w_bppersvgame = w_bpFaced/w_SvGms,
         l_bppersvgame = l_bpFaced/l_SvGms,
         w_pctptWon = (w_1stWon+w_2ndWon+(l_svpt-l_1stWon-l_2ndWon))/(w_svpt+l_svpt),
         l_pctptWon = (l_1stWon+l_2ndWon+(w_svpt-w_1stWon-w_2ndWon))/(w_svpt+l_svpt),
         w_1stretWon = (l_1stIn-l_1stWon)/l_1stIn,
         l_1stretWon = (w_1stIn-w_1stWon)/w_1stIn,
         w_2ndretWon = (l_svpt-l_1stIn-l_2ndWon)/(l_svpt-l_1stIn),
         l_2ndretWon = (w_svpt-w_1stIn-w_2ndWon)/(w_svpt-w_1stIn),
         w_retpctWon = 1-(l_1stWon+l_2ndWon)/l_svpt,
         l_retpctWon = 1-(w_1stWon+w_2ndWon)/w_svpt,
         w_ptsperretgame = l_svpt/l_SvGms,
         l_ptsperretgame = w_svpt/w_SvGms,
         w_bpConvpct = 1-l_bpSaved/l_bpFaced,
         l_bpConvpct = 1-w_bpSaved/w_bpFaced,
         w_bpperretgame = l_bpFaced/l_SvGms,
         l_bpperretgame = w_bpFaced/w_SvGms,
         w_retace = l_ace/l_svpt,
         l_retace = w_ace/w_svpt,
         w_retdf = l_df/l_svpt,
         l_retdf = w_df/w_svpt,
         ptspermin = (w_svpt+l_svpt)/minutes)

cluster_w <- playerstyle_WTA_cluster %>%
  select(tour, tourney_id, tourney_name, surface, tourney_date, year, month, winner_id, winner_name, winner_ht, winner_age, score, best_of, minutes, winner_rank, winner_rank_points, w_1stsvpct, w_svpctWon, w_1stsvWon, w_2ndsvWon, w_acepct, w_dfpct, w_ptspersvgame, w_bpSavepct, w_bppersvgame, w_pctptWon, w_1stretWon, w_2ndretWon, w_retpctWon, w_ptsperretgame, w_bpConvpct, w_bpperretgame, w_retace, w_retdf, ptspermin) %>%
  mutate(result = 1)

colnames(cluster_w) <- c("tour", "tourney_id", "tourney_name", "surface", "tourney_date", "year", "month", "id", "name", "height", "age", "score", "best_of", "minutes", "rank", "rank_points", "1stsvpct", "svpctWon", "1stsvWon", "2ndsvWon", "acepct", "dfpct", "ptspersvgame", "bpSavepct", "bppersvgame", "pctptWon", "1stretWon", "2ndretWon", "retpctWon", "ptsperretgame", "bpConvpct", "bpperretgame", "retace", "retdf", "ptspermin", "result")

cluster_l <- playerstyle_WTA_cluster %>%
  select(tour, tourney_id, tourney_name, surface, tourney_date, year, month, loser_id, loser_name, loser_ht, loser_age, score, best_of, minutes, loser_rank, loser_rank_points, l_1stsvpct, l_svpctWon, l_1stsvWon, l_2ndsvWon, l_acepct, l_dfpct, l_ptspersvgame, l_bpSavepct, l_bppersvgame, l_pctptWon, l_1stretWon, l_2ndretWon, l_retpctWon, l_ptsperretgame, l_bpConvpct, l_bpperretgame, l_retace, l_retdf, ptspermin) %>%
  mutate(result = 0)

colnames(cluster_l) <- c("tour", "tourney_id", "tourney_name", "surface", "tourney_date", "year", "month", "id", "name", "height", "age", "score", "best_of", "minutes", "rank", "rank_points", "1stsvpct", "svpctWon", "1stsvWon", "2ndsvWon", "acepct", "dfpct", "ptspersvgame", "bpSavepct", "bppersvgame", "pctptWon", "1stretWon", "2ndretWon", "retpctWon", "ptsperretgame", "bpConvpct", "bpperretgame", "retace", "retdf", "ptspermin", "result")

final_cluster_data <- rbind(cluster_w,cluster_l)

WTA_player_stats <- final_cluster_data %>%
  group_by(id, name) %>%
  summarize(height = mean(height), 
            age = max(age), 
            win_perc = mean(result), 
            perc_points_won = mean(pctptWon, na.rm = TRUE), 
            "1st_serv_perc" = mean(`1stsvpct`, na.rm = TRUE), 
            "1st_win" = mean(`1stsvWon`, na.rm = TRUE), 
            ace_perc = mean(acepct, na.rm = TRUE), 
            df_perc = mean(dfpct, na.rm = TRUE), 
            "2nd_win" = mean(`2ndsvWon`, na.rm = TRUE), 
            svc_perc_win = mean(svpctWon, na.rm = TRUE), 
            points_per_svc_game = mean(ptspersvgame, na.rm = TRUE), 
            break_point_save_perc = mean(bpSavepct, na.rm = TRUE), 
            bp_per_game = mean(bppersvgame, na.rm = TRUE), 
            return_1st_win = mean(`1stretWon`, na.rm = TRUE), 
            return_ace_perc = mean(retace, na.rm = TRUE), 
            return_df_perc = mean(retdf, na.rm = TRUE), 
            return_2nd_win = mean(`2ndretWon`, na.rm = TRUE), 
            return_perc_win = mean(retpctWon, na.rm = TRUE), 
            points_per_return_game = mean(ptsperretgame, na.rm = TRUE), 
            bp_convert_perc = mean(bpConvpct, na.rm = TRUE), 
            return_bp_per_game = mean(bpperretgame, na.rm = TRUE), 
            points_per_minute = mean(ptspermin, na.rm = TRUE))

surface_stats <- final_cluster_data %>%
  group_by(id, name, surface) %>%
  summarize(count = n()) %>%
  mutate(freq = count / sum(count)) %>%
  pivot_wider(id_cols = c(id, name), names_from = surface, values_from = freq) %>%
  summarize(clay_perc = Clay, grass_perc = Grass, hard_perc = Hard) %>%
  select(-2)

final_WTA <- cbind(surface_stats, WTA_player_stats) %>%
  select(-1) %>%
  rename(id = id...5) %>%
  select(4:7, 1:3, 8:27)

final_WTA_km <- final_WTA %>%
  drop_na() %>%
  select(height:points_per_minute) %>%
  mutate(across(height:points_per_minute, scale))

set.seed(13)
final_WTA_kclusts <- 
  tibble(k = 1:9) %>%
  mutate(final_WTA_kclust = map(k, ~kmeans(final_WTA_km, .x)),
         glanced = map(final_WTA_kclust, glance),
         tidied = map(final_WTA_kclust, tidy),
         augmented = map(final_WTA_kclust, augment, final_WTA_km)
  )

clusters <- 
  final_WTA_kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  final_WTA_kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  final_WTA_kclusts %>%
  unnest(cols = c(glanced))

set.seed(47)
WTA_clustered <- final_WTA_km %>%
  kmeans(centers = 4)

WTA_clusters <- cbind(WTA_clustered$cluster, final_WTA %>% drop_na())

WTA_clusters %>%
  rename(cluster = `WTA_clustered$cluster`) %>%
  group_by(cluster) %>%
  select(cluster, height:points_per_minute) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  mutate(cluster = as.factor(cluster)) %>%
  mutate(cluster = recode(cluster, "1" = "between tours", "2" = "top players", "3" = "strong servers (ATP)/strong returners (WTA)", "4" = "tier 2"))

tennis_results <- tennis_results %>%
  replace_with_na(replace = list(w_SvGms = 0, l_SvGms = 0, minutes = 0))

# selecting matches from 2011 onward
# creating the statistics that they mentioned in the table
playerstyle_ATP_cluster <- tennis_results %>%
  filter(year >= 2011, tour == "ATP", !is.na(w_1stIn), !is.na(w_svpt), !is.na(l_1stIn), !is.na(l_svpt), !is.na(w_1stWon), !is.na(w_2ndWon), !is.na(l_1stWon), !is.na(l_2ndWon), !is.na(w_ace), !is.na(l_ace), !is.na(w_df), !is.na(l_df), !is.na(w_SvGms), !is.na(l_SvGms), !is.na(w_bpSaved), !is.na(w_bpFaced), !is.na(l_bpSaved), !is.na(l_bpFaced), !is.na(minutes)) %>%
  mutate(w_1stsvpct = w_1stIn/w_svpt, 
         l_1stsvpct = l_1stIn/l_svpt,
         w_svpctWon = (w_1stWon+w_2ndWon)/w_svpt,
         l_svpctWon = (l_1stWon+l_2ndWon)/l_svpt,
         w_1stsvWon = w_1stWon/w_1stIn,
         l_1stsvWon = l_1stWon/l_1stIn,
         w_2ndsvWon = w_2ndWon/(w_svpt-w_1stIn),
         l_2ndsvWon = l_2ndWon/(l_svpt-l_1stIn),
         w_acepct = w_ace/w_svpt,
         l_acepct = l_ace/l_svpt,
         w_dfpct = w_df/w_svpt,
         l_dfpct = l_df/l_svpt,
         w_ptspersvgame = w_svpt/w_SvGms,
         l_ptspersvgame = l_svpt/l_SvGms,
         w_bpSavepct = w_bpSaved/w_bpFaced,
         l_bpSavepct = l_bpSaved/l_bpFaced,
         w_bppersvgame = w_bpFaced/w_SvGms,
         l_bppersvgame = l_bpFaced/l_SvGms,
         w_pctptWon = (w_1stWon+w_2ndWon+(l_svpt-l_1stWon-l_2ndWon))/(w_svpt+l_svpt),
         l_pctptWon = (l_1stWon+l_2ndWon+(w_svpt-w_1stWon-w_2ndWon))/(w_svpt+l_svpt),
         w_1stretWon = (l_1stIn-l_1stWon)/l_1stIn,
         l_1stretWon = (w_1stIn-w_1stWon)/w_1stIn,
         w_2ndretWon = (l_svpt-l_1stIn-l_2ndWon)/(l_svpt-l_1stIn),
         l_2ndretWon = (w_svpt-w_1stIn-w_2ndWon)/(w_svpt-w_1stIn),
         w_retpctWon = 1-(l_1stWon+l_2ndWon)/l_svpt,
         l_retpctWon = 1-(w_1stWon+w_2ndWon)/w_svpt,
         w_ptsperretgame = l_svpt/l_SvGms,
         l_ptsperretgame = w_svpt/w_SvGms,
         w_bpConvpct = 1-l_bpSaved/l_bpFaced,
         l_bpConvpct = 1-w_bpSaved/w_bpFaced,
         w_bpperretgame = l_bpFaced/l_SvGms,
         l_bpperretgame = w_bpFaced/w_SvGms,
         w_retace = l_ace/l_svpt,
         l_retace = w_ace/w_svpt,
         w_retdf = l_df/l_svpt,
         l_retdf = w_df/w_svpt,
         ptspermin = (w_svpt+l_svpt)/minutes)

cluster_w_m <- playerstyle_ATP_cluster %>%
  select(tour, tourney_id, tourney_name, surface, tourney_date, year, month, winner_id, winner_name, winner_ht, winner_age, score, best_of, minutes, winner_rank, winner_rank_points, w_1stsvpct, w_svpctWon, w_1stsvWon, w_2ndsvWon, w_acepct, w_dfpct, w_ptspersvgame, w_bpSavepct, w_bppersvgame, w_pctptWon, w_1stretWon, w_2ndretWon, w_retpctWon, w_ptsperretgame, w_bpConvpct, w_bpperretgame, w_retace, w_retdf, ptspermin) %>%
  mutate(result = 1)

colnames(cluster_w_m) <- c("tour", "tourney_id", "tourney_name", "surface", "tourney_date", "year", "month", "id", "name", "height", "age", "score", "best_of", "minutes", "rank", "rank_points", "1stsvpct", "svpctWon", "1stsvWon", "2ndsvWon", "acepct", "dfpct", "ptspersvgame", "bpSavepct", "bppersvgame", "pctptWon", "1stretWon", "2ndretWon", "retpctWon", "ptsperretgame", "bpConvpct", "bpperretgame", "retace", "retdf", "ptspermin", "result")

cluster_l_m <- playerstyle_ATP_cluster %>%
  select(tour, tourney_id, tourney_name, surface, tourney_date, year, month, loser_id, loser_name, loser_ht, loser_age, score, best_of, minutes, loser_rank, loser_rank_points, l_1stsvpct, l_svpctWon, l_1stsvWon, l_2ndsvWon, l_acepct, l_dfpct, l_ptspersvgame, l_bpSavepct, l_bppersvgame, l_pctptWon, l_1stretWon, l_2ndretWon, l_retpctWon, l_ptsperretgame, l_bpConvpct, l_bpperretgame, l_retace, l_retdf, ptspermin) %>%
  mutate(result = 0)

colnames(cluster_l_m) <- c("tour", "tourney_id", "tourney_name", "surface", "tourney_date", "year", "month", "id", "name", "height", "age", "score", "best_of", "minutes", "rank", "rank_points", "1stsvpct", "svpctWon", "1stsvWon", "2ndsvWon", "acepct", "dfpct", "ptspersvgame", "bpSavepct", "bppersvgame", "pctptWon", "1stretWon", "2ndretWon", "retpctWon", "ptsperretgame", "bpConvpct", "bpperretgame", "retace", "retdf", "ptspermin", "result")

final_cluster_data_m <- rbind(cluster_w_m,cluster_l_m)

ATP_player_stats <- final_cluster_data_m %>%
  group_by(id, name) %>%
  summarize(height = mean(height), age = max(age), win_perc = mean(result), perc_points_won = mean(pctptWon, na.rm = TRUE), "1st_serv_perc" = mean(`1stsvpct`, na.rm = TRUE), "1st_win" = mean(`1stsvWon`, na.rm = TRUE), ace_perc = mean(acepct, na.rm = TRUE), df_perc = mean(dfpct, na.rm = TRUE), "2nd_win" = mean(`2ndsvWon`, na.rm = TRUE), svc_perc_win = mean(svpctWon, na.rm = TRUE), points_per_svc_game = mean(ptspersvgame, na.rm = TRUE), break_point_save_perc = mean(bpSavepct, na.rm = TRUE), bp_per_game = mean(bppersvgame, na.rm = TRUE), return_1st_win = mean(`1stretWon`, na.rm = TRUE), return_ace_perc = mean(retace, na.rm = TRUE), return_df_perc = mean(retdf, na.rm = TRUE), return_2nd_win = mean(`2ndretWon`, na.rm = TRUE), return_perc_win = mean(retpctWon, na.rm = TRUE), points_per_return_game = mean(ptsperretgame, na.rm = TRUE), bp_convert_perc = mean(bpConvpct, na.rm = TRUE), return_bp_per_game = mean(bpperretgame, na.rm = TRUE), points_per_minute = mean(ptspermin, na.rm = TRUE))

surface_stats_m <- final_cluster_data_m %>%
  group_by(id, name, surface) %>%
  summarize(count = n()) %>%
  mutate(freq = count / sum(count)) %>%
  pivot_wider(id_cols = c(id, name), names_from = surface, values_from = freq) %>%
  summarize(clay_perc = Clay, grass_perc = Grass, hard_perc = Hard) %>%
  select(-2)

final_ATP <- cbind(surface_stats_m, ATP_player_stats) %>%
  select(-1) %>%
  rename(id = id...5) %>%
  select(4:7, 1:3, 8:27)

final_ATP_km <- final_ATP %>%
  drop_na() %>%
  select(height:points_per_minute) %>%
  mutate(across(height:points_per_minute, scale))

set.seed(7)
final_ATP_kclusts <- 
  tibble(k = 1:9) %>%
  mutate(final_ATP_kclust = map(k, ~kmeans(final_ATP_km, .x)),
         glanced = map(final_ATP_kclust, glance),
         tidied = map(final_ATP_kclust, tidy),
         augmented = map(final_ATP_kclust, augment, final_ATP_km)
  )

clusters_m <- 
  final_ATP_kclusts %>%
  unnest(cols = c(tidied))

assignments_m <- 
  final_ATP_kclusts %>% 
  unnest(cols = c(augmented))

clusterings_m <- 
  final_ATP_kclusts %>%
  unnest(cols = c(glanced))

set.seed(47)
ATP_clustered <- final_ATP_km %>%
  kmeans(centers = 4) # use 4 to compare to WTA

ATP_clusters <- cbind(ATP_clustered$cluster, final_ATP %>% drop_na())

ATP_clusters %>%
  rename(cluster = `ATP_clustered$cluster`) %>%
  group_by(cluster) %>%
  select(cluster, height:points_per_minute) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  mutate(cluster = as.factor(cluster)) %>%
  mutate(cluster = recode(cluster, "1" = "tier 2", "2" = "top players", "3" = "between tours", "4" = "strong servers (ATP)/strong returners (WTA)"))

######
diff_rank_func <- function(.x){
  rank_point_results %>%
    filter(!is.na(winner_rank)) %>%
    mutate(permrank = sample(winner_rank, replace = FALSE)) %>%
    group_by(tour) %>%
    summarize(avg_permrank = mean(permrank), 
              avg_rank = mean(winner_rank)) %>%
    summarize(diff_permrank = diff(avg_permrank),
              diff_rank = diff(avg_rank))
}


set.seed(47)
perm_diff_rank <- map_df(1:1000, diff_rank_func)


rank_point_results <- rank_point_results %>%
  mutate(reveal = case_when(
    tour == "WTA" ~ 1,
    tour == "WTA" ~ 2,
    tour == "ATP" ~ 3,
    tour == "ATP" ~ 4
  ))

diff_rank_func <- function(.x){
  rank_point_results %>%
    filter(!is.na(winner_rank)) %>%
    mutate(permrank = sample(winner_rank, replace = FALSE)) %>%
    group_by(tour) %>%
    summarize(avg_permrank = mean(permrank), 
              avg_rank = mean(winner_rank)) %>%
    summarize(diff_permrank = diff(avg_permrank),
              diff_rank = diff(avg_rank))
}

set.seed(47)
perm_diff_rank <- map_df(1:1000, diff_rank_func)

ATP_cluster_results <- ATP_clusters %>%
  rename(cluster = `ATP_clustered$cluster`) %>%
  group_by(cluster) %>%
  select(cluster, height:points_per_minute) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  mutate(tour = "ATP",
         cluster = as.factor(cluster)) %>%
  mutate(cluster = recode(cluster, "1" = "tier 2", "2" = "top players", 
                          "3" = "between tours", 
                          "4" = "strong servers (ATP)/\nstrong returners (WTA)")) %>%
  select(27, 1:26)

WTA_cluster_results <- WTA_clusters %>%
  rename(cluster = `WTA_clustered$cluster`) %>%
  group_by(cluster) %>%
  select(cluster, height:points_per_minute) %>%
  summarise_each(funs(mean(., na.rm = TRUE))) %>%
  mutate(tour = "WTA",
         cluster = as.factor(cluster)) %>%
  mutate(cluster = recode(cluster, "1" = "between tours", "2" = "top players", 
                          "3" = "strong servers (ATP)/\nstrong returners (WTA)", 
                          "4" = "tier 2")) %>%
  select(27, 1:26)

all_cluster_results <- rbind(ATP_cluster_results, WTA_cluster_results)
######

#text on left, in HTML format
text0 <- HTML("<p><b>Emma Raducanu</b> (pictured) was a qualifier to the 2021 US Open, 
              who had never previously won a tour level match prior to one of the biggest tennis tournaments in the world. 
              Yet inexplicably, she had a 'fairytale' run to the title.</p>
              <p>Since we began following tennis in the early 2010s, 
              the mainstream narrative has told us that women's tennis is more inconsistent than men's tennis, 
              and this seemingly astonishing result was strong evidence for that claim.
              The Grand Slams were supposed to be reserved for the highest level of tennis by the best players in the world! 
              <i>How did this unknown teenager storm through the draw without dropping a single set?</i></p>
              <p>In this Shiny app, while we do not examine the specific factors that led to Raducanu's title run,
              we take a look at the broader picture of this concept of 'consistency' 
              in the men's (ATP) and women's (WTA) professional tennis tours.
              Specifically, we investigated some of the more public-facing aspects of tennis to delve deeper into the question: 
              <u>What influences the public perception of the WTA's '(in)consistency' relative to the ATP's 'consistency?'</u></p>
              <p><b>First</b>, we looked at <u>top player performance</u>, 
              which we explored by looking at the rank of Grand Slam winners.
              <br><b>Second</b>, we analyzed the <u>playing styles</u> of WTA and ATP tennis players.
              <br><b>Finally</b>, we examined <u>ranking trends</u>, 
              as motivated by the concept of the 'Big 4' in men's tennis.</p>")

text1 <- HTML("<H2>1. Top Player Performance: Looking at Rank of Grand Slam Winners</H2>
              The four Grand Slams (GS) are the biggest tennis tournaments in the world: 
              Australian Open, French Open, Wimbledon, and the US Open.
              Players dream about participating in and winning a Grand Slam.
              The tour calendar is structured around preparing for these tournaments.")

text2 <- HTML("<p>Usually, only the top-ranked players are considered as serious contenders to win a Grand Slam.
              However, in recent years, there have been several memorable 'surprise' winners on the WTA tour. 
              Jelena Ostapenko and Iga Swiatek won their first-ever tournaments at the French Open.
              In addition to the aforementioned Raducanu, 
              Sloane Stephens won the US Open one month into her comeback from injury. 
              (Just weeks before, she was ranked outside of the top 900 in the world!)</p>")

text3 <- HTML("Visually, we can immediately see that the ATP and WTA have a comparable number of outlier Grand Slam winners.
              However, it's interesting to note that all the outliers (marked in blue) for the ATP come before 2005,
              while all the outliers for the WTA come after 2005.")

text4 <- HTML("This is an important transition period for professional tennis that we explore later.")

text5 <- HTML("<H3>Is the average rank of GS winners significantly different across tours?</H3>
              <p>If we hypothesize that there is no difference in the average rank of Grand Slam winners between the WTA and ATP tours, 
              then switching around the rankings of the winners shouldn't significantly change this difference. 
              If we repeat this process 1000 times (permuting the ranking labels and calculating the difference),
              then we can get a sense where the actual difference lies relative to its simulated distribution. 
              The average rank of WTA GS winners is 7, while the average of ATP GS winners is 6.6, 
              for a difference of 0.4. 
              Using this value, we return a p-value of 0.589, meaning that approximately 59% of the time, 
              we return a smaller difference or a difference where average rank of ATP winners is higher. 
              The actual difference between WTA and ATP GS winners is not significant.</p>")

texta <- HTML("<H2>2. Playing Style/Player Type Clusters</H2>
              Using an unsupervised method called k-means clustering with k=4 clusters, 
              we found four different groups of players on each of the WTA and ATP tours with unique identifying characteristics. 
              Three clusters were similar across tour. 
              One was the group of top players, who displayed strong match statistics across the board. 
              Another was a group of second tier players, who were slightly but significantly below the top players. 
              (The margins in tennis are extremely small! A difference of a percentage or two is massive.) 
              Finally, the last was a group of players between tours. 
              This label indicates that the players were most likely between the challenger circuit, 
              the level below the main professional tour, and the main tour. 
              Another possibility was that the player retired somewhere in middle of the dataset's timeframe. 
              Either way, this label generally indicated a lower win percentage and fewer matches 
              because these players were not participating consistently in the main tour, 
              which is the level of competition that the dataset logs. 
              Feel free to explore the cluster's statistics below:<br><br><br>")

textb <- HTML("<H2></H2>Some observations from exploring the statistics above:
              <ul>
              <li>The men are, on average, taller across the board than the women. No surprise there.</li>
              <li>The women are, on average, younger than the men.</li>
              <li>The men who are between tours are more likely than the women between tours 
              to specialize or favor tournaments played on a clay surface.</li>
              <li>The men are, on average, stronger servers than the women across the board by all service metrics 
              (ace %, % of 1st or 2nd serve points won, % of break points saved).</li>
              <li>The women are, on average, stronger returners than the men across the board by all return metrics 
              (% of 1st or 2nd serve return points won, % of break points converted).</li>
              </ul>
              <p>While not completely unexpected, the serve/return contrast is the most intriguing observation. 
              Three of the clusters were parallel, 
              and the fourth cluster differed by a group of men having particularly strong serves and 
              a group of women having particularly strong returns. 
              However, the converse could also be stated. 
              Perhaps, male tennis players tend to have weaker returns and 
              female tennis players tend to have weaker serves.</p>
              <p>From my tennis experience, the game has a bias towards serving; 
              in other words, winning your service games is a sign of a high quality match. 
              A match filled with many breaks of service would be derided as a 'break-fest'. 
              I might argue that this bias is rooted in sexism 
              or at least has differential impact on the perception of women's tennis. 
              The findings above are typically framed as men having 'strong' serves and women having 'weak' serves, 
              instead of highlighting the relative strengths of serve and return respectively. 
              While exploring this sentiment more in-depth is beyond the scope of our analysis, 
              this double standard may be one influence on the perception of the women's tour as 'inconsistent,' 
              which has very tangible consequences (e.g. the gender pay gap in tennis). </p>")

text <- function(num) {
  p(
    switch(num,
           text1,
           text2,
           text3,
           text4,
           text5)
  )
}

render_text <- function(num){
  div(
    text(num), class = "text"
  )
}

ui <- fluidPage(
  # Application title
  titlePanel(
    div(
      HTML(
        "Women's vs. Men's Tennis: Is the WTA <i>really</i> more inconsistent than the ATP?
        <p style='font-size:26px'> by Julie Ye, Chris Meng, Alan Zhou</p>"
        )
      )
    ),
  
  div(img(src = "emmar.png", width = "69%"), style="text-align: center;"),
  
  fluidRow(column(1),
           
           column(
             10,
             # intro text
             fluidRow(id = 'text',
                      column(1),
                      column(
                        10,
                        br(),
                        text0,
                        hr(),
                        h4(
                          class = "instructions",
                          "Note:",
                          "Hover over each",
                          icon("circle"),
                          "to see more details on the data point.",
                        )
                      ),
                      column(1)),
           ),
           
           column(1)),
  
  scrolly_container(outputId = "scr",
                    scrolly_graph(br(),br(),br(),br(),
                      plotlyOutput("plot_1", height = '600px')
                    ),
                    scrolly_sections(
                      ## each of these sections corresponds to an update
                      scrolly_section(id = 1, render_text(1)),
                      scrolly_section(id = 2, render_text(2)),
                      scrolly_section(id = 3, render_text(3)),
                      scrolly_section(id = 4, render_text(4)),
                      # add a scrolly_section with nothing in it;
                      # this buffer prevents the plot from disappearing while reading last section
                      scrolly_section(id = "buffer", br())
                    )
                    
  ),
  
  scrolly_container(outputId = "scr2",
                    scrolly_graph(
                      plotlyOutput("plot_2", height = '600px')
                    ),
                    scrolly_sections(
                      ## each of these sections corresponds to an update
                      scrolly_section(id = 5, render_text(5)),
                      # add a scrolly_section with nothing in it;
                      # this buffer prevents the plot from disappearing while reading last section
                      scrolly_section(id = "buffer", br())
                    )
  ),
  
  titlePanel(texta),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable", 
                  label = "Variable:",
                  c("Height" = "height",
                    "Age" = "age",
                    "% of Tournaments Played on Clay" = "clay_perc",
                    "% of Tournaments Played on Grass" = "grass_perc",
                    "% of Tournaments Played on Hard" = "hard_perc",
                    "Win %" = "win_perc",
                    "% of Points Won" = "perc_points_won",
                    "First Serve %" = "1st_serv_perc",
                    "% of First Serve Points Won" = "1st_win",
                    "Ace %" = "ace_perc",
                    "Double Fault %" = "df_perc",
                    "% of Second Serve Points Won" = "2nd_win",
                    "% of Points Won on Serve" = "svc_perc_win",
                    "Points per Service Game" = "points_per_svc_game",
                    "Break Points Saved %" = "break_point_save_perc",
                    "Break Points Faced per Game" = "bp_per_game",
                    "% of First Serve Return Points Won" = "return_1st_win",
                    "% of (Opponent) Aces While Returning" = "return_ace_perc",
                    "% of (Opponent) Double Faults While Returning" = "return_df_perc",
                    "% of Second Serve Return Points Won" = "return_2nd_win",
                    "% of Points Won on Return" = "return_perc_win",
                    "Points per Return Game" = "points_per_return_game",
                    "% of Break Points Converted" = "bp_convert_perc",
                    "Break Points per Return Game" = "return_bp_per_game",
                    "Points per Minute" = "points_per_minute"))
    ),
    
    # main panel for displaying outputs
    mainPanel(
      # output: formatted text for caption
      h3(textOutput('caption')),
      
      #output: plot of requested variable by tour and clusters
      plotOutput('clusterBars')
    )
  ),
  
  titlePanel(textb)
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot_1 <- renderPlotly({
    add <- input$scr
    
    plot_1 <- rank_point_results %>%
      filter(if (add != 4) add >= reveal else reveal %in% c(1:4)) %>%
      ggplot(aes(x = tourney_date, y = winner_rank)) +
      geom_vline(aes(xintercept = as.Date("2005-05-23")), # split at 2005
                 color = "red") +
      geom_point(aes(color = unseeded_winner, text = glue('<b>Player Name</b>: {winner_name}
                                            <b>Player Rank</b>: {winner_rank}
                                            <b>Tournament Date</b>: {tourney_date}
                                            <b>Unseeded Winner</b>: {unseeded_winner}'))) +
      scale_color_manual(values=c("black","blue")) +
      facet_grid(~ tour) +
      theme(legend.position = "none") +
      xlab("Tournament Date") +
      ylab("Rank") +
      labs(title = "Rank of Grand Slam (GS) Winners Over Time by Tour") +
      lims(x = as.Date(c("1970-01-01", "2022-01-01")), y = c(1, 225))
    ggplotly(plot_1, tooltip = "text")
  })
  
  output$plot_2 <- renderPlotly({
    add <- input$scr2
    
    plot_2 <- perm_diff_rank %>%
      ggplot() +
      geom_histogram(aes(x = diff_permrank)) +
      geom_vline(aes(xintercept = diff_rank, 
                     text = glue('<b>Average Difference in GS Winner Rank: {diff_rank}')),
                 color = "red") +
      geom_text(aes(x = 4.5, y = 60, label = "p-value = 0.589")) +
      xlab("Difference in Permuted Rank") +
      labs(title = "Simulated Distribution of Difference in GS Winner Rank")
      theme(axis.title.y = element_blank())      
    
    ggplotly(plot_2, tooltip = "text")
  })

  output$scr <- renderScrollytell({scrollytell()})
  output$scr2 <- renderScrollytell({scrollytell()})
  renderText(paste0("Section: ", input$scr))
  observe({cat("section:", input$scr, "\n")})
  
  formulaText <- reactive({
    input$variable
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  # generate a plot of requested variable against mpg
  output$clusterBars <- renderPlot({
    all_cluster_results %>%
      ggplot() +
      geom_bar(mapping = aes(fill = tour, x = cluster,
                             y = .data[[formulaText()]]),
               position = 'dodge', stat = 'identity') +
      coord_flip() +
      labs(x = "", y = "")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
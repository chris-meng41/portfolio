# GroupE-equity

## Project roles:

Chris - Project Manager & Director of Research

Julie - Task Manager & Director of Computation

Alan - Facilitator & Reporter

Title - Is the WTA really more ‘inconsistent’ than the ATP?

## Purpose:

We want to build a model to either (a) predict individual tennis matches or (b) predict next season's results OR (c) we want to compare and contrast the men's and women's professional tennis tours. 

(a/b) There is a huge betting economy contignent on these match/season results. Not that we will participate, but in general, it's of interest to sports viewers who will win. There are several fantasy sports leagues that follow results closely. What factors go into predicting who will win? How does this compare with popular perception of who will win? What variables are "X-factors" that are impossible to model? Can we predict upsets/unexpected results? Is predicting a tennis match/season possible? **END PRODUCT: model predicting future results** 

(c) There is often a perception that women's tennis is lower quality than men's tennis. Some people say that women are less capable and their style of play is less exciting. On another note, the women's tour is characterized as far more unpredictable than the men's tour. What does the data say about the similarities and differences between the tours, with regards to specific match statistics (like serve, return, unforced errors, winners), impact on social media/news world, and consistency of top players? **END PRODUCT: visualizations/statistical analysis**

## Data:
https://tennisabstract.com has extensive data that we can scrape. https://github.com/JeffSackmann contains many CSV files on individual tennis match data AND overall season result data.

## Variables/Glossary of Values:

1. tourney_level
* The level of the tournament.
  + O: Olympics
  + G: Grand Slam, the four biggest tournaments of the season (Australian, French, and US Opens and Wimbledon)
  + D: Davis Cup or Fed Cup, which is country vs. country competition
  + W: all other WTA tournaments not classified into another category, up until 2000 when the tier system was introduced
  + J: juniors tournaments (for 18 under players, not professional tour) -> very few, plan to delete as well
  + E: Exhibition, an unofficial tennis match
  + F: Year-End Finals tournament with the best players, currently the format is where the top 8 players compete in Round Robin style
  + T1-5: A classification system formerly used by the WTA, Tier 1 being the biggest tournaments and Tier 5 the smallest
  + CC: The challenger circuit, the level of competition below the main WTA and ATP tours. (Only includes a tiny fraction of matches from certain years and only WTA... plan to delete these observations.)
  + I: A WTA "international" level tournament (new WTA classification system in 2009), the lowest level of main tour competition (I believe it's equivalent to Tier 4-5)
  + PM: A WTA "Premier Mandatory" level tournament, the highest level of competition just below the grand slams (equivalent to ATP Masters, former Tier 1)
  + P: A WTA "Premier" tournament, the second highest level of competition below Premier Mandatories (former Tier 2-3)
  + A: All other ATP tournaments not classified into Masters?
  + M: Masters tournaments, only an ATP classification, highest level below Grand Slams (like Premier Mandatories)
2. winner_entry and loser_entry
* How a player enters the tournament, if not directly through high ranking.
  + `ALT`: Alternative, where a player enters the tournament that has no qualifiers, usually after another competitor's withdrawal
  + `IP`: Special case for the Olympics, where the highest rank player from an underrepresented country qualifies for the Olympics
  + `LL`: Lucky loser, where a player loses in the qualifying round but then enters the main draw, usually after the withdrawal of another competitor due to injury, illness, etc.
  + `PR`: Protected ranking, where a player uses their protected ranking from the first few months of their injury or pregnancy leave to enter tournaments when making a comeback
  + `Q`: Qualifier, where a player won the qualifying match
  + `SE`: Special exempt, where a player played well in a preceding tournament that would have overlapped with the dates of the current tournament; given a special exempt to enter into the main draw without playing in the qualifying matches
  + `SR`: See `PR`
  + `WC`: Wild card, where a player is invited to participate in an event that they wouldn't have able to qualify for under normal circumstances (e.g. young players with high potential, players returning from injuries, seasoned players who enter late)

| variable           | class     | description  |
| ----- | ----- | ------------ |
| tour               | character | `ATP` (Association of Tennis Professionals) or `WTA` (Women's Tennis Association) |
| tourney_id         | character | Tournament unique ID |
| tourney_name       | character | Tournament name (e.g. US Open, Wimbledon) |
| surface            | character | Type of court (e.g. 'Clay', 'Grass, or 'Hard') |
| draw_size          | numeric   | Total number of players in the tournament |
| tourney_level      | character | See **Variables/Glossary** #4 |
| tourney_date       | Date      | Date of tournament in the format `YYYY-MM-DD`, often the Sunday or Monday of the tournament week |
| year               | numeric   | Year of tournament |
| month              | numeric   | Month of tournament |
| match_num          | numeric   | Match number |
| winner_id          | numeric   | Player ID of match winner |
| winner_seed        | numeric   | Seed of match winner |
| winner_entry       | character | See **Variables/Glossary** #5 |
| winner_name        | character | Name of winner
| winner_hand        | character | Dominant hand of winner (e.g. 'R' for right, 'L' for left, 'U' for unknown) |
| winner_ht          | numeric   | Height of winner in centimeters |
| winner_ioc         | character | Three-letter country abbreviation of winner |
| winner_age         | numeric   | Age of winner in years |
| loser_id           | numeric   | Player ID of match loser |
| loser_seed         | numeric   | Seed of match loser |
|loser_entry         | character | See **Variables/Glossary** #5 |
| loser_name         | character | Name of loser |
| loser_hand         | character | Dominant hand of loser (e.g. 'R' for right, 'L' for left, 'U' for unknown) |
| loser_ht           | numeric   | Height of loser in centimeters |
| loser_ioc          | character | Three-letter country abbreviation of loser |
| loser_age          | numeric   | Age of loser in years |
| score              | character | Final scores of match |
| best_of            | numeric   | Number of sets for the match (e.g. '3', '5') |
| round              | character | Round of tournament (e.g. `R16` means 16 players left, `QF` for quarterfinals, `SF` for semifinals, `F` for finals, `RR` for round robin, `BR` for bronze medal (third place))
| minutes            | numeric   | Length of match in minutes |
| w_ace              | numeric   | Winner number of service aces (no-touch serves that win the point) |
| w_df               | numeric   | Winner number of double fault counts (missing both attempts of the serve) |
| w_svpt             | numeric   | Winner number of service points played (total points won or lost when serving) |
| w_1stIn            | numeric   | Winner number of first serves made (serves made on the first attempt) |
| w_1stWon           | numeric   | Winner number of first serve points won |
| w_2ndWon           | numeric   | Winner number of second serve points won |
| w_SvGms            | numeric   | Winner number of service games won (games in which they served) |
| w_bpSaved          | numeric   | Winner number of break points saved (when the opponent is one point away from winning your service game, and you end up taking the point) |
| w_bpFaced          | numeric   | Winner number of break points faced (total number of occurrences where your opponent was one point away from winning your service game) |
| l_ace              | numeric   | Loser number of service aces |
| l_df               | numeric   | Loser number of double fault counts |
| l_svpt             | numeric   | Loser number of service points played (total points won or lost when serving) |
| l_1stIn            | numeric   | Loser number of first serves made |
| l_1stWon           | numeric   | Loser number of first serve points won |
| l_2ndWon           | numeric   | Loser number of second serve points won |
| l_SvGms            | numeric   | Loser number of service games won |
| l_bpSaved          | numeric   | Loser number of break points saved |
| l_bpFaced          | numeric   | Loser number of break points faced |
| winner_rank        | numeric   | Winner's most recent ATP or WTA rank as of the tournament date | 
| winner_rank_points | numeric   | Winner's most recent number of ranking points as of the tournament date |
loser_rank           | numeric   | Loser's most recent ATP or WTA rank as of the tournament date |
| loser_rank_points  | numeric   | Loser's most recent number of ranking points as of the tournament date |
| tourney_winner     | character | Final tournament winner



## Project update (11/23/21):
1. We have access to most of our data (90%), but are still in the process of exploring which variables are important and which are not. The other 10% may include more specific ranking or player data, depending on how we structure our final report (more on that in 2).
2. Our biggest issue so far seems to be visualizing (to ourselves, not in R) what our final product might look like. We want to make a data visualization of differences between the men's and women's tour across certain metrics, and compare those differences to common perceptions, but it's not entirely clear which metrics we will end up caring about and how we might create a cohesive visualization that brings together these metrics in some way (as opposed to having separate visualizations for each of our metrics). Some ideas we had were looking up game commentary, news articles, and other forms of literature that reflect the perception that the women's tour is not as strong in comparison to the men's tour, and using what we find to get a better idea of what metrics might be of interest to us for the resulting visualization. Then, after deciding which metrics to focus on, we can begin performing statistical analysis and building our visualizations to see if the data backs up these perceptions.
3. We are thinking about using Shiny to create a scrollytelling format for our report. 

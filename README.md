# NBA Playoff Excitement
*An exploration of the most exciting (and plausible) NBA Playoffs formats.*

Quinn Johnson

Colorado State University

June 2020


## Script run order
1. __current.format.sim.R__: Functions that simulate the NBA Playoffs in its current format (as of June 2020)
2. __8west8east.format.sim.R__: Functions that simulate the NBA Playoffs in a format where the Eastern and Western Conferences each send their top eight teams to a conference-less tournament. So the #1 overall seed plays the #16 overall seed, as opposed to the #1 seed in each conference plays the #8 seed in the same conference.
3. __16team.format.sim.R__: Functions that simulate the NBA Playoffs in a format where the top 16 teams, regardless of conference, play in a conference-less tournament. 
4. __201718NBA.sim.R__: Scrapes the 2016/17 and 2017/18 NBA season data and creates models that simulate playoff games and series from each of the three formats. Metrics used to measure "excitement" are tracked within the simulations.
5. __201718.get.postseasons.R__: Scrapes data from the previous ten completed NBA postseasons (2009/10-2018/19) and gathers the real excitement metrics from those years. Then cleans up the data from the simualted postseasons and performs Tukey's Honestly Significant Difference tests on the simulations and formats to test if they are significantly different or not.
6. __201718.figures.R__: Plots each simulated formats' champions, as well as, the simulated metrics compared to the real-life data. The metrics are plotted by both format and round and just by format, to see how each of them differ/are similar.
7. __201920NBA.sim.R__: Same as __201718NBA.sim.R__, but for the 2018/19 and 2019/20 seasons.
8. __201920.get.postseasons.R__: Same as __201718.get.postseasons.R__, but used simulated metrics from the 2019/20 simulated postseasons.
9. __201920.figures.R__: Same as __201718.figures.R__, but obviously the simulated data from the 2019/20 postseason formats.




## Overview
Throughout recent years, many leagues have altered their postseason systems (e.g. MLB Wild Card Game, NFL increasing postseason berths to 14 teams), and while the NBA has made some changes over the last couple decades regarding divisions and seeding, the same basic format has persisted. In this system, each conference sends its top eight teams to play in a bracket-style tournament consisting of three best-of-seven rounds in order to name a conference champion. Then, the conference champions face off in a seven-game series to crown an NBA Champion. This format is simulated in the __current.format.sim.R__ script.

However, there have been alternate playoff formats proposed to make them more exciting and fair. The first of these formats sends, again, each conference's top eight teams into a 16-team bracket tournament where the 1st overall seed faces the 16th overall seed, 2nd faces 15th, etc. in the first round. This strays from the current format as there are no "conference champions" crowned (in the postseason at least, it is likely each conference's #1 seed would assume this title after the regular season), rather East and West teams can play eachother in rounds prior to the NBA Finals. This would allow for the #1 seed to play the team that truly was the last to earn playoff berth, as opposed to maybe the 13th or 14th overall seed if the other conference had a weaker season. This format is simulated in the __8west8east.format.sim.R__ script. The third and final format simulated in this repository is one that doesn't always differ from the format that was previously described, but is widely considered one that should be employed. In the __16team.format.sim.R__ scipt, the top 16 teams in the NBA –regardless of conference – are given playoff berths. This means that the West could have nine or ten teams in the postseason, while the East would only have seven or six. The last time this scenario arose was in the 2017/18 season in which the Denver Nuggets where in ninth place in the West after regular season play – and therefore didn't make the playoffs. However, Denver had a better record than three Eastern Conference teams that earned the sixth (Miami Heat), seventh (Milwaukee Bucks), and eighth (Washington Wizards) seeds in the East and thus were each given playoff spots. Under this new proposed format, the Nuggets would have been the 14th overall seed in the postseason and the Wizards would've missed the dance by one game. This format ensures that the league's best 16 teams earn the right to compete in the playoffs, rather than the berths be equally split between conferences. While there would be disputes around the merit/point of having conferences and divisions if this system were employed, it provides the best teams with what they deserve, regardless of if their conference opponents also have strong seasons or not.

To test which of these formats would produce the most exciting games and series, I recruited NBA game data from the 2018/19 season and the 2019/20 season – up until the league went on break due to coronavirus on March 11, 2020 – to build the predictive model and simulations for the current and 8 West, 8 East formats. This way I could kill two birds with one stone and attempt to also predict the outcome of the 2020 NBA Playoffs (given the playoffs began under the same circumstances as the league left off on in March), while also determining excitement. For the 16 Team, conference-less format I had to use the 2017/18 season as, like mentioned earlier, that was the last season where the format differed from the 8 West, 8 East scheme (later I go back and run all 3 formats using the 2017/18 season, but wanted to incorporate the current NBA situation as much as possible). In order to quantify how "exciting" games and series are, I decided to track how often series went to 6 and 7 games, the proportion of games that ended within one possession (4 points) or with a high probability of overtime, and finally the proportion of upsets to total games played (lower seed beats the higher seed). These four metrics were tracked over tens of thousands of simulations of each playoff format and later compared visually and using Tukey's honestly significant difference test to determine the most exciting scheme. 

## Script run order
1. __get.scores.R__
2. __create.model.R__
3. __current.format.sim.R__
4. __8west8east.format.sim.R__
5. __16team.format.sim.R__
6. __figures.R__
7. __get.postseasons.R__ *(optional)*
8. __NBA201718.sim.R__
9. __get.postseasons.R__ *(optional)*

## Process
__get.scores.r__ collects NBA regular season data from [basketball-reference.com](https://www.basketball-reference.com/leagues/). The script cleans the data into a more usable and sleek form so that it is easier to work with in __create.model.R__. 

The name of the script says it all, __create.model.R__ creates the model I used to simulate games. After further mutating the data into a form that contains every game from the current and previous season, I was able to create a linear model that takes in the team, opponent and whether the contest is home or away as covariates, with the response variable being point differential of the game. This trains the model to minimize the difference between expected point differential and actual point differential. Next, I used the 31 coefficients produced from the model (30 teams and the coeffient that represent location/home court advantage) to simulate the point differentials of the games we used to build the model. We do so in order to determine the relationship between expected/simulated point difference and the probability of winning. This was accomplished by building a generalized linear model that has the expected point difference as the covariate and whether the team actually won the game or not as the response. This was a binomial model. 

This simulation method was developed by [Yale Univeristy Sports Analytics Group.](https://sports.sites.yale.edu/r-nba-todays-games-predictions)

Once the models were built, I developed the three scripts that run the postseason simulations (__current.format.sim.R__,__8west8east.format.sim.R__, and __16team.format.sim.R__). Each file begins with a function that simulates one game given the team, opponent, and game in the series (1,2,5,7 are played at higher seed's home court and 3,4,6 at lower seed's). This function then returns the expected point difference, high seed's win probability, and the a bernoulli random variable (1 or 0) deciding whether the high seed won or not using p=win probability. Next, follows functions for the three rounds and the finals. At each round, the four metrics previously mentioned are recorded and half of the teams are eliminated from the simulation until we have a winner. One hundred postseason simulations finish in under a minute, but I ran each format 20,000 times to ensure all bases were covered. 

To visualize these results, I created __figures.R__ which creates histograms for the frequency of each team winning, as well as, box plots that plot the four metrics split up between rounds of the playoffs and format. Additionally, to ease the determination of significant differences between rounds and formats, the groups that were determined by the Tukey HSD tests labelled above the box plots and they are color-coded as well.

Next, __NBA201718.sim.R__ uses the 2016/17 and 2017/18 data to simulate the 2018 NBA Playoffs in all three formats and then plot them using the same visualization tools that were utilized in __figures.R__.

Finally, __get.postseason.R__ allows for the user to scrape, clean and mutate data from any other year's (or multiple years') postseason and plot the metrics over the graphs from either the 2017/18 or 2019/20 simulations (depending on order of execution).

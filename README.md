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

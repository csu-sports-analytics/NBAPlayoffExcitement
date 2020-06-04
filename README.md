# NBAPlayoffExcitement
## Quinn Johnson's exploration of the most exciting (plausible) NBA Playoffs format

Throughout recent years, many leagues have altered their postseason systems (e.g. MLB Wild Card Game, NFL increasing postseason berths to 14 teams), and while the NBA has made some changes over the last couple decades regarding divisions and seeding, the same basic format has persisted. In this system, each conference sends its top eight teams to play in a bracket-style tournament consisting of three best-of-seven rounds in order to name a conference champion. Then, the conference champions face off in a seven-game series to crown an NBA Champion. This format is simulated in the __current.format.sim.R__ script.

However, there have been alternate playoff formats proposed to make them more exciting and fair. The first of these formats sends, again, each conference's top eight teams into a 16-team bracket tournament where the 1st overall seed faces the 16th overall seed, 2nd faces 15th, etc. in the first round. This strays from the current format as there are no "conference champions" crowned (in the postseason at least, it is likely each conference's #1 seed would assume this title after the regular season), rather East and West teams can play eachother in rounds prior to the NBA Finals. This would allow for the #1 seed to play the team that truly was the last to earn playoff berth, as opposed to maybe the 13th or 14th overall seed if the other conference had a weaker season. This format is simulated in the __8west8east.format.sim.R__ script. The third and final format simulated in this repository is one that doesn't always differ from the format that was previously described, but is widely considered one that should be employed. In the __16team.format.sim.R__ scipt, the top 16 teams in the NBA –regardless of conference – are given playoff berths. This means that the West could have nine or ten teams in the postseason, while the East would only have seven or six. The last time this scenario arose was in the 2017/18 season in which the Denver Nuggets where in ninth place in the West after regular season play – and therefore didn't make the playoffs. However, Denver had a better record than three Eastern Conference teams that earned the sixth (Miami Heat), seventh (Milwaukee Bucks), and eighth (Washington Wizards) seeds in the East and thus were each given playoff spots. Under this new proposed format, the Nuggets would have been the 14th overall seed in the postseason and the Wizards would've missed the dance by one game. This format ensures that the league's best 16 teams earn the right to compete in the playoffs, rather than the berths be equally split between conferences. While there would be disputes around the merit/point of having conferences and divisions if this system were employed, it provides the best teams with what they deserve, regardless of if their conference opponents also have strong seasons or not.

Script run order:
1. __get.scores.R__
2. __create.model.R__
3. __current.format.sim.R__
4. __8west8east.format.sim.R__
5. __16team.format.sim.R__
6. __figures.R__

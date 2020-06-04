library(agricolae)

#Function that runs Tukey's HSD and gives data their groups
tukey_label <- function(df, hsd){
  df$Group <- NA
  for(i in 1:nrow(hsd$groups)){
    df$Group[which(rownames(hsd$groups)[i]==paste0(df$Format, ":",df$Round))] <- as.character(hsd$groups[i,2])
  }
  return(df)
}

# Current format champions plot
ggplot(data = champsfreq1, aes(x = champs1, y = Freq)) + geom_bar(stat = "identity", aes(fill = champs1)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors1) + geom_text(aes(label=Freq, vjust = -.25))+
  labs(title = "Frequency of Simulated Championships (20,000 sims)", x = "Team", y = "Frequency", subtitle = "Current Playoff Format for 2019/20 Season")

# 8 West, 8 East
ggplot(data = champsfreq2, aes(x = champs2, y = Freq)) + geom_bar(stat = "identity", aes(fill = champs2)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors2) + geom_text(aes(label=Freq, vjust = -.25))+
  labs(title = "Frequency of Simulated Championships (20,000 sims)", x = "Team", y = "Frequency", subtitle = "8 West and 8 East, Conference-less Playoff Format  for 2019/20 Season")

# 16 team
ggplot(data = champsfreq3, aes(x = champs3, y = Freq)) + geom_bar(stat = "identity", aes(fill = champs3)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors3) + geom_text(aes(label=Freq, vjust = -.25))+
  labs(title = "Frequency of Simulated Championships (20,000 sims)", x = "Team", y = "Frequency", subtitle = "16 Total Teams, Conference-less Playoff Format for 2017/18 Season")

#High Overtime for by rounds
#Round 1
highOT1r1 <- data.frame(Count = highOT1[seq(1, length(highOT1), 4)]) %>% mutate(., Round = 1, Format = "Current")
highOT2r1 <- data.frame(Count = highOT2[seq(1, length(highOT2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
highOT3r1 <- data.frame(Count = highOT3[seq(1, length(highOT3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
highOT1r2 <- data.frame(Count = highOT1[seq(2, length(highOT1), 4)]) %>% mutate(., Round = 2, Format = "Current")
highOT2r2 <- data.frame(Count = highOT2[seq(2, length(highOT2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
highOT3r2 <- data.frame(Count = highOT3[seq(2, length(highOT3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
highOT1r3 <- data.frame(Count = highOT1[seq(3, length(highOT1), 4)]) %>% mutate(., Round = 3, Format = "Current")
highOT2r3 <- data.frame(Count = highOT2[seq(3, length(highOT2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
highOT3r3 <- data.frame(Count = highOT3[seq(3, length(highOT3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
highOT1r4 <- data.frame(Count = highOT1[seq(4, length(highOT1), 4)]) %>% mutate(., Round = 4, Format = "Current")
highOT2r4 <- data.frame(Count = highOT2[seq(4, length(highOT2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
highOT3r4 <- data.frame(Count = highOT3[seq(4, length(highOT3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
highOT <- rbind(highOT1r1,highOT1r2,highOT1r3,highOT1r4,
                highOT2r1,highOT2r2,highOT2r3,highOT2r4,
                highOT3r1,highOT3r2,highOT3r3,highOT3r4)

#High OT HSD Tests (Separated by rounds)
lm.OT <- lm(Count ~ Format + Round, data = highOT)
HSD.OT <- HSD.test(lm.OT, c("Format", "Round")); HSD.OT$groups
highOT <- tukey_label(highOT, HSD.OT)
highOT <- highOT %>%
  mutate(., FR = paste0(Format,Round)) %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = highOT, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Number of Games With High Probability of Overtime",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format") +
  geom_text(data=highOT, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  ylim(0,15) + facet_wrap(~ Round)

#High OT HSD Tests (Not separated by rounds)
highOT1 <- data.frame(Count = highOT1) %>% mutate(., Format = "Current")
highOT2 <- data.frame(Count = highOT2) %>% mutate(., Format = "8W8E")
highOT3 <- data.frame(Count = highOT3) %>% mutate(., Format = "16TOT")
highOTf <- rbind(highOT1,highOT2,highOT3)
lm.OTf <- lm(Count ~ Format, data = highOTf)
HSD.OTf <- HSD.test(lm.OTf, "Format"); HSD.OTf$groups
highOTf <- highOTf %>%
  mutate(., Group = if_else(Format == "Current", HSD.OTf$groups[1,2],
                            if_else(Format == "8W8E", HSD.OTf$groups[2,2], HSD.OTf$groups[3,2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = highOTf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Number of Games With High Probability of Overtime",
       subtitle = "Across All Rounds",
       x = "Playoff Format") +
  geom_text(data=highOTf, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE)






#Upset Ratio by rounds
#Round 1
upsets1r1 <- data.frame(Count = upsets1[seq(1, length(upsets1), 4)]) %>% mutate(., Round = 1, Format = "Current")
upsets2r1 <- data.frame(Count = upsets2[seq(1, length(upsets2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
upsets3r1 <- data.frame(Count = upsets3[seq(1, length(upsets3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
upsets1r2 <- data.frame(Count = upsets1[seq(2, length(upsets1), 4)]) %>% mutate(., Round = 2, Format = "Current")
upsets2r2 <- data.frame(Count = upsets2[seq(2, length(upsets2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
upsets3r2 <- data.frame(Count = upsets3[seq(2, length(upsets3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
upsets1r3 <- data.frame(Count = upsets1[seq(3, length(upsets1), 4)]) %>% mutate(., Round = 3, Format = "Current")
upsets2r3 <- data.frame(Count = upsets2[seq(3, length(upsets2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
upsets3r3 <- data.frame(Count = upsets3[seq(3, length(upsets3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
upsets1r4 <- data.frame(Count = upsets1[seq(4, length(upsets1), 4)]) %>% mutate(., Round = 4, Format = "Current")
upsets2r4 <- data.frame(Count = upsets2[seq(4, length(upsets2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
upsets3r4 <- data.frame(Count = upsets3[seq(4, length(upsets3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
upsets <- rbind(upsets1r1,upsets1r2,upsets1r3,upsets1r4,
                upsets2r1,upsets2r2,upsets2r3,upsets2r4,
                upsets3r1,upsets3r2,upsets3r3,upsets3r4)

#Upsets HSD Tests (Separated by rounds)
lm.upsets <- lm(Count ~ Format + Round, data = upsets)
HSD.upsets <- HSD.test(lm.upsets, c("Format", "Round")); HSD.upsets$groups
upsets <- tukey_label(upsets, HSD.upsets)
upsets <- upsets %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = upsets, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Number of Upsets (Lower Seed Beat Higher Seed)/Total Number of Games in the Round",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format") + 
  geom_text(data=upsets, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  ylim(0,1.25) +
  facet_wrap(~ Round)

#Upsets HSD Tests (Not separated by rounds)
upsets1 <- data.frame(Count = upsets1) %>% mutate(., Format = "Current")
upsets2 <- data.frame(Count = upsets2) %>% mutate(., Format = "8W8E")
upsets3 <- data.frame(Count = upsets3) %>% mutate(., Format = "16TOT")
upsetsf <- rbind(upsets1,upsets2,upsets3)
lm.upsetsf <- lm(Count ~ Format, data = upsetsf)
HSD.upsetsf <- HSD.test(lm.upsetsf, "Format"); HSD.upsetsf$groups
upsetsf <- upsetsf %>%
  mutate(., Group = if_else(Format == "Current", HSD.upsetsf$groups[2,2],
                            if_else(Format == "8W8E", HSD.upsetsf$groups[3,2], HSD.upsetsf$groups[1,2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = upsetsf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Number of Upsets (Lower Seed Beat Higher Seed)/Total Number of Games in the Round",
       subtitle = "Across All Rounds",
       x = "Playoff Format") +
  geom_text(data=upsetsf, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE)







#Long series by rounds
#Round 1
longSeries1r1 <- data.frame(Count = longSeries1[seq(1, length(longSeries1), 4)]) %>% mutate(., Round = 1, Format = "Current")
longSeries2r1 <- data.frame(Count = longSeries2[seq(1, length(longSeries2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
longSeries3r1 <- data.frame(Count = longSeries3[seq(1, length(longSeries3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
longSeries1r2 <- data.frame(Count = longSeries1[seq(2, length(longSeries1), 4)]) %>% mutate(., Round = 2, Format = "Current")
longSeries2r2 <- data.frame(Count = longSeries2[seq(2, length(longSeries2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
longSeries3r2 <- data.frame(Count = longSeries3[seq(2, length(longSeries3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
longSeries1r3 <- data.frame(Count = longSeries1[seq(3, length(longSeries1), 4)]) %>% mutate(., Round = 3, Format = "Current")
longSeries2r3 <- data.frame(Count = longSeries2[seq(3, length(longSeries2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
longSeries3r3 <- data.frame(Count = longSeries3[seq(3, length(longSeries3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
longSeries1r4 <- data.frame(Count = longSeries1[seq(4, length(longSeries1), 4)]) %>% mutate(., Round = 4, Format = "Current")
longSeries2r4 <- data.frame(Count = longSeries2[seq(4, length(longSeries2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
longSeries3r4 <- data.frame(Count = longSeries3[seq(4, length(longSeries3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
longSeries <- rbind(longSeries1r1,longSeries1r2,longSeries1r3,longSeries1r4,
                longSeries2r1,longSeries2r2,longSeries2r3,longSeries2r4,
                longSeries3r1,longSeries3r2,longSeries3r3,longSeries3r4)

#Long Series HSD Tests (Separated by rounds)
lm.longSeries <- lm(Count ~ Format + Round, data = longSeries)
HSD.longSeries <- HSD.test(lm.longSeries, c("Format", "Round")); HSD.longSeries$groups
longSeries <- tukey_label(longSeries, HSD.longSeries)
longSeries <- longSeries %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, as.integer(Count), as.integer(NaN))) %>%
  ungroup(.)

ggplot(data = longSeries, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Number of Long Series (6 and 7 Games)",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format") +
  geom_text(data=longSeries, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  ylim(0,10) +
  facet_wrap(~ Round)

#longSeries HSD Tests (Not separated by rounds)
longSeries1 <- data.frame(Count = longSeries1) %>% mutate(., Format = "Current")
longSeries2 <- data.frame(Count = longSeries2) %>% mutate(., Format = "8W8E")
longSeries3 <- data.frame(Count = longSeries3) %>% mutate(., Format = "16TOT")
longSeriesf <- rbind(longSeries1,longSeries2,longSeries3)
lm.longSeriesf <- lm(Count ~ Format, data = longSeriesf)
HSD.longSeriesf <- HSD.test(lm.longSeriesf, "Format"); HSD.longSeriesf$groups
longSeriesf <- longSeriesf %>%
  mutate(., Group = if_else(Format == "Current", HSD.longSeriesf$groups[3,2],
                            if_else(Format == "8W8E", HSD.longSeriesf$groups[2,2], HSD.longSeriesf$groups[1,2])))
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, as.integer(NaN))) %>%
  ungroup(.)

ggplot(data = longSeriesf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Number of Long Series (6 and 7 Games)",
       subtitle = "Across All Rounds",
       x = "Playoff Format") +
  geom_text(data=longSeriesf, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE)







#Close games by rounds
#Round 1
closeGames1r1 <- data.frame(Count = closeGames1[seq(1, length(closeGames1), 4)]) %>% mutate(., Round = 1, Format = "Current")
closeGames2r1 <- data.frame(Count = closeGames2[seq(1, length(closeGames2), 4)]) %>% mutate(., Round = 1, Format = "8W, 8E")
closeGames3r1 <- data.frame(Count = closeGames3[seq(1, length(closeGames3), 4)]) %>% mutate(., Round = 1, Format = "16 TOT")
#Round 2
closeGames1r2 <- data.frame(Count = closeGames1[seq(2, length(closeGames1), 4)]) %>% mutate(., Round = 2, Format = "Current")
closeGames2r2 <- data.frame(Count = closeGames2[seq(2, length(closeGames2), 4)]) %>% mutate(., Round = 2, Format = "8W, 8E")
closeGames3r2 <- data.frame(Count = closeGames3[seq(2, length(closeGames3), 4)]) %>% mutate(., Round = 2, Format = "16 TOT")
#Round 3
closeGames1r3 <- data.frame(Count = closeGames1[seq(3, length(closeGames1), 4)]) %>% mutate(., Round = 3, Format = "Current")
closeGames2r3 <- data.frame(Count = closeGames2[seq(3, length(closeGames2), 4)]) %>% mutate(., Round = 3, Format = "8W, 8E")
closeGames3r3 <- data.frame(Count = closeGames3[seq(3, length(closeGames3), 4)]) %>% mutate(., Round = 3, Format = "16 TOT")
#Finals
closeGames1r4 <- data.frame(Count = closeGames1[seq(4, length(closeGames1), 4)]) %>% mutate(., Round = 4, Format = "Current")
closeGames2r4 <- data.frame(Count = closeGames2[seq(4, length(closeGames2), 4)]) %>% mutate(., Round = 4, Format = "8W, 8E")
closeGames3r4 <- data.frame(Count = closeGames3[seq(4, length(closeGames3), 4)]) %>% mutate(., Round = 4, Format = "16 TOT")
closeGames <- rbind(closeGames1r1,closeGames1r2,closeGames1r3,closeGames1r4,
                    closeGames2r1,closeGames2r2,closeGames2r3,closeGames2r4,
                    closeGames3r1,closeGames3r2,closeGames3r3,closeGames3r4)

#Close Games HSD Tests (Separated by rounds)
lm.close <- lm(Count ~ Format + Round, data = closeGames)
HSD.close <- HSD.test(lm.close, c("Format", "Round")); HSD.close$groups
closeGames <- tukey_label(closeGames, HSD.close)
closeGames <- closeGames %>%
  group_by(Format, Round) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, as.integer(Count), as.integer(NaN))) %>%
  ungroup(.)

ggplot(data = closeGames, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Number of Close Games (Predicted Pts Diff < 3)",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format") +
  geom_text(data=closeGames, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE) +
  ylim(0,30) +
  facet_wrap(~ Round)

#longSeries HSD Tests (Not separated by rounds)
closeGames1 <- data.frame(Count = closeGames1) %>% mutate(., Format = "Current")
closeGames2 <- data.frame(Count = closeGames2) %>% mutate(., Format = "8W8E")
closeGames3 <- data.frame(Count = closeGames3) %>% mutate(., Format = "16TOT")
closeGamesf <- rbind(closeGames1,closeGames2,closeGames3)
lm.closef <- lm(Count ~ Format, data = closeGamesf)
HSD.closef <- HSD.test(lm.closef, "Format"); HSD.closef$groups
closeGamesf <- closeGamesf %>%
  mutate(., Group = if_else(Format == "Current", HSD.closef$groups[3,2],
                            if_else(Format == "8W8E", HSD.closef$groups[2,2], HSD.closef$groups[1,2]))) %>%
  group_by(Format) %>%
  arrange(., desc(Count)) %>%
  mutate(., max_group = if_else(row_number()==1, Count, NaN)) %>%
  ungroup(.)

ggplot(data = closeGamesf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Number of Close Games (Predicted Pts Diff < 3)",
       subtitle = "Across All Rounds",
       x = "Playoff Format") +
  geom_text(data=closeGamesf, aes(y = max_group, label = Group, vjust = -.75, col = Group), show.legend = FALSE)












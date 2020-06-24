library(ggrepel)
#### 2019/20 current format champions plot ####
ggplot(data = champsfreq1, aes(x = reorder(champs1, -Freq), y = Freq/S)) + geom_bar(stat = "identity", aes(fill = champs1)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors1) + geom_text(aes(label=round(Freq/S,3), vjust = -.25), size = 2.5)+
  scale_y_continuous(limits = c(0,.35), labels = scales::percent_format(), breaks = c(0,.167,.333)) +
  labs(title = "Proportion of Simulated Championships (10,000 sims)", x = "Team", y = "Proportion", subtitle = "Current Playoff Format for 2019/20 Season")

#### 2019/20 8W,8E format champions plot ####
ggplot(data = champsfreq2, aes(x = reorder(champs2, -Freq), y = Freq/S)) + geom_bar(stat = "identity", aes(fill = champs2)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors2) + geom_text(aes(label=round(Freq/S,3), vjust = -.25), size = 2.5)+
  scale_y_continuous(limits = c(0,.35), labels = scales::percent_format(), breaks = c(0,.167,.333)) +
  labs(title = "Proportion of Simulated Championships (10,000 sims)", x = "Team", y = "Proportion", subtitle = "8 West and 8 East, Conference-less Playoff Format  for 2019/20 Season")

#### 2019/20 Sim High OT vs 2010-2019 (by Round and Format) ####
ggplot(data = highOT, aes(x = Format, y = highOT$Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Games With High Probability of Overtime",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format", y = "# of Games with High OT Prob/Total # of Games Played") +
  geom_text(data=highOT, aes(y = max_group, label = Group, vjust = -.75), show.legend = FALSE) +
  facet_wrap(~ Round) + scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_OT[1:40,], aes(yintercept = n, color = V3), linetype = "dashed") +
  geom_label_repel(data=past_OT[1:40,], aes(x = 2.5, y=n, label = Year, color = V3), 
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Sim High OT vs 2010-2019 (by Format) ####
ggplot(data = highOTf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Games With High Probability of Overtime",
       subtitle = "Across All Rounds",
       x = "Playoff Format", y = "# of Games with High OT Prob/Total # of Games Played") +
  geom_text(data=highOTf, aes(y = max_group, label = Group, vjust = -.75), show.legend = FALSE) +
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_OT[41:50,], aes(yintercept = n, color = V3), linetype = "dashed") +
  geom_label_repel(data=past_OT[41:50,], aes(x = 2.5, y=n, label = Year, color = V3), 
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Sim Upsets vs 2010-2019 (by Round and Format) ####
ggplot(data = upsets, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Upsets (Lower Seed Beat Higher Seed)",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format", y = "# of Upsets/Total # of Games Played") + 
  geom_text(data=upsets, aes(y = max_group, label = Group, vjust = -.75), show.legend = FALSE) +
  facet_wrap(~ Round) + scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_Upset[1:40,], aes(yintercept = n, color = V3), linetype = "dashed") +
  geom_label_repel(data=past_Upset[1:40,], aes(x = 2.5, y=n, label = Year, color = V3), 
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Sim Upsets vs 2010-2019 (by Format) ####
ggplot(data = upsetsf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Upsets (Lower Seed Beat Higher Seed)",
       subtitle = "Across All Rounds",
       x = "Playoff Format", y = "# of Upsets/Total # of Games Played") +
  geom_text(data=upsetsf, aes(y = max_group, label = Group, vjust = -.75), show.legend = FALSE) +
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_Upset[41:50,], aes(yintercept = n, color = V3), linetype = "dashed") +
  geom_label_repel(data=past_Upset[41:50,], aes(x = 2.5, y=n, label = Year, color = V3), 
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Sim Long Series vs 2010-2019 (by Round and Format) ####
ggplot(data = longSeries, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Long Series (6 and 7 Games)",
       subtitle = "Separated by Playoff Rounds (4 = Finals)",
       x = "Playoff Format", y = "# of Long Series/Total # of Series Played") +
  geom_text(data=longSeries, aes(y = max_group, label = Group, vjust = -.75), show.legend = FALSE) +
  facet_wrap(~ Round) + scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_longSeries[1:40,], aes(yintercept = n, color = V3), linetype = "dashed") +
  geom_label_repel(data=past_longSeries[1:40,], aes(x = 2.5, y=n, label = Year, color = V3), 
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Sim Long Series vs 2010-2019 (by Format) ####
ggplot(data = longSeriesf, aes(x = Format, y = Count)) +
  geom_boxplot(aes(fill = Group), show.legend = FALSE) + 
  theme_minimal() + 
  labs(title = "Percentage of Long Series (6 and 7 Games)",
       subtitle = "Across All Rounds",
       x = "Playoff Format", y = "# of Long Series/Total # of Series Played") +
  geom_text(data=longSeriesf, aes(y = max_group, label = Group, vjust = -.75), show.legend = FALSE) +
  geom_hline(data = past_longSeries[41:50,], aes(yintercept = n, color = V3), linetype = "dashed") +
  geom_label_repel(data=past_longSeries[41:50,], aes(x = 2.5, y=n, label = Year, color = V3), 
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")












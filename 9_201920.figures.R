library(ggrepel)
#### 2019/20 current format champions plot ####
ggplot(data = champsfreq1, aes(x = reorder(champs1, -Freq), y = Freq/S)) + geom_bar(stat = "identity", aes(fill = champs1)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors1) + geom_text(aes(label=round(Freq/S,3), vjust = -.25), size = 2.5)+
  scale_y_continuous(limits = c(0,.55), labels = scales::percent_format(), breaks = c(0,.25,.5)) +
  labs(title = "Proportion of Simulated Championships (10,000 sims)", x = "Team", y = "Proportion", subtitle = "Current Playoff Format for 2019/20 Season")

#### 2019/20 8W,8E format champions plot ####
ggplot(data = champsfreq2, aes(x = reorder(champs2, -Freq), y = Freq/S)) + geom_bar(stat = "identity", aes(fill = champs2)) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), legend.position = "none") + 
  scale_fill_manual(values = primcolors2) + geom_text(aes(label=round(Freq/S,3), vjust = -.25), size = 2.5)+
  scale_y_continuous(limits = c(0,.55), labels = scales::percent_format(), breaks = c(0,.25,.5)) +
  labs(title = "Proportion of Simulated Championships (10,000 sims)", x = "Team", y = "Proportion", subtitle = "8 West and 8 East, Conference-less Playoff Format  for 2019/20 Season")

#### 2019/20 Sim High OT vs 2010-2019 (by Round and Format) ####
ggplot(data = highOT, aes(x = Format, y = Count)) +
  geom_violin(aes(fill = Format), show.legend = FALSE) +
  theme_minimal() + scale_fill_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  labs(title = "Percentage of Games With High Probability of Overtime by Playoff Format and Round",
       x = "Playoff Format", y = "Proportion with High OT Probability") +
  facet_wrap(~ Round, labeller = labeller(Round = roundlabs)) + 
  scale_y_continuous(limits = c(0,.8), labels = scales::percent_format(), breaks = c(0,.25,.5,.75)) +
  geom_hline(data = past_OT[1:40,], aes(yintercept = n),  color = '#A4A4A4', linetype = "dashed") +
  geom_label_repel(data=past_OT[1:40,], aes(x = 4, y=n, label = Year), color = '#A4A4A4',
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Sim High OT vs 2010-2019 (by Format) ####
ggplot(data = highOTf, aes(x = Format, y = Count)) +
  geom_violin(aes(fill = Format), show.legend = FALSE) +
  theme_minimal() + scale_fill_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  labs(title = "Percentage of Games With High Probability of Overtime by Playoff Format",
       x = "Playoff Format", y = "Proportion of Games with High OT Probability") +
  scale_y_continuous(limits = c(0,0.8), labels = scales::percent_format(), breaks = c(0,.25,.5,.75)) +
  geom_hline(data = past_OT[41:50,], aes(yintercept = n),  color = '#A4A4A4', linetype = "dashed") +
  geom_label_repel(data=past_OT[41:50,], aes(x = 4, y=n, label = Year), color = '#A4A4A4',
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Upsets vs 2010-2019 (by Round and Format) ####
ggplot(data = upsets, aes(x = Format, y = Count)) +
  geom_violin(aes(fill = Format), show.legend = FALSE) +
  theme_minimal() + scale_fill_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  labs(title = "Percentage of Upsets by Playoff Format and Round",
       x = "Playoff Format", y = "Proportion of Upsets") +
  facet_wrap(~ Round, labeller = labeller(Round = roundlabs)) + 
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_Upset[1:40,], aes(yintercept = n),  color = '#A4A4A4', linetype = "dashed") +
  geom_label_repel(data=past_Upset[1:40,], aes(x = 4, y=n, label = Year), color = '#A4A4A4',
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Sim Upsets vs 2010-2019 (by Format) ####
ggplot(data = upsetsf, aes(x = Format, y = Count)) +
  geom_violin(aes(fill = Format), show.legend = FALSE) +
  theme_minimal() + scale_fill_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  labs(title = "Percentage of Upsets by Playoff Format",
       x = "Playoff Format", y = "Proportion of Upsets") +
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_Upset[41:50,], aes(yintercept = n),  color = '#A4A4A4', linetype = "dashed") +
  geom_label_repel(data=past_Upset[41:50,], aes(x = 4, y=n, label = Year), color = '#A4A4A4',
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Long Series vs 2010-2019 (by Round and Format) ####
ggplot(data = longSeries, aes(x = Format, y = Count)) +
  geom_count(aes(color = Format), show.legend = FALSE) +
  theme_minimal() + scale_color_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  labs(title = "Percentage of Long Series by Playoff Format and Round",
       x = "Playoff Format", y = "Proportion of Long Series") +
  facet_wrap(~ Round, labeller = labeller(Round = roundlabs)) + 
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_longSeries[1:40,], aes(yintercept = n),  color = '#A4A4A4', linetype = "dashed") +
  geom_label_repel(data=past_longSeries[1:40,], aes(x = 4, y=n, label = Year), color = '#A4A4A4',
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")

#### 2019/20 Sim Long Series vs 2010-2019 (by Format) ####
ggplot(data = longSeriesf, aes(x = Format, y = Count)) +
  geom_count(aes(color = Format), show.legend = FALSE) +
  theme_minimal() + scale_color_manual(values = c("#619CFF", "#00BA38", "#F8766D")) +
  labs(title = "Percentage of Long Series by Playoff Format",
       x = "Playoff Format", y = "Proportion of Long Series") +
  scale_y_continuous(limits = c(0,1.1), labels = scales::percent_format(), breaks = c(0,.25,.5,.75,1)) +
  geom_hline(data = past_longSeries[41:50,], aes(yintercept = n),  color = '#A4A4A4', linetype = "dashed") +
  geom_label_repel(data=past_longSeries[41:50,], aes(x = 4, y=n, label = Year), color = '#A4A4A4',
                   force = 10, size = 2.5, direction = "y", box.padding = 1) +
  theme(legend.position = "none")













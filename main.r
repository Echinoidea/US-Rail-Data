# Remove Mexico and Canada data
df <- data.frame(read.csv("North_American_Rail_Lines.csv"))
df2 <- df[!df$COUNTRY!="US",]

# Sum all MILES together for each state
StateAB <- unique(df2$STATEAB)
states_df <- aggregate(df2$MILES, list(df2$STATEAB), sum)

# Rename columns
colnames(states_df) = c("StateAB", "RailMiles")

# Add State Name from abbreviation, sort, remove DC, create Area col and RailMiles / Area of state
states_df$StateName <- state.name[match(states_df$StateAB, state.abb)]
states_df <- states_df[order(states_df$StateName),]
states_df <- states_df[-51,]
states_df$Area <- state.area
states_df$RailByArea <- states_df$RailMiles / states_df$Area

# Visualizations
library(ggplot2)
states_df <- states_df[order(states_df$RailByArea),]

# Top and bottom 5 states ranked by rail to land area proportion
dfHeadFoot <- states_df[1:5,]
dfHeadFoot <- rbind(dfHeadFoot, states_df[46:50,])

# Order ascending and create factor to retain order in bar graph
dfHeadFoot <- dfHeadFoot[order(dfHeadFoot$RailByArea),]
dfHeadFoot$StateAB <- factor(dfHeadFoot$StateAB, levels = dfHeadFoot$StateAB)

ggplot(dfHeadFoot, aes(x=StateAB, y=RailByArea, fill = StateAB)) +
  geom_bar(stat="identity", width = 0.5) +
  geom_text(aes(label=round(RailByArea, digits=3)), position=position_dodge(width=0.9), hjust=-0.08) +
  labs(title = "5 Bottom and Top States by Rail Line / Area",
       subtitle = "Rail by Area = Rail Miles / State Land Area (sq. mi)") +
  xlab("Rail by Area") +
  ylab("State") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        legend.position = "none") +
  scale_fill_manual(values=c("dodgerblue2", "dodgerblue2", "dodgerblue2", 
                             "dodgerblue2", "dodgerblue2", "tomato3", "tomato3", 
                             "tomato3", "tomato3", "tomato3")) +
  coord_flip()
    
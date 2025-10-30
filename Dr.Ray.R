library(tidyverse)
plays <- read.csv("plays.csv")

plays_clean <- na.omit(plays)

boxplot(yardsGained~offenseFormation,data=plays_clean)

coverage_group <- forcats::fct_collapse(
  plays_clean$coverage_group,
  Cover_3 = c("Cover-3", "Cover-3 Cloud Left", "Cover-3 Cloud Right", 
              "Cover-3 Seam", "Cover-3 Double Cloud"),
  Cover_6 = c("Cover 6-Left", "Cover-6 Right"),
  Cover_1=c("Cover-1","Cover-1 Double"),
  Other_Rare = c("Bracket", "Miscellaneous", "Prevent", "2-Man", "Goal Line")
)

plays %>%
  group_by(offenseFormation,pff_passCoverage) %>%
  summarize(mean_value = mean(yardsGained, na.rm = TRUE), n=n())

colnames(plays)
mean(plays$yardsGained,na.rm = TRUE)
boxplot(Value ~ interaction(offenseFormation, Factor2), data = plays,
        main = "Boxplot of Value by Interaction of Factor1 and Factor2",
        xlab = "Interaction of Factor1 and Factor2",
        ylab = "Value")

down,offensFor, yardLine,yardtoGo,timeLeft

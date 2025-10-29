plays <- read.csv("plays.csv")
View(plays)

dim(plays)        #  (rows, columns)

head(plays,6)

str(plays)  

#Summary data
# yardsGained
summary(plays$yardsGained)
sd(plays$yardsGained, na.rm = TRUE)

# passLength
summary(plays$passLength)
sd(plays$passLength, na.rm = TRUE)


# Frequency table
table(plays$pff_passCoverage)

# Proportions
prop.table(table(plays$pff_passCoverage))


#Check for na
sum(is.na(plays$yardsGained))
sum(is.na(plays$passLength))
sum(is.na(plays$pff_passCoverage))

#2 variable summary
library(dplyr)

plays %>%
  group_by(pff_passCoverage) %>%
  summarise(
    Min = min(yardsGained, na.rm = TRUE),
    Q1 = quantile(yardsGained, 0.25, na.rm = TRUE),
    Median = median(yardsGained, na.rm = TRUE),
    Q3 = quantile(yardsGained, 0.75, na.rm = TRUE),
    Max = max(yardsGained, na.rm = TRUE),
    Mean = mean(yardsGained, na.rm = TRUE),
    SD = sd(yardsGained, na.rm = TRUE)
  )

#Subset the data to filter only 4th down play
# Filter 4th quarter plays
df_4th <- subset(plays, down == 4 & !is.na(yardsGained))

summary(df_4th[, c("yardsGained", "yardsToGo", 
                   "absoluteYardlineNumber", "passLength", 
                   "gameClock", "expectedPoints")])




#Visualize the numeric variable
library(ggplot2)

# Pass Length vs Yards Gained
ggplot(df_4th, aes(x = passLength, y = yardsGained)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Pass Length vs Yards Gained (4th Down)",
       x = "Pass Length (Air Yards)", y = "Yards Gained")

# Coverage Type vs Yards Gained
ggplot(df_4th, aes(x = pff_passCoverage, y = yardsGained)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Yards Gained by Coverage Type (4th Down)",
       x = "PFF Coverage Type", y = "Yards Gained") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Correlation
num_vars <- df_4th[, c("yardsGained", "yardsToGo",
                           "absoluteYardlineNumber", "passLength", "expectedPoints")]
round(cor(num_vars, use = "complete.obs"), 2)


#Catagorial variable
table(df_4th$pff_passCoverage)
table(df_4th$offenseFormation)
table(df_4th$playAction)
table(df_4th$dropbackType)
table(df_4th$pff_manZone)


#Distribution of yardGain in 4th down play
library(ggplot2)

ggplot(df_4th, aes(x = yardsGained)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Yards Gained on 4th Down Plays",
       x = "Yards Gained",
       y = "Number of Plays") +
  theme_minimal()

#YardGained and passLength
ggplot(df_4th, aes(x = passLength, y = yardsGained)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Pass Length vs. Yards Gained on 4th Down",
       x = "Pass Length (Air Yards)",
       y = "Total Yards Gained") +
  theme_minimal()

#YardGain and CoverageType
ggplot(df_4th, aes(x = pff_passCoverage, y = yardsGained)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Yards Gained by Defensive Coverage on 4th Down",
       x = "Defensive Coverage Type", y = "Yards Gained") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




df_4Quarter <- subset(plays, quarter == 4 & !is.na(yardsGained))

#Testing to see if 4th down is different from other downs in term of YardGain
anova_model <- aov(yardsGained ~ factor(down), data = plays)
summary(anova_model)
TukeyHSD(anova_model)

#Frequency table for downs
table(plays$down)

#From the anova_testing, we have not siginicant different
#from down 1-3 but 4 down have signifcant different
#therefore I split downs into 2 groups 1 is from 1-3 and the other one is 4th only
plays$fourthDown <- ifelse(plays$down == 4, "4th", "1-3th")
table(plays$fourthDown)



summary(as.numeric(table(plays$gameId)))

table(plays$possessionTeam)









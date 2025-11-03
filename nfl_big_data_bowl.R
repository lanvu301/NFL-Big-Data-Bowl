library(dplyr)
library(stringr)



# =========================================================
# STEP 0: LOAD DATA AND INITIAL INSPECTION
# =========================================================

plays <- read.csv("plays.csv")
players <- read.csv("nfl-big-data-bowl-2025/players.csv")
View(plays)

dim(plays)        # (rows, columns)
head(plays, 6)
str(plays)

# =========================================================
# STEP 1: Clean and format the data
# =========================================================

plays$fourthDown <- ifelse(plays$down == 4, "4th", "1-3th")
# Convert "mm:ss" to total seconds
plays$gameClock_sec <- sapply(strsplit(plays$gameClock, ":"), function(x) {
  as.numeric(x[1]) * 60 + as.numeric(x[2])
})

plays <- plays %>%
  mutate(
    passer_abbrev = str_extract(playDescription, "\\b[A-Z]\\.[A-Z][a-z]+")
  )

players <- players %>%
  mutate(passer_abbrev = paste0(substr(displayName, 1, 1), ".", word(displayName, 2)))

plays <- plays %>%
  left_join(players %>% filter(position == "QB"), by = "passer_abbrev")

View(plays$passer_abbrev)
table(plays$passer_abbrev)

#Filter entries that have passer apppear more than 5
plays <- plays %>%
  group_by(passer_abbrev) %>%       # group by the QB name/abbreviation
  filter(n() > 5) %>%               # keep only QBs with >5 plays
  ungroup()                         # remove grouping afterward

colnames(plays)
library(dplyr)

# Keep only specific columns
plays_small <- plays %>%
  select(gameId, playId, playDescription, passer_abbrev,quarter, yardsGained,passResult,pff_passCoverage,
         position.y,down,position.x,playAction)
View(plays_small)
table(plays_small$passer_abbrev)
sum(is.na(plays_small$position.x))

table(plays$fourthDown)
# =========================================================
# STEP 2: SUMMARY STATISTICS
# =========================================================

# yardsGained
summary(plays$yardsGained)
sd(plays$yardsGained, na.rm = TRUE)


# =========================================================
# STEP 3: CATEGORICAL SUMMARIES
# =========================================================

# Frequency table
table(plays$pff_passCoverage)

# Proportions
prop.table(table(plays$pff_passCoverage))

# =========================================================
# STEP 4: CHECK FOR MISSING VALUES
# =========================================================

sum(is.na(plays$yardsGained))
sum(is.na(plays$passLength))
sum(is.na(plays$pff_passCoverage))

colSums(is.na(plays))

# =========================================================
# STEP 5: TWO-VARIABLE SUMMARY
# =========================================================

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

# =========================================================
# STEP 6: VISUALIZATIONS
# =========================================================

library(ggplot2)

# Pass Length vs Yards Gained (4th Down)
ggplot(df_4th, aes(x = passLength, y = yardsGained)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Pass Length vs Yards Gained (4th Down)",
       x = "Pass Length (Air Yards)", y = "Yards Gained")

# Coverage Type vs Yards Gained (4th Down)
ggplot(df_4th, aes(x = pff_passCoverage, y = yardsGained)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Yards Gained by Coverage Type (4th Down)",
       x = "PFF Coverage Type", y = "Yards Gained") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Categorical variable frequencies
table(df_4th$pff_passCoverage)
table(df_4th$offenseFormation)
table(df_4th$playAction)
table(df_4th$dropbackType)
table(df_4th$pff_manZone)

# =========================================================
# STEP 7: DISTRIBUTION OF YARDS GAINED ON 4TH DOWN PLAYS
# =========================================================

ggplot(df_4th, aes(x = yardsGained)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Yards Gained on 4th Down Plays",
       x = "Yards Gained", y = "Number of Plays") +
  theme_minimal()

# =========================================================
# STEP 8: TEST WHETHER 4TH DOWN DIFFERS FROM OTHER DOWNS
# =========================================================

df_4Quarter <- subset(plays, quarter == 4 & !is.na(yardsGained))

anova_model <- aov(yardsGained ~ factor(down), data = plays)
summary(anova_model)
TukeyHSD(anova_model)

# Frequency table for downs
table(plays$down)

# From the ANOVA test:
# No significant difference from downs 1–3, but 4th down differs
# Therefore, split downs into two groups: 1–3 vs 4th


# =========================================================
# STEP 9: INITIAL MODEL WITH MANY VARIABLES
# =========================================================

model_ols <- lm(
  yardsGained ~ down + yardsToGo + quarter + absoluteYardlineNumber +
    offenseFormation + passResult + playAction +
    pff_passCoverage + pff_manZone + rushLocationType +
    qbSneak + qbKneel + penaltyYards,
  data = plays
)
summary(model_ols)

# =========================================================
# STEP 10: CLEAN MODEL WITH FEWER MISSING DATA
# =========================================================

plays_clean <- subset(plays, select = c(
  yardsGained, yardsToGo, quarter, absoluteYardlineNumber,
  offenseFormation, playAction, pff_passCoverage, pff_manZone,
  gameId, fourthDown
))

plays_clean <- na.omit(plays_clean)
nrow(plays_clean)

# =========================================================
# STEP 11: RE-RUN MODEL AFTER CLEANING
# =========================================================

model_ols <- lm(
  yardsGained ~ fourthDown + yardsToGo + quarter + absoluteYardlineNumber +
    offenseFormation + playAction + pff_passCoverage + pff_manZone+pff_passCoverage*offenseFormation,
  data = plays_clean
)
summary(model_ols)

# =========================================================
# STEP 12: MERGE SMALL COVERAGE GROUPS
# =========================================================

plays_clean$coverage_group <- plays_clean$pff_passCoverage

plays_clean$coverage_group <- forcats::fct_collapse(
  plays_clean$coverage_group,
  Cover_3 = c("Cover-3", "Cover-3 Cloud Left", "Cover-3 Cloud Right",
              "Cover-3 Seam", "Cover-3 Double Cloud"),
  Cover_6 = c("Cover 6-Left", "Cover-6 Right"),
  Cover_1 = c("Cover-1", "Cover-1 Double"),
  Other_Rare = c("Bracket", "Miscellaneous", "Prevent", "2-Man", "Goal Line")
)

table(plays_clean$coverage_group)

# =========================================================
# STEP 13: RE-RUN MODEL WITH COLLAPSED COVERAGES
# =========================================================

model_ols <- lm(
  yardsGained ~ fourthDown + yardsToGo + quarter + absoluteYardlineNumber +
    offenseFormation + playAction + coverage_group + pff_manZone,
  data = plays_clean
)
summary(model_ols)

# =========================================================
# STEP 14: === TEST OLS ASSUMPTIONS HERE ===
# =========================================================

# === Linearity ===
plot(model_ols, which = 1)  # Residuals vs Fitted

# === Normality of Residuals ===
plot(model_ols, which = 2)  # Q–Q plot

shapiro.test(residuals(model_ols))

# Q–Q plot
qqnorm(residuals(model_ols))
qqline(residuals(model_ols), col = "red", lwd = 2)

# Histogram
hist(residuals(model_ols), breaks = 50, col = "lightblue",
     main = "Distribution of Residuals", xlab = "Residuals")

#Scatter_Plot
plot(model_ols$fitted.values, residuals(model_ols),
     xlab = "Fitted Values (Predicted Yards)",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values",
     pch = 19, col = "darkgray")
abline(h = 0, col = "red", lwd = 2)


# === Homoscedasticity (Equal Variance) ===
library(lmtest)
bptest(model_ols)

# === Multicollinearity ===
library(car)
vif(model_ols)

olsrr::ols_vif_tol(model_ols)
car::vif(model_ols)

#This test show that pff_manZoneZone, and coverage_groupCover is highly correlated which make sense 
#because Cover-2, Cover-3, Cover-6, and Quarters are all zone-based defenses.

#Re-run 
model_ols1 <- lm(
  yardsGained ~ fourthDown + yardsToGo + quarter +
    absoluteYardlineNumber + offenseFormation +
    playAction + coverage_group+offenseFormation*coverage_group+offenseFormation*absoluteYardlineNumber,
  data = plays_clean
)
summary(model_ols1)

#Testing interaction factors
model_ols2 <- lm(
  yardsGained ~ fourthDown + yardsToGo + quarter + absoluteYardlineNumber +
    offenseFormation + playAction + coverage_group +
    playAction*coverage_group + yardsToGo*offenseFormation +
    playAction*offenseFormation + quarter*coverage_group,
  data = plays_clean
)


# === Linearity ===
plot(model_ols1, which = 1)  # Residuals vs Fitted

# === Normality of Residuals ===
plot(model_ols1, which = 2)  # Q–Q plot


# Q–Q plot
qqnorm(residuals(model_ols1))
qqline(residuals(model_ols1), col = "red", lwd = 2)

# Histogram
hist(residuals(model_ols1), breaks = 50, col = "lightblue",
     main = "Distribution of Residuals", xlab = "Residuals")

#Scatter_Plot
plot(model_ols1$fitted.values, residuals(model_ols1),
     xlab = "Fitted Values (Predicted Yards)",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values",
     pch = 19, col = "darkgray")
abline(h = 0, col = "red", lwd = 2)


# === Homoscedasticity (Equal Variance) ===
library(lmtest)
bptest(model_ols1)

# === Multicollinearity ===
library(car)
vif(model_ols1)

olsrr::ols_vif_tol(model_ols1)
car::vif(model_ols1)

# === Outliers / Influential Points ===
plot(model_ols, which = 5)
influence.measures(model_ols1)

# === Independence of Errors ===
library(car)
durbinWatsonTest(model_ols1)

# =========================================================
# STEP 15: CHECK CLUSTERING (ICC) BY GAME
# =========================================================

library(lme4)
icc_model <- lmer(
  yardsGained ~ fourthDown + yardsToGo + quarter +
    absoluteYardlineNumber + offenseFormation +
    playAction + coverage_group + pff_manZone +
    (1 | gameId),
  data = plays_clean
)

var_comp <- as.data.frame(VarCorr(icc_model))
icc <- var_comp$vcov[var_comp$grp == "gameId"] / sum(var_comp$vcov)
icc

# =========================================================
# STEP 16: CHECK CLUSTERING (ICC) BY OFFENSIVE TEAM
# =========================================================

plays_clean1 <- subset(plays, select = c(
  yardsGained, yardsToGo, quarter, absoluteYardlineNumber,
  offenseFormation, playAction, pff_passCoverage, pff_manZone,
  gameId, fourthDown, possessionTeam
))

# Those missing plays are almost certainly structural (kneels, spikes, penalties, or null plays)
# rather than systematically linked to performance or strategy. Therefore I drop the NA rows.
plays_clean1 <- na.omit(plays_clean1)

plays_clean1$coverage_group <- forcats::fct_collapse(
  plays_clean1$pff_passCoverage,
  Cover_3 = c("Cover-3", "Cover-3 Cloud Left", "Cover-3 Cloud Right",
              "Cover-3 Seam", "Cover-3 Double Cloud"),
  Cover_6 = c("Cover 6-Left", "Cover-6 Right"),
  Cover_1 = c("Cover-1", "Cover-1 Double"),
  Other_Rare = c("Bracket", "Miscellaneous", "Prevent", "2-Man", "Goal Line")
)

# Random intercept by offensive team
icc_offense <- lmer(
  yardsGained ~ fourthDown + yardsToGo + quarter +
    absoluteYardlineNumber + offenseFormation + playAction +
    coverage_group + pff_manZone + (1 | possessionTeam),
  data = plays_clean1
)

var_comp_off <- as.data.frame(VarCorr(icc_offense))
icc_possession <- var_comp_off$vcov[var_comp_off$grp == "possessionTeam"] / sum(var_comp_off$vcov)
icc_possession

# =========================================================
# END OF SCRIPT
# =========================================================



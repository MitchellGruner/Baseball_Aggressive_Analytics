"# For this project, I am statistically analyzing trends in data that
# point to my hypothesis that more home runs lead to more strikeouts.  
# Home runs are hit by players who are aggressive, which should 
# ultimately lead to more strikeouts.  I will also analyze whether 
# players are trending towards more of an aggressive style of play
# as the years progress.

# Supplying the data.frame with data from 'Kaggle.com', which is stored
# in the 'data' variable."
data <- read.csv(file.choose())
data

"# I am only concerned about the year, the number of at-bats, the home run totals 
# for each player, and the number of strikeouts for this hypothesis, so I therefore
# created a new data.frame which holds this information in a new variable
# called 'df'."
year <- data$yearID
at.bats <- data$AB
home.runs <- data$HR
strike.outs <- data$SO

"# Personalizing the data.frame's columns."
df <- data.frame(Year=year, At_Bats=at.bats, Home_Runs=home.runs, Strikeouts=strike.outs)
df <- na.omit(df)

"# Mean statistics for batters from every year (1871 - 2015)."
all.batters.mean.home.runs <- mean(df$Home_Runs, na.rm = TRUE)
all.batters.mean.strikeouts <- mean(df$Strikeouts, na.rm = TRUE)
all.batters.mean.at.bats <- mean(df$At_Bats, na.rm = TRUE)

all.batters.AB.per.HR <- all.batters.mean.at.bats / all.batters.mean.home.runs
all.batters.AB.per.SO <- all.batters.mean.at.bats / all.batters.mean.strikeouts

all.batters.SO.per.HR <- all.batters.mean.strikeouts / all.batters.mean.home.runs

"# Now that we have the means for all the data, we can figure out how
# this relates to 'early-baseball' (the first half of the baseball era),
# 'middle-baseball' (the era between early and late baseball), and 
# 'late-baseball' (from 1995 - 2015 (home run era))."

middle.baseball.era <- (1871 + 2015) / 2       # 1943

"# 'Early-baseball' statistics."
early.years <- df$Year <= 1943
early.years.df <- na.omit(df[early.years,])

early.batters.mean.home.runs <- mean(early.years.df$Home_Runs, na.rm = TRUE)
early.batters.mean.strikeouts <- mean(early.years.df$Strikeouts, na.rm = TRUE)
early.batters.mean.at.bats <- mean(early.years.df$At_Bats, na.rm = TRUE)

"# early.batters.mean.at.bats = 165.8203     ('Early-baseball' had 15.85 more at-bats than)
# all.batters.mean.at.bats   =  149.9703      the mean at-bats from all batters)"

early.batters.AB.per.HR <- early.batters.mean.at.bats / early.batters.mean.home.runs
early.batters.AB.per.SO <- early.batters.mean.at.bats / early.batters.mean.strikeouts

early.batters.SO.per.HR <- early.batters.mean.strikeouts / early.batters.mean.home.runs

"# early.batters.AB.per.HR = 114.5657        From this data, we can clearly see that batters
# all.batters.AB.per.HR = 50.84938          from the 'early-baseball' era were far less aggressive
# early.batters.AB.per.SO = 11.12945        in their batting style, as the mean batter from this period
# all.batters.AB.per.SO = 6.933489          struck out once in every 11.12945 at-bats (6.933489 for all batters),
#                                           and hit a home run once in every 114.5657 at-bats (50.84938 for all batters).
#                                           Keep in mind that we have not analyzed the other two eras yet, and we can still tell
#                                           that players are becoming more aggressive as time passes."

"# 'Middle-baseball' statistics"
middle.years <- df$Year > 1943 & df$Year < 1995
middle.years.df <- na.omit(df[middle.years,])

middle.batters.mean.home.runs <- mean(middle.years.df$Home_Runs, na.rm = TRUE)
middle.batters.mean.strikeouts <- mean(middle.years.df$Strikeouts, na.rm = TRUE)
middle.batters.mean.at.bats <- mean(middle.years.df$At_Bats, na.rm = TRUE)

middle.batters.AB.per.HR <- middle.batters.mean.at.bats / middle.batters.mean.home.runs
middle.batters.AB.per.SO <- middle.batters.mean.at.bats / middle.batters.mean.strikeouts

middle.batters.SO.per.HR <- middle.batters.mean.strikeouts / middle.batters.mean.home.runs

"# 'Late-baseball' statistics"
late.years <- df$Year >= 1995
late.years.df <- na.omit(df[late.years,])

late.batters.mean.home.runs <- mean(late.years.df$Home_Runs, na.rm = TRUE)
late.batters.mean.strikeouts <- mean(late.years.df$Strikeouts, na.rm = TRUE)
late.batters.mean.at.bats <- mean(late.years.df$At_Bats, na.rm = TRUE)

late.batters.AB.per.HR <- late.batters.mean.at.bats / late.batters.mean.home.runs
late.batters.AB.per.SO <- late.batters.mean.at.bats / late.batters.mean.strikeouts

late.batters.SO.per.HR <- late.batters.mean.strikeouts / late.batters.mean.home.runs


"########### Comparison of the Statistics Between 'early-baseball', 'middle-baseball', and 'late-baseball' ###########
# early.batters.AB.per.HR = 114.5657                Home runs are being hit at an astonishing rate, as the mean batter from the
# middle.batters.AB.per.HR = 43.47473               'early-baseball' era hit a home run every 114.5657 at-bats, whereas the mean
# late.batters.AB.per.HR = 33.04399                 batter from the 'late-baseball' era hit one every 33.04399 at-bats!

# early.batters.AB.per.SO = 11.12945                Early-batters would, on average, go 11.12945 at-bats for every strikeout they
# middle.batters.AB.per.SO = 6.622853               experienced.  Now, batters can only go 5.043229 at-bats (on average) for every
# late.batters.AB.per.SO = 5.043229                 strikeout.  The middle-batters are also trending in the right direction for our
#                                                   analysis."

"# I will make three different data.frames which will contain how many at-bats
# each eligible player (player who has hit a home run) has per home run."
AB_per_HR_early <- c(early.years.df$At_Bats / early.years.df$Home_Runs)
AB_per_HR_middle <- c(middle.years.df$At_Bats / middle.years.df$Home_Runs)
AB_per_HR_late <- c(late.years.df$At_Bats / late.years.df$Home_Runs)

stats.frame.early <- na.omit(data.frame(AB_per_HR_early))
stats.frame.early <- stats.frame.early[!is.infinite(rowSums(stats.frame.early)),]

stats.frame.middle <- na.omit(data.frame(AB_per_HR_middle))
stats.frame.middle <- stats.frame.middle[!is.infinite(rowSums(stats.frame.middle)),]

stats.frame.late <- na.omit(data.frame(AB_per_HR_late))
stats.frame.late <- stats.frame.late[!is.infinite(rowSums(stats.frame.late)),]

"########### Statistical Analysis Between AB per HR for 'early-baseball' and 'late-baseball' ###########"
early.to.late.analysis <- t.test(stats.frame.early, stats.frame.late, alternative = "two.sided", conf.level = 0.95)
early.to.late.analysis      # Welch Two Sample t-test

"# data:  stats.frame.early and stats.frame.late
# t = 56.355, df = 10777, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# 77.32038 82.89307
# sample estimates:
# mean of x mean of y 
# 132.26236  52.15563"

"# The two means (of eligible players) above indicate that on average, a player will hit one home run
# every 132.26236 at-bats.  This mean differs from the previously calculated mean because it only 
# calculates batters that have actually hit a home run.  This is a much higher number than late batters,
# as they hit one home run every 52.15563 at-bats.
#
# If we look at the 95 percent confidence interval, we see that there is a 95 percent chance that an 
# early baseball player would have between 77.32038 and 82.89307 more at-bats per one home run than
# a late baseball player.
#
# Our p-value (< 2.2e-16) is basically 0, which tells us that there is a 0 percent chance that the different
# types of batters in our population are actually equal.
#
# Since our p-value < 0.05, we reject the null hypothesis, and thus are able to conclude that our results are 
# statistically significant!
#
# We can therefore confidently say that batters from 1871 through 1943 hit much fewer home runs than batters from
# 1995 to 2015 with statistics."

"########### Statistical Analysis Between AB per HR for 'middle-baseball' and 'late-baseball' ###########"
middle.to.late.analysis <- t.test(stats.frame.middle, stats.frame.late, alternative = "two.sided", conf.level = 0.95)
middle.to.late.analysis     # Welch Two Sample t-test

"# data:  stats.frame.middle and stats.frame.late
# t = 22.74, df = 28473, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# 16.29232 19.36586
# sample estimates:
# mean of x mean of y 
# 69.98472  52.15563"

"# I will make three different data.frames which will contain how many at-bats
# each eligible player (player who has hit a home run) has per strikeout."
AB_per_SO_early <- c(early.years.df$At_Bats / early.years.df$Strikeouts)
AB_per_SO_middle <- c(middle.years.df$At_Bats / middle.years.df$Strikeouts)
AB_per_SO_late <- c(late.years.df$At_Bats / late.years.df$Strikeouts)

stats.frame.early.SO <- na.omit(data.frame(AB_per_SO_early))
stats.frame.early.SO <- stats.frame.early.SO[!is.infinite(rowSums(stats.frame.early.SO)),]

stats.frame.middle.SO <- na.omit(data.frame(AB_per_SO_middle))
stats.frame.middle.SO <- stats.frame.middle.SO[!is.infinite(rowSums(stats.frame.middle.SO)),]

stats.frame.late.SO <- na.omit(data.frame(AB_per_SO_late))
stats.frame.late.SO <- stats.frame.late.SO[!is.infinite(rowSums(stats.frame.late.SO)),]

"########### Statistical Analysis Between AB per SO for 'early-baseball' and 'late-baseball' ###########"
early.to.late.analysis.SO <- t.test(stats.frame.early.SO, stats.frame.late.SO, alternative = "two.sided", conf.level = 0.95)
early.to.late.analysis.SO    # Welch Two Sample t-test

"# data:  stats.frame.early.SO and stats.frame.late.SO
# t = 66.621, df = 21436, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# 7.681566 8.147272
# sample estimates:
# mean of x mean of y 
# 12.482505  4.568086"

"# From the t-test data, we can clearly see that an early-baseball player has, on average, 12.482505 at-bats per strikeout,
# whereas late-baseball players have 4.568086 at-bats per strikeout on average.
#
# We are also 95 percent confident that early-baseball players have between 7.681566 and 8.147272 more at-bats before they 
# strikeout than late-baseball players.
#
# The p-value is practically 0, which indicates that we can reject the null hypothesis, making our findings 
# statistically significant."

"########### Statistical Analysis Between AB per SO for 'middle-baseball' and 'late-baseball' ###########"
middle.to.late.analysis.SO <- t.test(stats.frame.middle.SO, stats.frame.late.SO, alternative = "two.sided", conf.level = 0.95)
middle.to.late.analysis.SO  # Welch Two Sample t-test

"# data:  stats.frame.middle.SO and stats.frame.late.SO
# t = 54.798, df = 52160, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# 1.866644 2.005128
# sample estimates:
# mean of x mean of y 
# 6.503972  4.568086"   

"# From the material presented above, we have 'soft-data' (dividing all players at-bats by their home-run totals, and 
# comparing the numbers - even if they never hit a home run) and 'hard-data' (statistical analysis).
# By analyzing the 'hard-data', we can therefore come to the statistical conclusion that players have become more aggressive
# over time, because fewer at-bats are required to hit a home run and at-bats per strikeout are lower."
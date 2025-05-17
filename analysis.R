# In R, any text after a # is a "comment." These will be ignored by
# R, and are extremely useful for making notes about how your code
# works, what you are doing, and why you are doing it.

# First, we are going to load packages. An R package is an extension to the
# language that provides additional functionality. The only library we'll
# be using today is "tidyverse", which provides more intuitive data loading,
# manipulation, and plotting code.

library(tidyverse)

# Next, we will read in the data. This data comes from the Integrated Public Use
# Microdata Sample (https://usa.ipums.org), which is an anonymized sample of individual
# responses to the American Community Survey, an annual survey conducted by the US
# Census Bureau. We have a random sample of 50,000 responses.

# The read_csv function reads data from a Comma Separated Values (CSV) file. The
# here::here function finds data in the same directory as the R file. The functions
# in R work just like functions in math: f(g(x)) means apply function g to x, then
# apply function f to the result of that operation.
data = read_csv(here::here("data", "commute_time.csv"))

# We can look at the first few rows of our data to get some idea what we're working with
head(data)

# The data have the following columns:
#  year: year of survey
#  age: age of respondent
#  sex: sex of respondent (Census Bureau only allows Male or Female)
#  college: binary variable, did respondent go to four-year college
#  trantime: commute time
#  incwage_cpiu_2022: personal (not household) income, adjusted for inflation to 2022
#    dollars using the Consumer Price Index for All Urban Consumers (CPI-U)

# We can start with some descriptive statistics to begin to answer our first research question,
# how prevalent is extreme commuting?
# data$trantime refers to the trantime column in the data table
mean(data$trantime)

median(data$trantime)

# What does the relationship between the mean and median of commute time tell us about
# the distribution of commute time?

# We can verify that by making a histogram.
# We are going to use the ggplot (grammar of graphics plot) function to make our
# plot. In ggplot you specify an "aesthetic" which defines how different attributes
# of the data map onto the plot, and then you specify "geometries" which is how
# those data are rendered. The pipe operator |> is another way to write function
# calls; whatever is on the left will become input to what is on the right (or below).
# Here, we map the travel time onto the x axis. Since we are making a histogram, the
# y axis is the number of respondents in each bin.
data |>
    ggplot(aes(x=trantime)) +
    geom_histogram()

# What does this plot tell you about the distribution of commute times in our sample?

# We can also calculate the proportion of respondents with extreme commutes
# which we define as more than 60 minutes.

# data$trantime > 60 creates a new vector (list of numbers) that are 1 for everyone with
# a commute greater than 60 minutes, and 0 otherwise; the mean of this is the proportion
# with a commute greater than 60 minutes.
mean(data$trantime > 60)

# We can also calculate a confidence interval
# (note: calculating confidence intervals with ACS data is actually more complicated than
# this due to the survey design, so this is only an approximation.)
prop.test(sum(data$trantime > 60), nrow(data))

# We are 95% confident that between 3.59% and 3.92% of American commuters have commutes
# over an hour.

# Does this differ by sex?
# A good way to evaluate differences like this is with a boxplot. We can do
# separate boxplots by sex very easily.
data |>
    ggplot(aes(x=sex, y=trantime)) +
        geom_boxplot()

# There do not appear to many differences in commute time between men and women overall,
# but the extreme commutes don't show up very well in a boxplot.
# We can also do a hypothesis test to see if the proportions differ. This requires a
# little data reshaping.
# first, calculate proportions
data |>
    group_by(sex) |>
    summarize(prop_extreme=mean(trantime > 60)) |>
    ungroup()

# next, we can calculate the hypothesis test. This requires the data to be in a slightly
# different format, a matrix of counts of "successes" (extreme commutes) and "failures"
# (non-extreme commutes).
mtx = data |>
    group_by(sex) |>
    summarize(successes=sum(trantime > 60), failures=sum(trantime <= 60)) |>
    ungroup() |>
    select(-sex) |>
    as.matrix()

mtx
prop.test(mtx)
# How confident are we that men are more likely to have extreme commutes than women?

# Has extreme commuting increased over time?
# We can make a line plot of extreme commuting by year, again using the group_by function.
data |>
    group_by(year) |>
    summarize(proportion_extreme=mean(trantime > 60)) |>
    ggplot(aes(x=year, y=proportion_extreme)) +
        geom_line()

# We can clean that up a bit
data |>
    group_by(year) |>
    summarize(proportion_extreme=mean(trantime > 60)) |>
    ggplot(aes(x=year, y=proportion_extreme)) +
        geom_line() +
        xlab("Year") +
        ylab("Percent extreme commuters") +
        scale_y_continuous(labels=scales::percent)

# What does this tell us about extreme commuting over time?

# Does this differ by men and women?
# It's very easy to put two lines on the same plot.
data |>
    group_by(year, sex) |>
    summarize(proportion_extreme=mean(trantime > 60)) |>
    ggplot(aes(x=year, y=proportion_extreme, color=sex)) +
        geom_line() +
        xlab("Year") +
        ylab("Percent extreme commuters") +
        scale_y_continuous(labels=scales::percent)

# Do the amounts of extreme commuting differ among men and women?
# Do the trends?

# Are differences explained by age, education level or income?
# The best tool for answering a question like this is a regression. The coefficients
# in a regression indicate the relationship between the independent and dependent variables,
# holding all the other independent variables constant. So we can "take out" or
# control for the relationship between extreme commuting and income, education, etc.
# and see if there are still differences by sex.

# We will start by modeling commute time (not specifically extreme commuting) using the
# simplest type of regression model, a linear regression.
model = lm(trantime ~ sex + age + college + incwage_cpiu_2022 + factor(year), data)
summary(model)

# Controlling for these factors, do men have longer commutes than women?
# By how much?

# This models commute time overall. If we specifically wanted to look at extreme
# commuting, we could use another kind of regression, logistic regression.
# Logistic regression models the "odds" of a binary outcome (in this case,
# yes or no commuting is extreme).

# We'll start by creating an extreme_commute binary variable
data$extreme_commute = data$trantime > 60

logistic_model = glm(extreme_commute ~ sex + age + college + incwage_cpiu_2022 + factor(year), data,
    family=binomial(link="logit"))
summary(logistic_model)

# The coefficients in a logistic regression are not very useful.
# An easier to interpret version is the odds ratio, which is easy
# to calculate.
exp(coef(logistic_model))

# This is showing that men are 49.7% more likely to have
# extreme commutes than women (1.497 times more likely).
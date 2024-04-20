library(mars)
library(ISLR)
library(MASS)

# test1
data("mtcars")
mc1 = mars.control(Mmax = 10)
fit1 <- mars(mpg ~ wt + hp, data = mtcars, control=mc1)
predict1 = predict(fit1)
predict1
summary.mars(fit1)
anova.mars(fit1)
plot.mars(fit1)
print.mars(fit1)


#test2
data("airquality")
# Clean the dataset to remove NA values for simplicity
airquality_clean <- na.omit(airquality)

mc_airquality = mars.control(Mmax = 10)
fit_airquality <- mars(Ozone ~ Solar.R + Wind + Temp, data = airquality_clean, control=mc_airquality)
predict_airquality = predict(fit_airquality)
predict_airquality
summary.mars(fit_airquality)
anova.mars(fit_airquality)
plot.mars(fit_airquality)
print.mars(fit_airquality)



## test3 dataset of gapminder
#test3
# Load the gapminder dataset
if (!requireNamespace("gapminder", quietly = TRUE)) {
  install.packages("gapminder")
}
library(gapminder)
data("gapminder")

gapminder_2007 <- subset(gapminder, year == 2007)

# Setting control parameters for the MARS model
mc_gapminder <- mars.control(Mmax=10)

# Fitting the MARS model to predict life expectancy based on GDP per capita and population
fit_gapminder <- mars(lifeExp ~ gdpPercap + pop, data=gapminder_2007, control=mc_gapminder)

# Making predictions with the fitted model
predict_gapminder <- predict(fit_gapminder)

# Displaying predictions
predict_gapminder

# Summarizing the fitted model
summary.mars(fit_gapminder)

# Analyzing the model's components with ANOVA
anova.mars(fit_gapminder)

# Plotting the MARS model
plot.mars(fit_gapminder)

# Printing the model's details
print.mars(fit_gapminder)









# load the packages we'll need
library(plyr); library(dplyr)
library(rstanarm)
library(bayesplot)
library(ggplot2)
library(readr)

# read in raw data
# specify directory where the data are stored
in_dir <- '/home/directory/'
# read in the data from csv
steps_weather <- read_csv(paste0(in_dir, 'steps_weather.csv'))

# first date
min(steps_weather$date)
# last date
max(steps_weather$date)

# what does it look like?
head(steps_weather) 

# histograms of our 2 focal variables
# daily total step count
hist(steps_weather$daily_total, breaks = 50)
# avg temp in degrees Celsius
hist(steps_weather$temp, breaks = 50)

#  simple model
# we get different results every time. we'll have to save out the model to 
# have access to a stable set of parameters for the blog post...
fit_1 <- stan_glm(daily_total ~ temp, data = steps_weather) 
summary(fit_1)

# extract the simulations from the model object
sims <- as.matrix(fit_1)

# what does it look like?
# we have 4000 posterior simulations of parameters 
# these simulations express the uncertainties summarized in 
# the above model output
head(sims)

# the means and sd's from these simulations are the 
# point estimates displayed in the model summary
# mean and intercept - matches summary table
mean(sims[,1])
sd(sims[,1])
# mean and sd temp - matches summary table
mean(sims[,2])
median(sims[,2])
sd(sims[,2])
# mean and sd sigma - matches summary table
mean(sims[,3])
sd(sims[,3])

# plot the posterior distribution of the parameters

# area plots
# all of the parameters
# scale of parameters is so varied
# that it's hard to see the details
color_scheme_set("red")
plot_title <- ggtitle("Posterior distributions",
                      "with medians and 90% intervals")
mcmc_areas(sims, prob = 0.90) + plot_title

# area plot for temperature parameter
mcmc_areas(sims,
          pars = c("temp"),
          prob = 0.90) + plot_title

# 90% credible intervals
# big-ups to:
# https://rstudio-pubs-static.s3.amazonaws.com/454692_62b73642b49840f9b52f46ceac7696aa.html
# using rstanarm function and model object
posterior_interval(fit_1, pars = "temp", prob=.9)
# calculated directly from simulations from posterior distribution
quantile(sims[,2], probs = c(.05,.95))  

# plot a bunch of regression lines from the simulations
# adapted from: 
# https://www.tjmahr.com/visualizing-uncertainty-rstanarm/

# Coercing a model to a data-frame returns a 
# data-frame of posterior samples 
# One row per sample.
fits <- fit_1 %>% 
  as_tibble() %>% 
  rename(intercept = `(Intercept)`) %>% 
  select(-sigma)
# what does the dataframe look like?
head(fits)

# aesthetic controllers
n_draws <- 500
alpha_level <- .15
color_draw <- "grey60"
color_mean <-  "#3366FF"

# make the plot
ggplot(steps_weather) + 
  # first - set up the chart axes from original data
  aes(x = temp, y = daily_total ) + 
  # restrict the y axis to focus on the differing slopes in the 
  # center of the data
  coord_cartesian(ylim = c(15000, 18000)) +
  # Plot a random sample of rows from the simulation df
  # as gray semi-transparent lines
  geom_abline(
    aes(intercept = intercept, slope = temp), 
    data = sample_n(fits, n_draws), 
    color = color_draw, 
    alpha = alpha_level
  ) + 
  # Plot the mean values of our parameters in blue
  # this corresponds to the coefficients returned by our 
  # model summary
  geom_abline(
    intercept = mean(fits$intercept), 
    slope = mean(fits$temp), 
    size = 1, 
    color = color_mean
  ) +
  geom_point() + 
  # set the axis labels and plot title
  labs(x = 'Average Daily Temperature (Degrees Celsius)', 
       y = 'Daily Total Steps' , 
       title = 'Visualization of 500 Regression Lines From the Posterior Distribution')


# posterior predictive checks
# https://mc-stan.org/bayesplot/reference/pp_check.html
# http://mc-stan.org/rstanarm/reference/pp_check.stanreg.html

# The idea behind posterior predictive checking is simple: if a model is a 
# good fit then we should be able to use it to generate data that looks a lot 
# like the data we observed. To generate the data used for posterior predictive 
# checks (PPCs) we simulate from the posterior predictive distribution. 

# posterior predictive check - for more info see:
# https://mc-stan.org/bayesplot/reference/pp_check.html
# http://mc-stan.org/rstanarm/reference/pp_check.stanreg.html
pp_check(fit_1, "stat")


# Using the Model to Make Predictions
# Using the Model to Make Predictions
# Using the Model to Make Predictions
# Using the Model to Make Predictions
# Using the Model to Make Predictions
# Using the Model to Make Predictions

# define new data from which we will make predictions
# we make predictions for when the average daily temperature 
# is 10 degrees Celsius 
new <- data.frame(temp = 10)
new
# simply using the single point summary of the posterior distributions
# for the model coefficients (those displayed in the model summary above)
y_point_est <- predict(fit_1, newdata = new)
# same prediction "by hand"
y_point_est_2 <- mean(sims[,1]) + mean(sims[,2])*new

# these guys are the same
predict(fit_1, newdata = new)
mean(sims[,1]) + mean(sims[,2])*new

# linear predictor with uncertainty using posterior_linpred
# linear predictor with uncertainty using posterior_linpred
# linear predictor with uncertainty using posterior_linpred
# linear predictor with uncertainty using posterior_linpred

y_linpred <- posterior_linpred(fit_1, newdata = new)
# compute it "by hand"
# we use the sims matrix we defined above 
# sims <- as.matrix(fit_1)
y_linpred_2 <- sims[,1] + (sims[,2]*10)  

# check - these are the same!
plot(y_linpred,y_linpred_2)
cor.test(y_linpred, y_linpred_2)

# predictive distribution for a new observation using posterior_predict
# predictive distribution for a new observation using posterior_predict
# predictive distribution for a new observation using posterior_predict
# predictive distribution for a new observation using posterior_predict
set.seed(1)
y_post_pred <- posterior_predict(fit_1, newdata = new)

# calculate it "by hand"
n_sims <- nrow(sims)
sigma <- sims[,3]
set.seed(1)
y_post_pred_2 <- as.numeric(sims[,1] + sims[,2]*10) + rnorm(n_sims, 0, sigma)

# check - these are the same!
plot(y_post_pred, y_post_pred_2)
cor.test(y_post_pred, y_post_pred_2)

# Plotting the Predictive Distributions
# Plotting the Predictive Distributions
# Plotting the Predictive Distributions
# Plotting the Predictive Distributions
# Plotting the Predictive Distributions
# Plotting the Predictive Distributions

# create a dataframe containing the values from the posterior distributions 
# of the predictions of daily total step count at 10 degrees Celcius
post_dists <- as.data.frame(rbind(y_linpred, y_post_pred)) %>% 
      setNames(c('prediction'))
post_dists$pred_type <- c(rep('posterior_linpred', 4000),
                          rep('posterior_predict', 4000))
y_point_est_df = as.data.frame(y_point_est)

# 70's colors - from NineteenEightyR package
# https://github.com/m-clark/NineteenEightyR
pal <- c('#FEDF37', '#FCA811', '#D25117', '#8A4C19', '#573420')

ggplot(data = post_dists, aes(x = prediction, fill = pred_type)) + 
  geom_histogram(alpha = .75, position="identity") + 
  geom_point(data = y_point_est_df,
             aes(x = y_point_est,
                 y = 100,
                 fill = 'Linear Point Estimate'),
             color =  pal[2],
             size = 4,
             # alpha = .75,
             show.legend = F) +
  scale_fill_manual(name = "Prediction Method",
                    values = c(pal[c(2,3,5)]),
                    labels = c(bquote(paste("Linear Point Estimate ", italic("(predict)"))),
                               bquote(paste("Linear Prediction With Uncertainty " , italic("(posterior_linpred)"))),
                               bquote(paste("Posterior Predictive Distribution ",  italic("(posterior_predict)"))))) +
  # set the plot labels and title
  labs(x = "Predicted Daily Total Step Count", 
       y = "Count", 
       title = 'Uncertainty in Posterior Prediction Methods')   +
  theme_bw()


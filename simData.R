
##-----------------------------------------------------------------------
##                       Simulate data LME model                       --
##  Function that simulates the data for a linear mixed-effects model  --
##             with random intercepts for participants and items       --
##            and random slopes for the effect within participants     --
##      Created by Francesco Pupillo, Goethe University Frankfurt      --
##                      created = Tue Jun  1  2021                     --
##                            last update =                            --
##                      Wed Jun  2 16:51:56 2021                       --
##-----------------------------------------------------------------------

# load packages
library("lme4") # model specification / estimation
library("lmerTest") # provides p-values in the output
library("tidyverse") # data wrangling and visualisation
library("bannerCommenter")

txt<-("Function that simulates the data for a linear mixed-effects model
      with random intercepts for participants and items
      and random slopes for the effect within participants")
banner("Simulate data LME model",  txt,
       "Created by Francesco Pupillo, Goethe University Frankfurt", 
       "created = Tue Jun  1  2021",
       "last update = ", date(), emph = F, center = T, bandChar = "-")


# ensure this script returns the same results on each run
#set.seed(8675309)

my_sim_data <- function(
  
  n_subj = 100, # number of subjects
  group1= n_subj/2, # number of group1 participants
  group2 = n_subj/2, # number of group2 participants
  
  # names of the conditions
  cond1 = "lowPE",
  cond2 = "highPE",
  
  beta_0 = 700, # intercept - grand mean
  beta_1 = 30, # fixed slope, population level

  n_items = 50, # number of items, for both conditions
  
  tau_0 = 100, # by-subject random intercept sd.
  # Check if it is sd, because than it will be squared into variance
  
  # by item random intercept sd.
  omega_0 = 80,
  
  # aoutocorrelation between items, only for continuous iv variable autocorrelated (e.g. valence)
  autocorrelation = 0.3,
  
  tau_1 = 40, # by-subject random slope sd
  rho = 0.2, # correlation between intercept and slope
  sigma = 200) { # residual (standard deviation). It is dispersion in generalised models

  # autocorrelation
  # generate a matrix
  # tmp.r <- matrix(autocorrelation, n_items, n_items)
  # tmp.r <-tmp.r^abs(row(tmp.r)-col(tmp.r))

  # need to set the autocorrelation
  items <- data.frame(
    item_id = seq_len(n_items), condition = rep(c(cond1, cond2), each = n_items/2),
   # continuousIV = MASS::mvrnorm(1, rep(valenceMeanYA,n_items), tmp.r) # the autocorrelation is between 3 consecutiive assessments
   # random intercept of items 
   O_0i = rnorm(n = n_items, mean = 0,sd = omega_0 )
      )
  
  # effect-code condition
  items$X_i <- recode(items$condition, "lowPE" = 0, "highPE" = 1)
  
  # variance-covariance matrix
  cov_mx <- matrix(
    c(tau_0^2, rho * tau_0 * tau_1,
      rho * tau_0 * tau_1, tau_1^2 ),
    nrow = 2, byrow = TRUE)
  
  # by-subject random effect
  subjects_rfx<- data.frame(
                         MASS::mvrnorm(n =n_subj, # drawn from multivariate distribution
                                       mu = c(T_0s = 0, T_1s = 0), # mean zero
                                       Sigma = cov_mx)) # variance as covariance matrix
  # combine with subject IDs
  subjects <- data.frame(subj_id = seq_len(n_subj),
                         subjects_rfx)
  
  # simulate trials
  trials<- crossing(subjects, items) %>%
    mutate(e_si = rnorm(nrow(.), mean = 0, sd = sigma)) %>% # trial level random error, drawn from 
    # normal distribution with mean 0 and sd = signma
    select(subj_id, item_id, condition , X_i, everything())
  
  # calculate the response variable
  dat_sim <- trials %>%
    mutate(RT = beta_0+ T_0s + O_0i+(beta_1 + T_1s)*X_i  + e_si) %>%
    select(subj_id, item_id, condition, X_i ,RT)
  
  return(dat_sim)
}


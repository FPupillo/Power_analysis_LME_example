##---------------------------------------------------------------
##                    Power analysis for LME                   --
##        Simulation-based Power analysis for LME models       --
##  Created by Francesco Pupillo, Goethe University Frankfurt  --
##                  created = Tue Jun  1  2021                 --
##                        last update =                        --
##                  Wed Jun  2 16:57:11 2021                   --
##---------------------------------------------------------------

library("bannerCommenter")
library("lme4")
library("lmerTest")
library("reshape")

txt<-("Simulation-based Power analysis for LME models")
banner("Power analysis for LME",  txt,
       "Created by Francesco Pupillo, Goethe University Frankfurt", 
       "created = Tue Jun  1  2021",
       "last update = ", date(), emph = F, center = T, bandChar = "-")

# source the function to simulate data
source("simData.R")

# set number of runs
n_runs<-50

# set number of subjects and items
n_subj <- 100 # number of subjects

n_items <-100

# define variables
# create an empty vector
sims<-vector()

# the simulations run in steps of two, since whe have two groups
for (j in seq(10,n_subj,10)){
  for (i in seq(50,n_items,20)){
    
    # p value
    p<-NA
    
    for (n in 1:n_runs){
    # simulate
    data_sim<-my_sim_data(n_subj = j, n_items = i)
    # fit
    mod_sim <- lmer(RT ~ condition + (1 | item_id) + (1 + condition | subj_id),
                    data_sim)
    # extract p
    param<-summary(mod_sim)
    p[n]<-coefficients(param)[10]
  }
  
    # calculate power
    power<-length(p[p<0.05])/n_runs
    sims<-rbind(sims, c(j, i, power))
    }
} 

powerAn<-as.data.frame(sims)
names(powerAn)<-c("Subjects", "items", "power")

powerWide<-reshape(powerAn, idvar="Subjects", timevar="items", direction = "wide")



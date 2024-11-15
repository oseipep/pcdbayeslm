# To run the package cpcbayeslm:
rm(list = ls()) # clean the environment
library(devtools)

# move outside the cpcbayeslm folder:
setwd("../")
# Run the command:
devtools::install("cpcbayeslm/")
library(cpcbayeslm)

# Get the variables for the run:
iter = 100 # total number of samples
noitems = 3 # number of items for the comparison
nocompars = rep(2, choose(noitems,2)) # a vector of number of comparisons between items
scores = 1:noitems-mean(1:noitems) # the true score/outcome of the comparisons
vars = 0.25
muprior = matrix(0,nrow = noitems, ncol = 1) # prior mean
varprior = vars*diag(noitems)
#Edges = c(1,2,1,3,2,3) # a vector containing the edges of the comparison
Edges = cpcbayeslm:::vedges(noitems)
# you can get using cpcbayes:::vedges(noitems)
prior = "conju" # the type of prior to use: conjugate, semi-conjugate, flat or reference prior
type = "simulated" # the type of data given either simulated or real data
# if it is real then data would be the actual comparison outcomes
res <- bayeslmsamples(iter = iter, noitems = noitems, nocompars = nocompars,
                      scores = scores, vars = vars, muprior = muprior,
                      varprior = varprior, Edges = Edges,
                      prior = prior, datatype = type)




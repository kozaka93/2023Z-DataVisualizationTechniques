#########################################################################
###                                                                   ###
###       klas12b.r                                      ################
###       version May 30, 2019                           ################
###                                                                   ###
### This is an analysis of the data set collected by Andrea Knecht    ###
###                                                                   ###
### Andrea Knecht, 2008.                                              ###
### Friendship Selection and Friends' Influence.                      ###
### Dynamics of Networks and Actor Attributes in Early Adolescence.   ###
### PhD dissertation, University of Utrecht.                          ###
###                                                                   ###
### and analyzed in                                                   ###
### Snijders, T.A.B., Steglich, C.E.G., and van de Bunt, G.G., 2010.  ### 
### Introduction to actor-based models for network dynamics.          ###
### Social Networks, 32, 44-60.                                       ###
#########################################################################

library(RSiena)

#read data
# http://www.stats.ox.ac.uk/~snijders/siena/klas12b.zip
setwd("where you have the data") 

klas12b.friend1 <- as.matrix(read.table("klas12b-net-1.dat"))
klas12b.friend2 <- as.matrix(read.table("klas12b-net-2.dat"))
klas12b.friend3 <- as.matrix(read.table("klas12b-net-3.dat"))
klas12b.friend4 <- as.matrix(read.table("klas12b-net-4.dat"))

klas12b.deli <- as.matrix(read.table("klas12b-delinquency.dat"))
klas12b.demo <- as.matrix(read.table("klas12b-demographics.dat"))
klas12b.advice <- as.matrix(read.table("klas12b-advice.dat"))
klas12b.primary <- as.matrix(read.table("klas12b-primary.dat"))

# identify missing values (see 'klasdata-readme.txt'):
klas12b.friend1[klas12b.friend1==9] <- NA
klas12b.friend2[klas12b.friend2==9] <- NA
klas12b.friend3[klas12b.friend3==9] <- NA
klas12b.friend4[klas12b.friend4==9] <- NA
klas12b.primary[klas12b.primary==9] <- NA
klas12b.demo[klas12b.demo==0] <- NA
klas12b.advice[klas12b.advice==0] <- NA

# find out number of actors in data sets:
(klas12b.numberActors <- dim(klas12b.demo)[1]) # 26

# identify dependent variable for the analysis:
klas12b.friends <- sienaDependent(array(
        c(klas12b.friend1, klas12b.friend2, klas12b.friend3, klas12b.friend4),
  dim=c(klas12b.numberActors, klas12b.numberActors, 4)))
klas12b.delinq <- sienaNet(klas12b.deli, type="behavior")

# identify covariates for the analysis:
klas12b.sex <- coCovar(klas12b.demo[, 1]) # 1=F, 2=M
klas12b.age <- coCovar(klas12b.demo[, 2]) # years
klas12b.eth <- coCovar(klas12b.demo[, 3]) # ethnicity # 1 = Dutch, 2 = other
klas12b.reli <- coCovar(klas12b.demo[, 4]) # Religion # 1 = Christian, 2 = non-religious, 3 = non-Christian
klas12b.adv <- coCovar(klas12b.advice[, 1]) # advice; 4 is low, 8 is high
klas12b.prim <- coDyadCovar(klas12b.primary)

# Composition change:
composition <- sienaCompositionChangeFromFile("klas12b-present.dat")

# Define data set:
klas12b.data <- sienaDataCreate(friends=klas12b.friends,
#                   delinq= klas12b.delinq, 
# this is for if you wish to analyse the networks and behavior study
                   sex=klas12b.sex, primary=klas12b.prim, composition)

# Specify model as in Snijders, van de Bunt & Steglich, 2010:
klas12b.effects <- getEffects(klas12b.data)
klas12b.effects <- includeEffects(klas12b.effects, transTrip, transTies, cycle3)
klas12b.effects <- includeEffects(klas12b.effects, inPopSqrt, outActSqrt, outPopSqrt)
klas12b.effects <- includeEffects(klas12b.effects, X, interaction1="primary")
klas12b.effects <- includeEffects(klas12b.effects, egoX, altX, sameX, interaction1="sex")
klas12b.alg  <- sienaAlgorithmCreate(projname = "klas_12b", n3 = 1000, seed = 1234)
klas12b.alg1  <- sienaAlgorithmCreate(projname = "klas_12b", nsub = 1, n2start = 5000,
                        n3 = 5000, seed = 4321)

(ans0 <- siena07(klas12b.alg, data=klas12b.data, effects=klas12b.effects))
(ans0 <- siena07(klas12b.alg1, data=klas12b.data, effects=klas12b.effects,
                    prevAns=ans0))
# Due to improved estimation techniques since 2010,
# there are some differences; but these are minor.

# Now with a specification that is more up to date.
# I also tried reciAct, but it was not significant.

klas12b.eff <- getEffects(klas12b.data)
klas12b.eff <- includeEffects(klas12b.eff, gwespFF)
klas12b.eff <- includeInteraction(klas12b.eff, gwespFF, recip)
klas12b.eff <- includeEffects(klas12b.eff, inPopSqrt, outActSqrt, outPopSqrt)
klas12b.eff <- setEffect(klas12b.eff, reciAct, include=FALSE)
klas12b.eff <- includeEffects(klas12b.eff, X, interaction1="primary")
klas12b.eff <- includeEffects(klas12b.eff, egoX, altX, sameX, interaction1="sex")
klas12b.eff
(ans1 <- siena07(klas12b.alg, data=klas12b.data, effects=klas12b.eff))
(ans1 <- siena07(klas12b.alg1, data=klas12b.data, effects=klas12b.eff,
                    prevAns=ans1))
                    
save(ans0, ans1, klas12b.eff, klas12b.effects, file='klas12Results.RData')

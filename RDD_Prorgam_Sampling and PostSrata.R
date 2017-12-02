                                        ######## Selecting and Analyzing RDD Survey ######
                                                         
                                                               ## and 
        
                                                    #  Poststratification
    

# Poststrata are defined by age group crossed with EDUC.
require(PracTools)
require(survey)
library(pps)
                                        
# I )  to design a postrata variable 

#to determine the number of Strata and to allocate individual to each startum

RDD$PS <- (RDD$AgBr - 1)*3 + RDD$EDUC  # With 3 is the number of EDUC categories

# to add the strata variable to the original total Database
N.PS <- table(PS = RDD$PS)




## II )  Determine the Sample size 

smp.size <- 1000


## III)  Approx . proporional sample size

strat.cts <- as.numeric(table(RDD$Gouv))
strat.ps <- strat.cts/sum(strat.cts)
strat.ps
tabsum(strat.ps)

## stratum sample size 

smp.size.h <- round (strat.ps * smp.size,0)
sum(smp.size.h)

## sort data file by sampling strata and select samples 

RDD <- RDD[order(RDD$Gouv),]

smp.IDs <- ppssstrat(size = RDD$Gouv, strat=RDD$Gouv, n = smp.size.h)

# verify no duplicates in the sample
length (smp.IDs)
length(unique(smp.IDs))

# subset to sampled records

smp.data <- RDD[smp.IDs,]


  # Determine number of rows by Strata 

    
    N <- NROW(RDD)  # need to investigate how to do the nrow by strata
    
    d <- rep(N/smp.size, smp.size) # to estimate the base weight 

    
    f1 <- rep(smp.size/N, smp.size) # to estimate the probability of selection 
    
# IV ) Declare the survey design 
    
    RDD.dsgn <- svydesign(ids = ~0,    # no clusters
                       strata = smp.data$Gouv,    #  strata
                       fpc = ~f1,
                       data = data.frame(smp.data),
                       weights = ~d)


## ---------------------------------V ) ### poststratified design object

    
    ps.dsgn <- postStratify(design = RDD.dsgn,
                        strata = ~PS,
                        population = N.PS,
                        partial=TRUE) # this command is added because few small PS are not found in the smp.data sample

    
    
    #check if all SE are 0

        svytotal(~as.factor(PS), ps.dsgn)

        
# -----------------------------------VI)  To Run the analysis 

# PS standard errors and cv’s
svytotal(~ as.factor(a2), ps.dsgn, na.rm=TRUE)



cv(svytotal(~ as.factor(a2), ps.dsgn, na.rm=TRUE))



#--------------------------------------Redo with Raking instead of Postratification ----------------
#  NOT WORKING YET !!

#Raking
# Create marginal pop totals
N.age <- table(RDD$AgBr)
N.EDUC <- table(RDD$EDUC)
pop.totals <- c('(Intercept)' = N, N.age[-1], N.EDUC[-1])
# create raked weights
rake.dsgn <- calibrate(design = RDD.dsgn,
                       formula = ~as.factor(AgBr) + as.factor(EDUC),
                       calfun = "raking",
                       population = pop.totals)
# raking standard errors and cv’s
svytotal(~ as.factor(a2), rake.dsgn, na.rm=TRUE)


cv(svytotal(~ as.factor(medicaid), rake.dsgn, na.rm=TRUE))






# srs standard error and cv’s
#svytotal(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE)
#cv(svytotal(~ as.factor(medicaid), nhis.dsgn, na.rm=TRUE))







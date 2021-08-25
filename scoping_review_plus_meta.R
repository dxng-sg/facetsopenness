#############################################################################################
##  A meta-analytical synthesis of scoping review
#############################################################################################

# Based and adapted from the script developed by Quintana DS (2015). From pre-registration to publication: a nontechnical primer for conducting a meta-analysis to 
# synthesize correlational data. Front. Psychol. 6:1549. doi: 10.3389/fpsyg.2015.01549

###############################
## Install and load packages ##
###############################

install.packages(c("robumeta", "metafor", "dplyr"))

library("robumeta")
library("metafor")
library("dplyr")


######################
## Import your data ##
######################

#Excel was used to create csv files that captures the correlation coefficients of openness facets with different indices of prejudice and tolerance.
# To update or import new data, follow the instructions below. This will import your data assuming that the file is saved as a csv file in 
# your working directory using the name "yourdata".

yourdata_1 <- read.csv("Meta-synthesis_prejudice_values.csv", header=TRUE)
yourdata_2 <- read.csv("Meta-synthesis_prejudice_aesthetics.csv", header=TRUE)
yourdata_3 <- read.csv("Meta-synthesis_prejudice_feelings.csv", header=TRUE) 
yourdata_4 <- read.csv("Meta-synthesis_prejudice_fantasy.csv", header=TRUE) 
yourdata_5 <- read.csv("Meta-synthesis_prejudice_ideas.csv", header=TRUE) 
yourdata_6 <- read.csv("Meta-synthesis_prejudice_actions.csv", header=TRUE) 

##################################
## Performing the Meta-analysis ##
##################################

# The first step is to transform r to Z and calculate the corresponding sample variances.

dat_values <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_1, slab=paste(authors, year, sep=", "))
dat_aesthetics <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_2, slab=paste(authors, year, sep=", ")) 
dat_feelings <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_3, slab=paste(authors, year, sep=", ")) 
dat_fantasy <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_4, slab=paste(authors, year, sep=", ")) 
dat_ideas <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_5, slab=paste(authors, year, sep=", ")) 
dat_actions <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_6, slab=paste(authors, year, sep=", ")) 

# This performs the r to z transform from the "yourdata" datasets and calculates the corresponding sample variances. "ri" represents the correlation coefficients  
# and "ni" represent the sample sizes.

# Lets have a look at the file again, notice the two new variables at the end. The "yi" variable is the z score transformation and the "vi" variable is the 
# corresponding estimated sampling variance.

View(dat_values)
View(dat_aesthetics)
View(dat_feelings)
View(dat_fantasy)
View(dat_ideas)
View(dat_actions)

# Now you're ready to perform the meta-analysis using a random-effects model. The following commands will print out the data and also calculates and 
#################################################################################
####     first, for the FACET of VALUES and prejudice. print the confidence interval for the amount of heterogeneity (I^2).
#################################################################################

res <- rma.mv(yi, vi, data=dat_values, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
sav<-confint(res)  
sav
W <- diag(1/dat_values$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * res$sigma2 / (res$sigma2 + (res$k-res$p)/sum(diag(P))) ### total I^2
100 * sav$random[1,2:3] / (sav$random[1,2:3] + (res$k-res$p)/sum(diag(P)))  ### CI for the total I^2


# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 12; method: REML)" 

#This line tells us we've used a random-effects model, with 12 studies (i.e., "k") and that the degree of heterogeneity (sigma^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "sigma^2 (estimated amount of total variance): 0.0514 (sqrt = 0.2266)"
# Number of unique studies = 9 (categorised according to study_id)
#I2: 91.37%, CI: 79.98 - 97.34
# "Test for Heterogeneity: 
# Q(df = 11) = 141.5067, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.4699   0.0798   -5.8846   <.0001   -0.6263   -0.3134      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  pi.lb  pi.ub
#-0.438 -0.556 -0.303 -0.736  0.001

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#sigma^2    0.0514  0.0194  0.1773
#sigma      0.2266  0.1391  0.4211

#These two lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_values, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.
#Cook's distance test: studies with values above 4/n, where n is the total number of datapoints, are considered influential cases
x<- cooks.distance(res)
plot(x,type="o",pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
hatvalues(res)
rstandard(res)

# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c(" Christopher et al., (2013)",
            " Christopher et al., (2013)",
            " Ekehammar & Akrami, (2007)",
            " Ekehammar & Akrami, (2007)", 
            " Miller, (2019)",
            " Miller et al., (2012)",
            " Onraet et al., (2011)", 
            " Onraet et al., (2011)",
            "*Averhart, (2012)",
            " Huxley et al., (2015)",
            " Proctor & McCord, (2009)",
            " Szeto et al., (2015)")


forest(res, xlim=c(-2.4,0.7), atransf=transf.ztor,
       at=transf.rtoz(c(-.8,-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_values$ni,dat_values$o_measure,dat_values$outcome),
       ilab.xpos=c(-1.9, -1.8, -1.6),
       ilab.pos=4, 
       digits=c(2,1), cex=.6,efac = 5,addpred = TRUE,showweights = TRUE)
text(-2.4, 14.4, "Facet of Values (NEO-PI-R/3)/ Liberalism (IPIP-based Measures)", pos=4, cex=.9)
text(-2.4, 13.5, "Author(s), Year                               N       Measure       Outcome (Prejudice)", pos=4, cex=.7)
text( 0.7, 13.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 12 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
res <- rma.mv(yi, vi, data=dat_values, 
              random = ~ 1 | study_id,
              mods = ~vi,
              method = "REML") 
res
fsn(yi,vi, data=dat_values,type="Rosenthal") #fail-safe n 

# Egger's regression test (z= -0.7167, p = 0.4736) was not statistically significant so there's no evidence of publication bias 
# according to these tests. And fail safe N test is 2131 which exceed the criterion of 60.

###################################################
### second, for facet of AESTHETICS and PREJUDICE
##############################################################################

res <- rma.mv(yi, vi, data=dat_aesthetics, 
              random = ~ 1 | study_id,
              method = "REML") 
res
sav<-confint(res)  
sav
W <- diag(1/dat_aesthetics$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * res$sigma2 / (res$sigma2 + (res$k-res$p)/sum(diag(P))) ### total I^2
100 * sav$random[1,2:3] / (sav$random[1,2:3] + (res$k-res$p)/sum(diag(P)))  ### CI for the total I^2
predict(res, digits=3, transf=transf.ztor)
 

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 11; method: REML)" 

#This line tells us we've used a random-effects model, with 11 studies (i.e., "k") and that the degree of heterogeneity (sigma^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "sigma^2 (estimated amount of total heterogeneity): 0.0104 (sqrt = 0.1020)"
# No of Levels: 8 (represents the number of unique studies according to study_id)
#I2: 65.09%, CI: 32.82 - 88.97
# "Test for Heterogeneity: 
# Q(df = 10) = 39.7411, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.2746   0.0449   -6.1217   <.0001   -0.3625   -0.1867      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  pi.lb  pi.ub
#-0.268 -0.347 -0.185 -0.457 -0.056

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#sigma^2    0.0104  0.0027  0.0450
#sigma      0.1020  0.0522  0.2121

#These two lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_aesthetics, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.
x<- cooks.distance(res)
plot(x,type="o",pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
hatvalues(res)
rstandard(res)

# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c(" Christopher et al., (2013)",
            "*Christopher et al., (2013)",
            " Ekehammar & Akrami, (2007)",
            " Ekehammar & Akrami, (2007)",
            " Miller, (2019)",
            " Miller et al., (2012)",
            " Onraet et al., (2011)",
            " Onraet et al., (2011)",
            " Huxley et al., (2015)",
            " Proctor and McCord, (2009)",
            " Szeto et al., (2015)") 


forest(res, xlim=c(-1.9,0.55), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_aesthetics$ni,dat_aesthetics$o_measure,dat_aesthetics$outcome),
       ilab.xpos=c(-1.49, -1.4, -1.23),
       ilab.pos = 4,
       digits=c(2,1), cex=.6,efac=4,addpred = TRUE,showweights = TRUE)
text(-1.9, 13.2, "Facet of Aesthetics (NEO-PI-R/3)/ Artistic Interests (IPIP-based Measures)", pos=4, cex=.9)
text(-1.9, 12.5, "Author(s), Year                                N         Measure         Outcome (Prejudice)", pos=4, cex=.7)
text( 0.55, 12.5, "Weight[%]   Correlation [95% CI]", pos=2, cex=.7)


# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
res <- rma.mv(yi, vi, data=dat_aesthetics, 
              random = ~ 1 | study_id,
              mods = ~vi,
              method = "REML") 
res
fsn(yi,vi, data=dat_aesthetics,type="Rosenthal") #fail-safe n


# Egger's regression test (z = -0.6917, p = 0.4891) was not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 536 which exceed the criterion of 60.


######################################################################
## Thirdly, for FACET of FEELINGS and PREJUDICE
#####################################################################


res <- rma.mv(yi, vi, data=dat_feelings, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

sav<-confint(res)  
sav
W <- diag(1/dat_feelings$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * res$sigma2 / (res$sigma2 + (res$k-res$p)/sum(diag(P))) ### total I^2
100 * sav$random[1,2:3] / (sav$random[1,2:3] + (res$k-res$p)/sum(diag(P)))  ### CI for the total I^2

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 10; method: REML)" 

#This line tells us we've used a random-effects model, with 11 studies (i.e., "k") and that the degree of heterogeneity (sigma^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "sigma^2 (estimated amount of variance): 0.0051 (sqrt = 0.0717)"
# nlvls = 7 (number of unique study)
#I2: 48.90%, Ci: 5.15 - 86.57%
# "Test for Heterogeneity: 
# Q(df = 9) = 31.3189, p-val = 0.0003"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.3143   0.0378   -8.3189   <.0001   -0.3883   -0.2402     *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#-0.304 -0.370 -0.236 -0.441 -0.154

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#sigma^2    0.0051  0.0003  0.0347
#sigma      0.0717  0.0171  0.1862

#These two lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_feelings, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

x<- cooks.distance(res)
plot(x,type="o",pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
hatvalues(res)
rstandard(res)
# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("Christopher et al., (2013)",
            "Christopher et al., (2013)",
            "Ekehammar & Akrami, (2007)",
            "Ekehammar & Akrami, (2007)",
            "Miller, (2019)",
            "Onraet et al., (2011)",
            "Onraet et al., (2011)",
            "Huxley et al., (2015)",         
            "Proctor & McCord, (2009)",
            "Szeto et al., (2015)") 


forest(res, xlim=c(-1.9,0.55), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_feelings$ni,dat_feelings$o_measure,dat_feelings$outcome),
       ilab.xpos=c(-1.51, -1.42, -1.25),
       ilab.pos = 4,
       digits=c(2,1), cex=.6,efac = 4,addpred = TRUE,showweights = TRUE)
text(-1.9, 12.2, "Facet of Feelings (NEO-PI-R/3)/ Emotionality (IPIP-based Measures)", pos=4, cex=.9)
text(-1.9, 11.5, "Author(s), Year                             N          Measure        Outcome (Prejudice)", pos=4, cex=.7)
text( 0.55, 11.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)





# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
res <- rma.mv(yi, vi, data=dat_feelings, 
              random = ~ 1 | study_id,
              mods = ~ vi,
              method = "REML") 
res
fsn(yi,vi, data=dat_feelings,type="Rosenthal") #fail-safe n 


# Egger's regression test (z = 1.1803, p = 0.2379) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 645 which exceed 60.

#####################################################################################
############# Now FACET of FANTASY and PREJUDICE
####################################################################################
 
res <- rma.mv(yi, vi, data=dat_fantasy, 
              random = ~ 1 | study_id,
              method = "REML") 
res
sav<-confint(res)  
sav
W <- diag(1/dat_fantasy$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * res$sigma2 / (res$sigma2 + (res$k-res$p)/sum(diag(P))) ### total I^2
100 * sav$random[1,2:3] / (sav$random[1,2:3] + (res$k-res$p)/sum(diag(P)))  ### CI for the total I^2

predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 9; method: REML)" 

#This line tells us we've used a random-effects model, with 9 studies (i.e., "k") and that the degree of heterogeneity (sigma^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "sigma^2 (estimated amount of total heterogeneity): 0.0141 (sqrt = 0.1188)"
#nlvls = 7 (no of unique studies)
#I2: 72.74%, CI: 35.76 - 93.09
# "Test for Heterogeneity: 
# Q(df = 8) = 29.5663, p-val = 0.0003"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.3535   0.0536   -6.5900   <.0001   -0.4586   -0.2483      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#-0.339 -0.429 -0.243 -0.543 -0.098

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#sigma^2    0.0141  0.0029  0.0712
#sigma      0.1188  0.0543  0.2669

#These two lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_fantasy, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

x<- cooks.distance(res)
plot(x,type="o",pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
hatvalues(res)
rstandard(res)
# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("Christopher et al., (2013)",
            "Christopher et al., (2013)",
            "Ekehammar & Akrami, (2007)",
            "Miller, (2019)",
            "Onraet et al., (2011)",
            "Onraet et al., (2011)",
            "Huxley et al., (2015)",
            "Proctor & McCord, (2009)",  
            "Szeto et al., (2015)")


forest(res, xlim=c(-2,0.58), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_fantasy$ni,dat_fantasy$o_measure,dat_fantasy$outcome),
       ilab.xpos=c(-1.6,-1.51, -1.35),
       ilab.pos = 4,
       digits=c(2,1), cex=.6,efac = 4,addpred = TRUE,showweights = TRUE)
text(-2, 11.2, "Facet of Fantasy (NEO-PI-R/3)/ Imagination (IPIP-based Measures)", pos=4, cex=.9)
text(-2, 10.5, "Author(s), Year                             N       Measure      Outcome (Prejudice)", pos=4, cex=.7)
text( 0.58, 10.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 9 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
res <- rma.mv(yi, vi, data=dat_fantasy, 
              random = ~ 1 | study_id,
              mods = ~vi,
              method = "REML") 
res
fsn(yi,vi, data=dat_fantasy,type="Rosenthal") #fail-safe n 

# Egger's regression test (z = 0.4941,p = 0.6213) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N is 729 which exceed 60.

##########################################################################
##For FACET of IDEAS and PREJUDICE
####################################################################

res <- rma.mv(yi, vi, data=dat_ideas, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

sav<-confint(res)  
sav
W <- diag(1/dat_ideas$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * res$sigma2 / (res$sigma2 + (res$k-res$p)/sum(diag(P))) ### total I^2
100 * sav$random[1,2:3] / (sav$random[1,2:3] + (res$k-res$p)/sum(diag(P)))  ### CI for the total I^2

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 10; method: REML)" 

#This line tells us we've used a random-effects model, with 10 studies (i.e., "k") and that the degree of heterogeneity (sigma^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "sigma^2 (estimated amount of total heterogeneity): 0.0123 (sqrt = 0.1107)"
#nlvls=8 (no of unique studies)
#I2: 68.99%, CI: 34.16 - 91.01
# "Test for Heterogeneity: 
# Q(df = 9) = 33.39, p-val = 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.2831   0.0481   -5.8872   <.0001   -0.3774   -0.1889      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  pi.lb  pi.ub
#-0.276 -0.360 -0.187 -0.477 -0.046

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#sigma^2    0.0123  0.0029  0.0558
#sigma    0.1107  0.0535  0.2362


#These two lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_ideas, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.
x<- cooks.distance(res)
plot(x,type="o",pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
hatvalues(res)
rstandard(res)
# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("Christopher et al., (2013)",
            "Christopher et al., (2013)",
            "Ekehammar & Akrami, (2007)",
            "Miller, (2019)",
            "Miller et al., (2012)",
            "Onraet et al., (2011)",
            "Onraet et al., (2011)",
            "Huxley et al., (2015)",
            "Proctor & McCord, (2009)",
            "Szeto et al., (2015)")  

forest(res, xlim=c(-2,0.58), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_ideas$ni,dat_ideas$o_measure,dat_ideas$outcome),
       ilab.xpos=c(-1.58,-1.49, -1.32),
       ilab.pos = 4,
       digits=c(2,1), cex=.6, efac=4,addpred = TRUE,showweights = TRUE)
text(-2, 12.2, "Facet of Ideas (NEO-PI-R/3)/ Intellect (IPIP-based Measures)", pos=4, cex=.9)
text(-2, 11.5, "Author(s), Year                         N        Measure      Outcome (Prejudice)", pos=4, cex=.7)
text( 0.58, 11.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 8 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
res <- rma.mv(yi, vi, data=dat_ideas, 
              random = ~ 1 | study_id,
              mods = ~vi,
              method = "REML") 
res
fsn(yi,vi, data=dat_ideas,type="Rosenthal") #fail-safe n 


# Egger's regression test (z=-0.1317,p = 0.8953) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 495 which exceed 60.

##################################################################################
####For the FACET of ACTIONS and PREJUDICE
#################################################################################

res <- rma.mv(yi, vi, data=dat_actions, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

sav<-confint(res)  
sav
W <- diag(1/dat_actions$vi)
X <- model.matrix(res)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * res$sigma2 / (res$sigma2 + (res$k-res$p)/sum(diag(P))) ### total I^2
100 * sav$random[1,2:3] / (sav$random[1,2:3] + (res$k-res$p)/sum(diag(P)))  ### CI for the total I^2

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 10; method: REML)" 

#This line tells us we've used a random-effects model, with 10 studies (i.e., "k") and that the degree of heterogeneity (sigma^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "sigma^2 (estimated amount of variance): 0.0155 (sqrt = 0.1246)"
#nlvls = 7 (no of unique studies)
#I2: 74.25%, CI: 40.87 - 93.38
# "Test for Heterogeneity: 
# Q(df = 9) = 36.30, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.2722   0.0549  -4.9543   <.0001   -0.3799   -0.1645      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  pi.lb  pi.ub
#-0.266 -0.363 -0.163 -0.492 -0.005

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#sigma^2    0.0155  0.0037  0.0758
#sigma      0.1246  0.0610  0.2754

#These two lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_actions, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

x<- cooks.distance(res)
plot(x,type="o",pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
hatvalues(res)
rstandard(res)
# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c(" Christopher et al., (2013)",
            " Christopher et al., (2013)",
            " Ekehammar & Akrami, (2007)",
            " Ekehammar & Akrami, (2007)",
             " Miller, (2019)",
            " Onraet et al., (2011)",
            "*Onraet et al., (2011)",
            " Huxley et al., (2015)",         
             " Proctor & McCord, (2009)",
            " Szeto et al., (2015)")  

forest(res, xlim=c(-2,0.58), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_actions$ni,dat_actions$o_measure,dat_actions$outcome),
       ilab.xpos=c(-1.58,-1.49, -1.32),
       ilab.pos = 4,
       digits=c(2,1), cex=.6,efac=4,addpred = TRUE,showweights = TRUE)
text(-2, 12.2, "Facet of Actions (NEO-PI-R/3)/ Adventurousness (IPIP-based Measures)", pos=4, cex=.9)
text(-2, 11.5, "Author(s), Year                               N        Measure       Outcome (Prejudice)", pos=4, cex=.7)
text( 0.58, 11.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
res <- rma.mv(yi, vi, data=dat_actions, 
              random = ~ 1 | study_id,
              mods = ~vi,
              method = "REML") 
res
fsn(yi,vi, data=dat_actions,type="Rosenthal") #fail-safe n 

# Egger's regression test (z=1.1245,p = 0.2608) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 578 which exceed 60.

################################################################################################################
##############################################################################################################
#####Now for the associations between FACETS of openness and tolerance
#############################################################################################################
##############################################################################################################


# The first step is to transform r to Z and calculate the corresponding sample variances.
yourdata_1t <- read.csv("Meta-synthesis_tolerance_values.csv", header=TRUE)
yourdata_2t <- read.csv("Meta-synthesis_tolerance_aesthetics.csv", header=TRUE)
yourdata_3t <- read.csv("Meta-synthesis_tolerance_feelings.csv", header=TRUE) 
yourdata_4t <- read.csv("Meta-synthesis_tolerance_fantasy.csv", header=TRUE) 
yourdata_5t <- read.csv("Meta-synthesis_tolerance_ideas.csv", header=TRUE) 
yourdata_6t <- read.csv("Meta-synthesis_tolerance_actions.csv", header=TRUE) 



dat_values_t <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_1t, slab=paste(authors, year, sep=", "))
dat_aesthetics_t <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_2t, slab=paste(authors, year, sep=", ")) 
dat_feelings_t <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_3t, slab=paste(authors, year, sep=", ")) 
dat_fantasy_t <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_4t, slab=paste(authors, year, sep=", ")) 
dat_ideas_t <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_5t, slab=paste(authors, year, sep=", ")) 
dat_actions_t <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_6t, slab=paste(authors, year, sep=", ")) 

# This performs the r to z transform from the "yourdata" datasets and calculates the corresponding sample variances. "ri" represents the correlation coefficients  
# and "ni" represent the sample sizes.

# Lets have a look at the file again, notice the two new variables at the end. The "yi" variable is the z score transformation and the "vi" variable is the 
# corresponding estimated sampling variance.

View(dat_values_t)
View(dat_aesthetics_t)
View(dat_feelings_t)
View(dat_fantasy_t)
View(dat_ideas_t)
View(dat_actions_t)

# Now you're ready to perform the meta-analysis using a random-effects model. The following commands will print out the data and also calculates and 
#################################################################################
####     first, for the FACET of VALUES and tolerance. print the confidence interval for the amount of heterogeneity (I^2).
#################################################################################

res <- rma(yi, vi, data=dat_values_t) 
res
res <- rma.mv(yi, vi, data=dat_values_t, 
              random = ~ 1 | study_id,
              method = "REML") 

predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 5; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 5 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0699 (SE = 0.0547)"

# "I^2 (total heterogeneity / total variability):   93.63%"

# This line indicates that I^2 was 93.63%. In other words 93.63% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 81.82, 99.18.

# "Test for Heterogeneity: 
# Q(df = 4) = 86.78, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   0.4304   0.1245   3.4574   0.0005   0.1864   0.6744      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#0.406 0.184 0.588 -0.141 0.763

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0699  0.0214  0.5734
#tau      0.2643  0.1462  0.7572
#I^2(%)  93.6310 81.8234 99.1781
#H^2      15.7011  5.5016  121.6730

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_values_t, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

inf <- influence(res)
print(inf)
plot(inf)

# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("*Han and Pistole, (2017)",
            " Thompson et al., (2002)",
            "*Kandler et al., (2012)",
            " Roccas et al., (2002)",
            " Unruh and McCord, (2010)")


forest(res, xlim=c(-1.49,1.6), 
              atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6,.8)),
       rows=c(5:4,1:3),
       ilab=cbind(dat_values_t$ni,dat_values_t$o_measure,dat_values_t$outcome),
       ilab.xpos=c(-.98,-.87, -.67),
       ilab.pos = 4,
       digits=c(2,1), cex=.6, efac=9,addpred = TRUE,showweights = TRUE)
text(-1.45, 6.9, "Facet of Values (NEO-PI-R/3)/ Liberalism (IPIP-based Measures)", pos=4, cex=.8)
text(-1.45, 6.35, "Author(s), Year                     N       Measure    Outcome (Tolerance)", pos=4, cex=.7)
text( 1.6, 6.35, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_values_t,type="Rosenthal") #fail-safe n 

# Egger's regression test (p = 0.2477) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 251 which exceed 30.

###################################################
### second, for facet of AESTHETICS and tolerance
##############################################################################
res <- rma(yi, vi, data=dat_aesthetics_t) 
res
res <- rma.mv(yi, vi, data=dat_aesthetics_t, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 4; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 4 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0868 (SE = 0.0787)"

# "I^2 (total heterogeneity / total variability):   91.85%"

# This line indicates that I^2 was 91.85%. In other words 91.85% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 73.93, 99.42.

# "Test for Heterogeneity: 
# Q(df = 3) = 39.3147, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   0.5953   0.1552   3.8353   0.0001   0.2911   0.8995      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#0.534 0.283 0.716 -0.057 0.848

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0868  0.0218  1.3232
#tau      0.2947  0.1478  1.1503
#I^2(%)  91.8548 73.9321 99.4214
#H^2      12.2771  3.8361  172.8310

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_aesthetics_t, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

inf <- influence(res)
print(inf)
plot(inf)

# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("*Han and Pistole, (2017)",
            " Thompson et al., (2002)",
            " Roccas et al., (2002)",
            " Unruh and McCord, (2010)")

forest(res, xlim=c(-1.5,1.64), atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6,.8)),
       ilab=cbind(dat_aesthetics_t$ni,dat_aesthetics_t$o_measure,dat_aesthetics_t$outcome),
       ilab.xpos=c(-1.,-.89, -.68),
       ilab.pos = 4,
       digits=c(2,1), cex=.6,efac=8,addpred = TRUE,showweights = TRUE)
text(-1.5, 5.95, "Facet of Aesthetics (NEO-PI-R/3)/ Artistic Interests (IPIP-based Measures)", pos=4, cex=.8)
text(-1.5, 5.38, "Author(s), Year                      N      Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.64, 5.38, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_aesthetics_t,type="Rosenthal") #fail-safe n 

# Egger's regression test (p = 0.3755) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 295 which exceed 30.

######################################################################
## Thirdly, for FACET of FEELINGS and TOLERANCE
#####################################################################
res <- rma(yi, vi, data=dat_feelings_t) 
res <- rma.mv(yi, vi, data=dat_feelings_t, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 4; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 4 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0296 (SE = 0.0316)"

# "I^2 (total heterogeneity / total variability):   79.38%"

# This line indicates that I^2 was 79.38%. In other words 79.38% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 39.08, 98.20.

# "Test for Heterogeneity: 
# Q(df = 3) = 18.8421, p-val = 0.0003"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   0.3356   0.0983   3.4132   .0006   0.1429   0.5284      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#0.324 0.142 0.484 -0.053 0.620

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0296  0.0049  0.4210
#tau      0.1722  0.0703  0.6489
#I^2(%)  79.3770 39.0782 98.2039
#H^2      4.8489  1.6414  55.6748

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_feelings_t, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

inf <- influence(res)
print(inf)
plot(inf)

# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("*Han and Pistole, (2017)",
            " Thompson et al., (2002)",
            "*Roccas et al., (2002)",
            " Unruh and McCord, (2010)")


forest(res, xlim=c(-1.5,1.6), atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6)),
       ilab=cbind(dat_feelings_t$ni,dat_feelings_t$o_measure,dat_feelings_t$outcome),
       ilab.xpos=c(-1.,-.9, -.7),
       ilab.pos = 4,
       digits=c(2,1), cex=.6, efac = 8,addpred = TRUE,showweights = TRUE)
text(-1.5, 5.98, "Facet of Feelings (NEO-PI-R/3)/ Emotionality (IPIP-based Measures)", pos=4, cex=.8)
text(-1.5, 5.42, "Author(s), Year                        N       Measure    Outcome (Tolerance)", pos=4, cex=.7)
text( 1.6, 5.42, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)





# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_feelings_t,type="Rosenthal") #fail-safe n 

# Egger's regression test (p = 0.6968) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 79 which exceed 30.

#####################################################################################
############# Now FACET of FANTASY and TOLERANCE
####################################################################################

res <- rma(yi, vi, data=dat_fantasy_t) 
res
res <- rma.mv(yi, vi, data=dat_fantasy_t, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 4; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 4 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0213 (SE = 0.0246)"

# "I^2 (total heterogeneity / total variability):   73.47%"

# This line indicates that I^2 was 73.47%. In other words 73.47% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 16.61, 97.93.

# "Test for Heterogeneity: 
# Q(df = 3) = 11.52, p-val = 0.0092"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   0.2889   0.0868   3.3273   .0009   0.1187   0.4591      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#0.281 0.118 0.429 -0.044 0.552

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0213  0.0015  0.3657
#tau      0.1460  0.0392  0.6047
#I^2(%)  73.4714 16.6117 97.9379
#H^2      3.7695  1.1992  48.4933

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_fantasy_t, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

inf <- influence(res)
print(inf)
plot(inf)

# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("*Han and Pistole, (2017)",
            " Thompson et al., (2002)", 
            " Roccas et al., (2002)",
            " Unruh and McCord, (2010)")


forest(res, xlim=c(-1.5,1.60), atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6)),
       ilab=cbind(dat_fantasy_t$ni,dat_fantasy_t$o_measure,dat_fantasy_t$outcome),
       ilab.xpos=c(-1.0,-.90, -.71),
       ilab.pos = 4,
       digits=c(2,1), cex=.6,efac=6,addpred = TRUE,showweights = TRUE)
text(-1.5, 6.05, "Facet of Fantasy (NEO-PI-R/3)/ Imagination (IPIP-based Measures)", pos=4, cex=.8)
text(-1.5, 5.35, "Author(s), Year                        N      Measure      Outcome (Tolerance)", pos=4, cex=.7)
text( 1.6, 5.35, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)



# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_fantasy_t,type="Rosenthal") #fail-safe n 


# Egger's regression test (p = 0.5511) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 64 which exceed 30.

##########################################################################
##For FACET of IDEAS and TOLERANCE
####################################################################

res <- rma(yi, vi, data=dat_ideas_t) 
res
res <- rma.mv(yi, vi, data=dat_ideas_t, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 4; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 4 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0980 (SE = 0.0878)"

# "I^2 (total heterogeneity / total variability):   92.71%"

# This line indicates that I^2 was 92.71%. In other words 92.71% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 76.75, 99.48.

# "Test for Heterogeneity: 
# Q(df = 3) = 44.46, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   0.4537   0.1640   2.7665   0.0057   0.1323   0.7750      ** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#0.425 0.131 0.650 -0.235 0.817

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0980  0.0254  1.4730
#tau      0.3130  0.1594  1.2137
#I^2(%)  92.7143 76.7485 99.4800
#H^2      13.7256  4.3008  192.2910

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_ideas_t, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

inf <- influence(res)
print(inf)
plot(inf)

# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("*Han and Pistole, (2017)",
            " Thompson et al., (2002)", 
            " Roccas et al., (2002)",
            " Unruh and McCord, (2010)")


forest(res, xlim=c(-1.5,1.6), atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6,.8)),
       ilab=cbind(dat_ideas_t$ni,dat_ideas_t$o_measure,dat_ideas_t$outcome),
       ilab.xpos=c(-1.02,-.91, -.7),
       ilab.pos = 4,
       digits=c(2,1), cex=.6,efac=8,addpred = TRUE,showweights = TRUE)
text(-1.5, 5.95, "Facet of Ideas (NEO-PI-R/3)/ Intellect (IPIP-based Measures)", pos=4, cex=.8)
text(-1.5, 5.38, "Author(s), Year                      N       Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.6, 5.38, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)


# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_ideas_t,type="Rosenthal") #fail-safe n 


# Egger's regression test (p = 0.3850) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 175 which exceed 30.

##################################################################################
####For the FACET of ACTIONS and TOLERANCE
#################################################################################

res <- rma(yi, vi, data=dat_actions_t) 
res
res <- rma.mv(yi, vi, data=dat_actions_t, 
              random = ~ 1 | study_id,
              method = "REML") 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 4; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 4 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.1310 (SE = 0.1148)"

# "I^2 (total heterogeneity / total variability):   94.45%"

# This line indicates that I^2 was 94.45%. In other words 94.45% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 82.36, 99.60.

# "Test for Heterogeneity: 
# Q(df = 3) = 58.4029, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   0.4902   0.1875   2.6141   0.0089   0.1227   0.8577      ** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#0.454 0.122 0.695 -0.299 0.859

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.1310  0.0360  1.9275
#tau      0.3620  0.1896  1.3883
#I^2(%)   94.45    82.3606 99.6021
#H^2      18.0148  5.6691  251.3023

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_actions_t, slab=study_id)  # New meta-analysis with study ID identifier  

# The next command will plot a Baujat plot.

baujat(b_res)

# Studies that fall to the top right quadrant of the Baujat plot contribute most to both these factors. Looking at the Molloy et al., 2014 data set reveals that 3 studies 
# that contribute to both of these factors. A closer look the characteristics of these studies may reveal moderating variables that may contribute to heterogeneity

# A set of diagnostics are also available to identify potential outliers and influential cases.

inf <- influence(res)
print(inf)
plot(inf)

# The plot visualizes the printed dataset. As there are no studies are marked with an asterisk in the printed dataset, none of the studies fulfilled the criteria as 
# an influential study. 

# Now we visualize the meta-analysis with a forest plot. 
res$slab
res$slab<-c("*Han and Pistole, (2017)",
            " Thompson et al., (2002)", 
            " Roccas et al., (2002)",
            " Unruh and McCord, (2010)")

forest(res, xlim=c(-1.5,1.6), ylim=c(-1.5,7),
              atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6,.8)),
       ilab=cbind(dat_actions_t$ni,dat_actions_t$o_measure,dat_actions_t$outcome),
       ilab.xpos=c(-1.01, -.91, -.72),
       ilab.pos = 4,
       digits=c(2,1), cex=.6,efac = 8,addpred = TRUE,showweights = TRUE)
text(-1.5, 5.95, "Facet of Actions (NEO-PI-R/3)/ Adventurousness (IPIP-based Measures)", pos=4, cex=.8)
text(-1.5, 5.38, "Author(s), Year                      N       Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.6, 5.38, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)


# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_actions_t,type="Rosenthal") #fail-safe n 

# Egger's regression test (p = 0.3729) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 210 which exceed 30.

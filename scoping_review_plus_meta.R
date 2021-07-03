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

res <- rma(yi, vi, data=dat_values) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 12; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 12 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0398 (SE = 0.0196)"

# "I^2 (total heterogeneity / total variability):   89.13%"

# This line indicates that I^2 was 89.13%. In other words 89.13% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 77.78, 96.14.

# "Test for Heterogeneity: 
# Q(df = 11) = 141.5067, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.4805   0.0619   -7.7589   <.0001   -0.6019   -0.3591      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#-0.447 -0.538 -0.344 -0.711 -0.071

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0398  0.0170  0.1209
#tau      0.1994  0.1303  0.3477
#I^2(%)  89.1307 77.7761 96.1439
#H^2      9.2002  4.4997  25.9330

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_values, slab=study_id)  # New meta-analysis with study ID identifier  

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
res$slab<-c("Christopher et al., 2013",  
                              "Christopher et al., 2013", 
                              "Ekehammar & Akrami, 2007",
                              "Ekehammar & Akrami, 2007",
                              "Miller, 2019",
                              "Miller et al. , 2012",
                              "Onraet et al., 2011", 
                              "Onraet et al., 2011",
                              "Averhart, 2012",
                              "Huxley et al., 2015",
                              "Proctor & McCord, 2009",
                              "Szeto et al., 2015")


forest(res, xlim=c(-2.4,0.7), atransf=transf.ztor,
       at=transf.rtoz(c(-.8,-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_values$o_measure,dat_values$outcome),
       ilab.xpos=c(-1.85, -1.6),
       ilab.pos=4, 
       digits=c(2,1), cex=.6,efac = 3,addpred = TRUE,showweights = TRUE)
text(-2.4, 14.4, "Facet of Values/Liberalism", pos=4, cex=.9)
text(-2.4, 13.5, "Author(s), Year                     Measure     Outcome (Prejudice)", pos=4, cex=.7)
text( 0.7, 13.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_values,type="Rosenthal") #fail-safe n 

# Egger's regression test (p = 0.1753) was not statistically significant so there's no evidence of publication bias 
# according to these tests. And fail safe N test is 2131 which exceed the criterion of 60.

###################################################
### second, for facet of AESTHETICS and PREJUDICE
##############################################################################


res <- rma(yi, vi, data=dat_aesthetics) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 11; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 11 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0141 (SE = 0.0091)"

# "I^2 (total heterogeneity / total variability):   71.70%"

# This line indicates that I^2 was 71.70%. In other words 71.70% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 40.5872, 89.4320.

# "Test for Heterogeneity: 
# Q(df = 10) = 39.7411, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.2699   0.0432   -6.2506   <.0001   -0.3546   -0.1853      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#-0.264 -0.340 -0.183 -0.476 -0.022

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0141  0.0038  0.0472
#tau      0.1189  0.0617  0.2173
#I^2(%)  71.7033 40.5872 89.4320
#H^2      3.5340  1.6831  9.4625

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_aesthetics, slab=study_id)  # New meta-analysis with study ID identifier  

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
res$slab<-c("Christopher et al., 2013","Christopher et al., 2013","Ekehammar & Akrami, 2007","Ekehammar & Akrami, 2007",
            "Miller, 2019","Miller et al., 2012","Onraet et al., 2011","Onraet et al., 2011","Huxley et al., 2015",
            "Proctor and McCord, 2009","Szeto et al., 2015") 


forest(res, xlim=c(-1.9,0.7), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_aesthetics$o_measure,dat_aesthetics$outcome),
       ilab.xpos=c(-1.33, -1.03),
       digits=c(2,1), cex=.6,efac=5,addpred = TRUE,showweights = TRUE)
text(-1.9, 13.2, "Facet of Aesthetics/Artistic Interests", pos=4, cex=.9)
text(-1.9, 12.5, "Author(s), Year                      Measure         Outcome (Prejudice)", pos=4, cex=.7)
text( 0.7, 12.5, "Weight[%]   Correlation [95% CI]", pos=2, cex=.7)


# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_aesthetics,type="Rosenthal") #fail-safe n


# Egger's regression test (p = 0.3479) was not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 536 which exceed the criterion of 60.


######################################################################
## Thirdly, for FACET of FEELINGS and PREJUDICE
#####################################################################


res <- rma(yi, vi, data=dat_feelings) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 10; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 11 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0131 (SE = 0.0090)"

# "I^2 (total heterogeneity / total variability):   70.94%"

# This line indicates that I^2 was 70.94%. In other words 70.94% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 37.55, 91.71.

# "Test for Heterogeneity: 
# Q(df = 9) = 31.3189, p-val = 0.0003"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.3142   0.0439   -7.1567   <.0001   -0.4002   -0.2281      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#-0.304 -0.380 -0.224 -0.504 -0.074

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0131  0.0032  0.0595
#tau      0.1146  0.0569  0.2439
#I^2(%)  70.9356 37.5488 91.7080
#H^2      3.4406  1.6012  12.0598

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_feelings, slab=study_id)  # New meta-analysis with study ID identifier  

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
res$slab<-c("Christopher et al., 2013",
            "Christopher et al., 2013",
            "Ekehammar & Akrami, 2007",
            "Ekehammar & Akrami, 2007",
            "Miller, 2019",
            "Onraet et al., 2011",
            "Onraet et al., 2011",
            "Huxley et al., 2015",         
            "Proctor & McCord, 2009",
            "Szeto et al., 2015") 


forest(res, xlim=c(-1.9,0.7), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_feelings$o_measure,dat_feelings$outcome),
       ilab.xpos=c(-1.33, -1.05),
       digits=c(2,1), cex=.6,efac = 3,addpred = TRUE,showweights = TRUE)
text(-1.9, 12.2, "Facet of Feelings/Emotionality", pos=4, cex=.9)
text(-1.9, 11.5, "Author(s), Year                    Measure       Outcome (Prejudice)", pos=4, cex=.7)
text( 0.7, 11.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)





# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_feelings,type="Rosenthal") #fail-safe n 


# Egger's regression test (p = 0.4462) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 645 which exceed 60.

#####################################################################################
############# Now FACET of FANTASY and PREJUDICE
####################################################################################

res <- rma(yi, vi, data=dat_fantasy) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 9; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 9 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0141 (SE = 0.0100)"

# "I^2 (total heterogeneity / total variability):   72.77%"

# This line indicates that I^2 was 72.77%. In other words 72.77% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 38.55, 92.24.

# "Test for Heterogeneity: 
# Q(df = 8) = 29.57, p-val = 0.0003"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.3663   0.0475   -7.7164   <.0001   -0.4593   -0.2732      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#-0.351 -0.430 -0.267 -0.549 -0.115

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0141  0.0032  0.0595
#tau      0.1189  0.0569  0.2439
#I^2(%)  72.7748 38.5495 92.2407
#H^2      3.6731  1.6273  12.8877

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_fantasy, slab=study_id)  # New meta-analysis with study ID identifier  

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
res$slab<-c("Christopher et al., 2013",
            "Christopher et al., 2013",
            "Ekehammar & Akrami, 2007",
            "Miller, 2019",
            "Onraet et al., 2011",
            "Onraet et al., 2011",
            "Huxley et al., 2015",
            "Proctor & McCord, 2009",  
            "Szeto et al., 2015")


forest(res, xlim=c(-2,0.7), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_fantasy$o_measure,dat_fantasy$outcome),
       ilab.xpos=c(-1.4, -1.1),
       digits=c(2,1), cex=.6,efac = 3,addpred = TRUE,showweights = TRUE)
text(-2, 11.2, "Facet of Fantasy/Imagination", pos=4, cex=.9)
text(-2, 10.5, "Author(s), Year                    Measure         Outcome (Prejudice)", pos=4, cex=.7)
text( 0.7, 10.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_fantasy,type="Rosenthal") #fail-safe n 

# Egger's regression test (p = 0.6145) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N is 729 which exceed 60.

##########################################################################
##For FACET of IDEAS and PREJUDICE
####################################################################

res <- rma(yi, vi, data=dat_ideas) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 10; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 10 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0138 (SE = 0.0094)"

# "I^2 (total heterogeneity / total variability):   71.45%"

# This line indicates that I^2 was 71.45%. In other words 71.45% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 38.15, 90.85.

# "Test for Heterogeneity: 
# Q(df = 9) = 33.39, p-val = 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.2817   0.0449   -6.2740   <.0001   -0.3697   -0.1937      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#-0.274 -0.354 -0.191 -0.484 -0.035

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0138  0.0034  0.0547
#tau      0.1175  0.0583  0.2340
#I^2(%)  71.4546 38.1515 90.8546
#H^2      3.5032  1.6169  10.9345

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_ideas, slab=study_id)  # New meta-analysis with study ID identifier  

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
res$slab<-c("Christopher et al., 2013",
            "Christopher et al., 2013",
            "Ekehammar & Akrami, 2007",
            "Miller, 2019",
            "Miller et al., 2012",
            "Onraet et al., 2011",
            "Onraet et al., 2011",
            "Huxley et al., 2015",
            "Proctor & McCord, 2009",
            "Szeto et al., 2015")  

forest(res, xlim=c(-2,0.7), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_ideas$o_measure,dat_ideas$outcome),
       ilab.xpos=c(-1.4, -1.07),
       digits=c(2,1), cex=.6, efac=3,addpred = TRUE,showweights = TRUE)
text(-2, 12.2, "Facet of Ideas/Intellect", pos=4, cex=.9)
text(-2, 11.5, "Author(s), Year                     Measure          Outcome (Prejudice)", pos=4, cex=.7)
text( 0.7, 11.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_ideas,type="Rosenthal") #fail-safe n 


# Egger's regression test (p = 0.7464) not statistically significant so there's no evidence of publication bias 
# according to these tests. Fail safe N test is 495 which exceed 60.

##################################################################################
####For the FACET of ACTIONS and PREJUDICE
#################################################################################


res <- rma(yi, vi, data=dat_actions) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

# The output provides important information to report the meta-analysis, let's look section-by-section at the relevant data.

# "Random-Effects Model (k = 10; tau^2 estimator: REML)" 

#This line tells us we've used a random-effects model, with 10 studies (i.e., "k") and that the degree of heterogeneity (tau^2) was calculated using a 
# restricted maximum-likelihood estimator.

# "tau^2 (estimated amount of total heterogeneity): 0.0165 (SE = 0.0106)"

# "I^2 (total heterogeneity / total variability):   75.37%"

# This line indicates that I^2 was 75.37%. In other words 75.37% of variation reflected actual differences in the population mean. 
# The confidence interval test revealed the 95% confidence interval for this value is 46.44, 92.62.

# "Test for Heterogeneity: 
# Q(df = 9) = 36.30, p-val < 0.0001"

#These next two lines show the Q-statistic with degrees of freedom as well as the p-value of the test. In this analysis, the p-value < 0.0001, 
# suggesting that the included studies do not share a common effect size.

# Model Results:
#
# estimate       se     zval     pval    ci.lb    ci.ub          
#   -0.2933   0.0476   -6.1588   <.0001   -0.3867   -0.2000      *** 

# Finally, we have the model results. The "estimate" provides the estimated model coefficient (i.e., the summary effect size) with standard error("se"). 
# The z-value is the corresponding test statistic, "pval" is the corresponding p-value, "ci.lb" the the lower bound of the confidence interval and 
# "ci.ub" the upper bound of the confidence interval.

#pred    ci.lb  ci.ub  cr.lb  cr.ub
#-0.285 -0.369 -0.197 -0.509 -0.025

# These two lines display the transformation of Fisher's z back to Pearson's r ("pred"), and the 95% confidence interval for r ("ci.lb" and "ci.ub") for 
# reporting the meta-analysis.

#        estimate   ci.lb   ci.ub
#tau^2    0.0165  0.0047  0.0676
#tau      0.1283  0.0683  0.2599
#I^2(%)   75.37    46.4407 92.6243
#H^2      4.0595  1.8671  13.5580

#These four lines display estimates and 95% confience intevals for heterogeneity measures as descibed above.


# While the Q-statistic and I^2 can provide evidence for heterogeneity, they do not provide information on which studies may be influencing to overall heterogeneity. 
# If there is evidence of overall heterogeneity, construction of a Bajaut plot can illustrate studies that are contribute to overall heterogeneity and the overall result. 
# Study IDs are used to identify studies

b_res <- rma(yi, vi, data=dat_actions, slab=study_id)  # New meta-analysis with study ID identifier  

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
res$slab<-c("Christopher et al., 2013",
            "Christopher et al., 2013",
            "Ekehammar & Akrami, 2007",
            "Ekehammar & Akrami, 2007",
             "Miller, 2019",
            "Onraet et al., 2011",
            "Onraet et al., 2011",
            "Huxley et al., 2015",         
             "Proctor & McCord, 2009",
            "Szeto et al., 2015")  

forest(res, xlim=c(-2,0.7), atransf=transf.ztor,
       at=transf.rtoz(c(-.6,-.4,-.2,0,.2)),
       ilab=cbind(dat_actions$o_measure,dat_actions$outcome),
       ilab.xpos=c(-1.4, -1.07),
       digits=c(2,1), cex=.6,efac=3,addpred = TRUE,showweights = TRUE)
text(-2, 12.2, "Facet of Actions/Adventurousness", pos=4, cex=.9)
text(-2, 11.5, "Author(s), Year                    Measure         Outcome (Prejudice)", pos=4, cex=.7)
text( 0.7, 11.5, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

# We've created a plot with all 16 studies. Importantly, the correlations and 95% CIs are reported for each study as well as the summary effect size 
# (the polygon at the bottom). The edges of the polygon represent the 95% confidence limit. Note the different sizes of each square - the studies with larger squares 
# contributed more to the summary effect size.  

### funnel plot
funnel(res, xlab = "Correlation coefficient")

#Tests for bias
regtest(res)
fsn(yi,vi, data=dat_actions,type="Rosenthal") #fail-safe n 

# Egger's regression test (p = 0.2289) not statistically significant so there's no evidence of publication bias 
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

forest(res, xlim=c(-1.45,1.7), 
              atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6,.8)),
       rows=c(5:4,1:3),
       ilab=cbind(dat_values_t$o_measure,dat_values_t$outcome),
       ilab.xpos=c(-.83, -.47),
       digits=c(2,1), cex=.6, efac=9,addpred = TRUE,showweights = TRUE)
text(-1.45, 6.9, "Facet of Values/Liberalism", pos=4, cex=.8)
text(-1.45, 6.35, "Author(s), Year               Measure          Outcome (Tolerance)", pos=4, cex=.7)
text( 1.7, 6.35, "Weight[%]  Correlation [95% CI]", pos=2, cex=.7)

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

forest(res, xlim=c(-1.5,1.64), atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6,.8)),
       ilab=cbind(dat_aesthetics_t$o_measure,dat_aesthetics_t$outcome),
       ilab.xpos=c(-0.88, -.61),
       digits=c(2,1), cex=.6,efac=8,addpred = TRUE,showweights = TRUE)
text(-1.5, 5.95, "Facet of Aesthetics/Artistic Interests", pos=4, cex=.8)
text(-1.5, 5.38, "Author(s), Year               Measure      Outcome (Tolerance)", pos=4, cex=.7)
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

forest(res, xlim=c(-1.5,1.23), atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6)),
       ilab=cbind(dat_feelings_t$o_measure,dat_feelings_t$outcome),
       ilab.xpos=c(-0.96, -.73),
       digits=c(2,1), cex=.6, efac = 8,addpred = TRUE,showweights = TRUE)
text(-1.5, 5.98, "Facet of Feelings/Emotionality", pos=4, cex=.8)
text(-1.5, 5.42, "Author(s), Year               Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.23, 5.42, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)





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

forest(res, xlim=c(-1.65,1.20), atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6)),
       ilab=cbind(dat_fantasy_t$o_measure,dat_fantasy_t$outcome),
       ilab.xpos=c(-1.1, -.86),
       digits=c(2,1), cex=.6,efac=6,addpred = TRUE,showweights = TRUE)
text(-1.65, 6.05, "Facet of Fantasy/Imagination", pos=4, cex=.8)
text(-1.65, 5.35, "Author(s), Year                 Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.2, 5.35, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)



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

forest(res, xlim=c(-1.58,1.76), atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6,.8)),
       ilab=cbind(dat_ideas_t$o_measure,dat_ideas_t$outcome),
       ilab.xpos=c(-0.94, -.61),
       digits=c(2,1), cex=.6,efac=8,addpred = TRUE,showweights = TRUE)
text(-1.58, 5.95, "Facet of Ideas/Intellect", pos=4, cex=.8)
text(-1.58, 5.38, "Author(s), Year                 Measure      Outcome (Tolerance)", pos=4, cex=.7)
text( 1.76, 5.38, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)


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

forest(res, xlim=c(-1.58,1.76), ylim=c(-1.5,7),
              atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,.2,.4,.6,.8)),
       ilab=cbind(dat_actions_t$o_measure,dat_actions_t$outcome),
       ilab.xpos=c(-0.94, -.61),
       digits=c(2,1), cex=.6,efac = 8,addpred = TRUE,showweights = TRUE)
text(-1.58, 5.95, "Facet of Actions/Adventurousness", pos=4, cex=.8)
text(-1.58, 5.38, "Author(s), Year              Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.76, 5.38, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)


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

#######################################################################################################
########################################################################################################
#####Now for HEXACO facets and prejudice
#############################################################################
#############################################################################

yourdata_hex_1p <- read.csv("Meta-synthesis_prejudice_aesthetic_apprec.csv", header=TRUE)
yourdata_hex_2p <- read.csv("Meta-synthesis_prejudice_inquisitive.csv", header=TRUE)
yourdata_hex_3p <- read.csv("Meta-synthesis_prejudice_creativity.csv", header=TRUE) 
yourdata_hex_4p <- read.csv("Meta-synthesis_prejudice_unconventionality.csv", header=TRUE) 

dat_aes_p_hex <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_hex_1p, slab=paste(authors, year, sep=", "))
dat_inq_p_hex<- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_hex_2p, slab=paste(authors, year, sep=", ")) 
dat_crea_p_hex <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_hex_3p, slab=paste(authors, year, sep=", ")) 
dat_uncon_p_hex <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_hex_4p, slab=paste(authors, year, sep=", ")) 

View(dat_aes_p_hex)
View(dat_inq_p_hex)
View(dat_crea_p_hex)
View(dat_uncon_p_hex)

#####################
#######aesthetics and prejudice
####################

res <- rma(yi, vi, data=dat_aes_p_hex) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

res$slab
res$slab<-c("Anglim et al., 2019.",
            "Anglim et al., 2019", 
            "Anglim et al., 2019",
            "Anglim et al., 2019") 

forest(res, xlim=c(-1,0.5), ylim=c(-1.5,6.5),
       atransf=transf.ztor,
       at=transf.rtoz(c(-.4,-.2,0,0.2)),
       ilab=cbind(dat_aes_p_hex$o_measure,dat_aes_p_hex$outcome),
       ilab.xpos=c(-0.72, -0.56),
       digits=c(2,1), cex=.6,efac = 7,addpred = TRUE,showweights = TRUE)
text(-1, 5.6, "Facet of Aesthetic Appreciation", pos=4, cex=.8)
text(-1, 4.95, "Author(s), Year              Measure     Outcome (Prejudice)", pos=4, cex=.7)
text( 0.5, 4.95, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)


###############################
#############Inquistiveness and prejudice
###############################

res <- rma(yi, vi, data=dat_inq_p_hex) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

res$slab
res$slab<-c("Anglim et al., 2019.",
            "Anglim et al., 2019", 
            "Anglim et al., 2019",
            "Anglim et al., 2019") 

forest(res, xlim=c(-1,0.5), ylim=c(-1.5,7),
       atransf=transf.ztor,
       at=transf.rtoz(c(-.4,-0.2, 0,.2)),
       ilab=cbind(dat_inq_p_hex$o_measure,dat_inq_p_hex$outcome),
       ilab.xpos=c(-0.72, -.56),
       digits=c(2,1), cex=.6,efac = 4,addpred = TRUE,showweights = TRUE)
text(-1, 5.82, "Facet of Inquistiveness", pos=4, cex=.8)
text(-1, 5.3, "Author(s), Year              Measure     Outcome (Prejudice)", pos=4, cex=.7)
text( 0.5, 5.3, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)

################################
#############creativity and prejudice
###############################
res <- rma(yi, vi, data=dat_crea_p_hex) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

res$slab
res$slab<-c("Anglim et al., 2019.",
            "Anglim et al., 2019", 
            "Anglim et al., 2019",
            "Anglim et al., 2019") 

forest(res, xlim=c(-1,0.5), ylim=c(-1.5,7),
       atransf=transf.ztor,
       at=transf.rtoz(c(-.4,-0.2, 0,.2)),
       ilab=cbind(dat_crea_p_hex$o_measure,dat_crea_p_hex$outcome),
       ilab.xpos=c(-0.72, -.56),
       digits=c(2,1), cex=.6,efac = 4,addpred = TRUE,showweights = TRUE)
text(-1, 5.82, "Facet of Creativity", pos=4, cex=.8)
text(-1, 5.3, "Author(s), Year              Measure     Outcome (Prejudice)", pos=4, cex=.7)
text( 0.5, 5.3, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)

###################################
############unconventionality and prejudice
###################################

res <- rma(yi, vi, data=dat_uncon_p_hex) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  
res$slab
res$slab<-c("Anglim et al., 2019.",
            "Anglim et al., 2019", 
            "Anglim et al., 2019",
            "Anglim et al., 2019") 

forest(res, xlim=c(-1,0.5), ylim=c(-1.5,7),
       atransf=transf.ztor,
       at=transf.rtoz(c(-.4,-0.2, 0,.2)),
       ilab=cbind(dat_uncon_p_hex$o_measure,dat_uncon_p_hex$outcome),
       ilab.xpos=c(-0.72, -.56),
       digits=c(2,1), cex=.6,efac = 4,addpred = TRUE,showweights = TRUE)
text(-1, 5.82, "Facet of Unconventionality", pos=4, cex=.8)
text(-1, 5.3, "Author(s), Year              Measure     Outcome (Prejudice)", pos=4, cex=.7)
text( 0.5, 5.3, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)



###################################################################################
############Now, for HEXACO facets and tolerance
###############################################################

yourdata_hex_1t <- read.csv("Meta-synthesis_tolerance_aesthetic_apprec.csv", header=TRUE)
yourdata_hex_2t <- read.csv("Meta-synthesis_tolerance_inquisitveness.csv", header=TRUE)
yourdata_hex_3t <- read.csv("Meta-synthesis_tolerance_creativity.csv", header=TRUE) 
yourdata_hex_4t <- read.csv("Meta-synthesis_tolerance_unconventionality.csv", header=TRUE) 

dat_aes_t_hex <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_hex_1t, slab=paste(authors, year, sep=", "))
dat_inq_t_hex<- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_hex_2t, slab=paste(authors, year, sep=", ")) 
dat_crea_t_hex <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_hex_3t, slab=paste(authors, year, sep=", ")) 
dat_uncon_t_hex <- escalc(measure="ZCOR", ri=ri, ni=ni, data=yourdata_hex_4t, slab=paste(authors, year, sep=", ")) 

View(dat_aes_t_hex)
View(dat_inq_t_hex)
View(dat_crea_t_hex)
View(dat_uncon_t_hex)

#####################
#######aesthetics and tolerance
####################

res <- rma(yi, vi, data=dat_aes_t_hex) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

forest(res, xlim=c(-1.5,1.5), ylim=c(-1.5,5),
       atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,0.2,.4,.6)),
       ilab=cbind(dat_aes_t_hex$o_measure,dat_aes_t_hex$outcome),
       ilab.xpos=c(-0.95, -0.65),
       digits=c(2,1), cex=.6,efac = 7,addpred = TRUE,showweights = TRUE)
text(-1.5, 3.8, "Facet of Aesthetic Appreciation", pos=4, cex=.8)
text(-1.5, 3.35, "Author(s), Year              Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.5, 3.35, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)

###############################
#############Inquistiveness and tolerance
###############################

res <- rma(yi, vi, data=dat_inq_t_hex) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

forest(res, xlim=c(-1.5,1.5), ylim=c(-1.5,5),
       atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,0.2,.4,.6)),
       ilab=cbind(dat_inq_t_hex$o_measure,dat_inq_t_hex$outcome),
       ilab.xpos=c(-0.95, -0.65),
       digits=c(2,1), cex=.6,efac = 7,addpred = TRUE,showweights = TRUE)
text(-1.5, 3.8, "Facet of Inquisitiveness", pos=4, cex=.8)
text(-1.5, 3.35, "Author(s), Year              Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.5, 3.35, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)


################################
#############creativity and tolerance
###############################
res <- rma(yi, vi, data=dat_crea_t_hex) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

forest(res, xlim=c(-1.5,1.5), ylim=c(-1.5,5),
       atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,0.2,.4,.6)),
       ilab=cbind(dat_crea_t_hex$o_measure,dat_crea_t_hex$outcome),
       ilab.xpos=c(-0.95, -0.65),
       digits=c(2,1), cex=.6,efac = 7,addpred = TRUE,showweights = TRUE)
text(-1.5, 3.8, "Facet of Creativity", pos=4, cex=.8)
text(-1.5, 3.35, "Author(s), Year              Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.5, 3.35, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)
###################################
############unconventionality and tolerance
###################################

res <- rma(yi, vi, data=dat_uncon_t_hex) 
res
predict(res, digits=3, transf=transf.ztor)
confint(res)  

forest(res, xlim=c(-1.5,1.5), ylim=c(-1.5,5),
       atransf=transf.ztor,
       at=transf.rtoz(c(-.2,0,0.2,.4,.6)),
       ilab=cbind(dat_uncon_t_hex$o_measure,dat_uncon_t_hex$outcome),
       ilab.xpos=c(-0.95, -0.65),
       digits=c(2,1), cex=.6,efac = 7,addpred = TRUE,showweights = TRUE)
text(-1.5, 3.8, "Facet of Unconventionality", pos=4, cex=.8)
text(-1.5, 3.35, "Author(s), Year              Measure     Outcome (Tolerance)", pos=4, cex=.7)
text( 1.5, 3.35, "Weight[%] Correlation [95% CI]", pos=2, cex=.7)
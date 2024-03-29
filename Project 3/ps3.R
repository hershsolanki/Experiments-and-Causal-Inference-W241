---
title: "Problem Set 3"
author: "Experiments and Causality"
output: 
    github_document: default
    pdf_document: default
knit: (function(inputFile, encoding) {
  rmarkdown::render(
    inputFile, encoding = encoding,
    output_format = c('github_document', 'pdf_document')) 
    })
---

```{r, results='hide'} 
# load packages 
library(data.table)
library(foreign)
```

# 0. Write Functions 
You're going to be doing a few things a *number* of times -- calculating robust standard errors, calculating clustered standard errors, and then calculating the confidence intervals that are built off these standard errors. 

*After* you've worked through a few of these questions, I suspect you will see places to write a function that will do this work for you. Include those functions here, if you write them. 

```{r}
library(sandwich)
library(lmtest)


get_VCOC_HC <- function(lm) {
  vcovHC <- vcovHC(lm)
  return (vcovHC)
}

get_ROBUST_SE <- function(lm) {
  print("Robust SE")
  return(sqrt(diag(vcov
                   HC)))
}

get_CONF <- function(lm) {
  print("CI")
  return(coefci(lm , vcov = get_VCOC_HC(lm)))
}


get_VCOC_CL <- function(lm, data, column, number) {
  vcovCL1 <- vcovCL(lm, cluster = data[column==number , cluster])
  return (vcovCL1)
}

get_CLUSTER_SE <- function(lm, data, column, number) {
  vcovCL1 <- vcovCL(lm, cluster = data[column==number , cluster])
  clusterse <- sqrt(diag(vcovCL1))
  print("Clustered SE")
  return (clusterse)
}

get_CLUSTER_CONF <- function(lm, data, column, number) {
  
  confidenceInterval <- coefci(lm , vcov = vcovCL(lm, cluster = data[column==number , cluster]))
  print("Clustered CI")
  return (confidenceInterval)
}

```

# 1. Replicate Results 
Skim [Broockman and Green's](./readings/brookman_green_ps3.pdf) paper on the effects of Facebook ads and download an anonymized version of the data for Facebook users only.

```{r}
d.1 <- fread("./data/broockman_green_anon_pooled_fb_users_only.csv")
head(d.1)
dim(d.1[d.1$studyno == 1])
``` 

a. Using regression without clustered standard errors (that is, ignoring the clustered assignment), compute a confidence interval for the effect of the ad on candidate name recognition in Study 1 only (the dependent variable is "name_recall"). 
+ **Note**: Ignore the blocking the article mentions throughout this problem.
+ **Note**: You will estimate something different than is reported in the study. 

```{r} 
get_CONF(lm(name_recall ~ treat_ad, data = d.1[d.1$studyno == 1]))
```
**We see a 95% confidence interval for the effect of the ad to be [-0.05121351, 0.03161774]**

b. What are the clusters in Broockman and Green's study? Why might taking clustering into account increase the standard errors?

**Clusters are individuals who have the same age/gender/location. Because there is not much variation within clusters, but varation between clusters, we must adjust for this. Thus, taking clustering into account should increase the standard errors.**

c. Now estimate a regression that estimates the effect of the ad on candidate name recognition in Study 1, but this time take take clustering into account. (hint: The estimation of the *model* does not change, only the estimation of the standard errors.) If you're not familiar with how to calculate these clustered and robust estimates, there is a demo worksheet that is available in our course repository: [`./week_05/cluster_and_robust.Rmd`](http://datahub.berkeley.edu/hub/user-redirect/git-pull?repo=https://github.com/UCB-MIDS/w241&urlpath=rstudio/).

```{r} 
library(lmtest)
library(multiwayvcov)

lm1c <- lm(name_recall ~ treat_ad, d.1[d.1$studyno == 1])
summary(lm1c)
get_CLUSTER_SE(lm1c, d.1, d.1$studyno, 1)
get_CLUSTER_CONF(lm1c, d.1, d.1$studyno, 1)

```
**We see the confidence interval is [0.14619376, 0.21874363] for the intercept. We see the confidence interval is [-0.05639555, 0.03679977] for treat_ad**

d. Again, run a regression to test for the effect of the ad on candidate name recognition using clustered standard errors, but this time conduct it only for Study 2. How can you employ some form of slicing to make the code you've written in parts (c) and (d) very similar? 

```{r} 
lm1d <- lm(name_recall ~ treat_ad, d.1[d.1$studyno == 2])
summary(lm1d)
coeftest(lm1d, vcov = get_VCOC_CL(lm1d, d.1, d.1$studyno, 2))
get_CLUSTER_CONF(lm1d, d.1, d.1$studyno, 2)
```
**We see the confidence interval is [0.57010652, 0.64147033] for the intercept. We see the confidence interval is [-0.07245159, 0.06684489] for treat_ad. It is similar to the last part in that all we do is change the study no.**


e. Run a regression to test for the effect of the ad on candidate name recognition, but this time the entire sample from both studies. Do not take into account which study the data is from (more on this in a moment), but just pool the data. What is the treatment effect estimate? Are you surprised of where it this estimate compared to the estimate on the two subsets of data? What is the p-value associated with this test? 

```{r}

total_lin_reg <- lm(name_recall ~ treat_ad, d.1)
coefci(total_lin_reg, vcov. = vcovCL(total_lin_reg, cluster=d.1[ , cluster]))
coeftest(total_lin_reg, vcov = vcovCL(total_lin_reg, cluster=d.1[ , cluster]))
```
**The 95% Cluster CI for Intercept is [0.4177710, 0.4906211]. The 95% Cluster CI for treat_ad is [-0.2074875, -0.1026590]**


**The treatment effect is: -0.155073 he p value is 7.344e-09**


**Increased advertising had a negative effect, so I am surprised. It is extremely significant when you pool the studies, but it doesn't make sense to pool because they are seperate studies.**


f. Now, the last question-part, but this time include a variable that identifies whether an observation was generated during Study 1 or Study 2. What is estimated in the "Study 1 Fixed Effect"? What is the treatment effect estimate and associated p-value? Think a little bit more about the treatment effect that you've estimated: can this treatment effect be *different* between Study 1 and Study 2? Why or why not? 

```{r}

d.1$indicator[d.1$studyno == 1] <- 0
d.1$indicator[d.1$studyno == 2] <- 1

lin_reg_indicator <- lm(name_recall ~ treat_ad + indicator, d.1)
coeftest(lin_reg_indicator, vcov = vcovCL(lin_reg_indicator, cluster=d.1[ , cluster]))
get_CONF(lin_reg_indicator)
```
**The treatment effect is -0.0067752, with a p value of 0.74 The cluster CI from Intercept is [0.15145341, 0.20991620]. The cluster CI from treat_ad is [-0.04196815, 0.02841765]. The cluster CI from indicator is [0.39073933, 0.46145831]. The study 1 fixed effect is 0.4260988. The treatment effect can be different because they are inherently different studies!**

g. Conduct a formal test -- it must have a p-value associated with the test -- for whether the treatment effects are different in Study 1 than Study 2. If they are different, why do you suppose they differ? Is one of the results "biased"? Why or why not? (Hint: see pages 75-76 of Gerber and Green, with more detailed discussion optionally available on pages 116-121.)

```{r}

lin_reg_g<- lm(name_recall ~ treat_ad + indicator + treat_ad*indicator, d.1)
coeftest(lin_reg_g, vcov =vcovCL(lin_reg_g, cluster=d.1[ , cluster]))
get_CONF(lin_reg_g)

```
**We see the p value to be 0.0069945. One difference is that we used indicator variable that told us the study#, and then turned out to be stat significant. This was omitted with part e, but included in part f. We know that e is biased because we failed to include the study number, which accounted for a massive part of the variation. This was all given to the treatment.**

**In either study, the treatment was not significant. ALl we can say is there is a difference in the two studies.**


# 2. Peruvian Recycling 

Look at [this article](./readings/recycling_peru.pdf) about encouraging recycling in Peru.  The paper contains two experiments, a "participation study" and a "participation intensity study."  In this problem, we will focus on the latter study, whose results are contained in Table 4 in this problem.  You will need to read the relevant section of the paper (starting on page 20 of the manuscript) in order to understand the experimental design and variables.  (*Note that "indicator variable" is a synonym for "dummy variable," in case you haven't seen this language before.*)

a. In Column 3 of Table 4A, what is the estimated ATE of providing a recycling bin on the average weight of recyclables turned in per household per week, during the six-week treatment period?  Provide a 95% confidence interval.

**The ATE is 0.187kg, with a confidence interval of .187 +/ 1.96 * (0.032) = [.124, .250]. This result is stat significant**

b. In Column 3 of Table 4A, what is the estimated ATE of sending a text message reminder on the average weight of recyclables turned in per household per week?  Provide a 95% confidence interval.

**The ATE is -0.024 kg for any household with a text messaged, with a confidence interval of -0.024 +/ 1.96 * (0.039) = [-.100, .052]. This result is not stat significant.**

c. Which outcome measures in Table 4A show statistically significant effects (at the 5% level) of providing a recycling bin?

**Everything except average contamination per week is significant. This would include, average market value of recyclables, average weight of recyclables, # of bins, and % of visits turned into baf.**

d. Which outcome measures in Table 4A show statistically significant effects (at the 5% level) of sending text messages?

**None of the outcomes measures of sending text messages are stat significant.**

e. Suppose that, during the two weeks before treatment, household A turns in 2kg per week more recyclables than household B does, and suppose that both households are otherwise identical (including being in the same treatment group).  From the model, how much more recycling do we predict household A to have than household B, per week, during the six weeks of treatment?   Provide only a point estimate, as the confidence interval would be a bit complicated.  This question is designed to test your understanding of slope coefficients in regression.

**We see that for recylcable weight, there is a covariate that is 0.281. Thus, a house (house A) that has 2 more kg of recyclable weight during the pre treatment weeks would then have 2x(0.281) = 0.562 kg more than recyclable waste in comparison to house B.**

f. Suppose that the variable "percentage of visits turned in bag, baseline" had been left out of the regression reported in Column 1.  What would you expect to happen to the results on providing a recycling bin?  Would you expect an increase or decrease in the estimated ATE?  Would you expect an increase or decrease in the standard error?  Explain your reasoning.

**It is likely that the SE would increase, and the ATE would increase as well. Without the covraiate, we know there would be a higher variance when the covariate is left out. We can compare the covariate value of 0.0374 to the any bin value of 0.045. Given how much bigger the covariate value is, we know that it is stat significant. There is some correlation between #of bins and percentags of bags. Thus, we know the ATE would be overestimated**


g. In column 1 of Table 4A, would you say the variable "has cell phone" is a bad control?  Explain your reasoning.

**Has cellphone is not a bad control because it is measured before the experiment occurs, and thus is not affected by the result of the experiment itsself. We are ultimately trying to find out if sending a text message does anything to recycling, thus if a subject doesn't have a phone, they would not have receieved the text message.**

h. If we were to remove the "has cell phone" variable from the regression, what would you expect to happen to the coefficient on "Any SMS message"?  Would it go up or down? Explain your reasoning.

**We are simply going to increase the variance without the covariate. Thus, the coefficeint of "Any SMS message" would increase, to take part of the variation that "has cellphone" was explaning.**

# 3. Multifactor Experiments 

Staying with the same experiment, now lets think about multifactor experiments. 

a. What is the full experimental design for this experiment?  Tell us the dimensions, such as 2x2x3.  (Hint: the full results appear in Panel 4B.)

**We see a 3x3 experiment design. There is bin treatment, where subjects get no bin, bin without sticker, and bin with informational sticker. There is also an SMS treatment, where subjects get no SMS, generic SMS reminder, and a personalized SMS reminder. You can think of it as a 3x3x2 if you look at cell phone v no cell phone, but it was taken before the study, so if that's ok, you can think of it as 3x3x2.**

b. In the results of Table 4B, describe the baseline category. That is, in English, how would you describe the attributes of the group of people for whom all dummy variables are equal to zero?

**The baseline category represent variables that were taken 2 weeks before the experiment. In terms of numbers, no bins = .374 bins turned in, no kg weight leads to .281 bins in, no MV for recyclables results in 0.233 bins, and no % contamination leads to an additional 0.292 bins. We include them in the regression.**


c. In column (1) of Table 4B, interpret the magnitude of the coefficient on "bin without sticker."  What does it mean?

**We see a value of 0.35, which is statistically significant because its more than twice its SE. A confidence interval at the 95th percentage would be .035 +/ 2 * (0.015) which is [0.0644, 0.0056]. Thus, we know there is a positive effect on # of bins turned in. Specifically, someone with a bin w/o sticker would turn in 0.35 more bins than someone without a bin w/o sticker.**

d. In column (1) of Table 4B, which seems to have a stronger treatment effect, the recycling bin with message sticker, or the recycling bin without sticker?  How large is the magnitude of the estimated difference?

**The first one, recycling bin with message has a treatment effect of 0.055, vs bin without sticker is at 0.035. The difference is 0.02. Note that bin w/ sticker is significant at the 1% level, vs bin w/o sticker which is significant at the 5% level.**

e. Is this difference you just described statistically significant?  Explain which piece of information in the table allows you to answer this question.

**No the difference is not statistically significant. We see the difference being 0.02, but 2 * SE is actually 0.03. Thus, we do not see a statistically significant difference.**

f. Notice that Table 4C is described as results from "fully saturated" models.  What does this mean?  Looking at the list of variables in the table, explain in what sense the model is "saturated."

**A fully saturated model has dummy/indicator variables for every mix of the treatment. All interaction terms are included. In this case, we see no phone, SMS, Bin, bin w/o sticker, and bin w/ sticker**

# 4. Now! Do it with data 
Download the data set for the recycling study in the previous problem, obtained from the authors. We'll be focusing on the outcome variable Y="number of bins turned in per week" (avg_bins_treat).

```{r}
d.4 <- read.dta("./data/karlan_data_subset_for_class.dta")
d.4 <- data.table(d.4)
head(d.4)


# Turn anything less than 0 into NA for street
d.4$street[d.4$street < 0] <- NA

# Check all the unique values. Havecell has a couple NAs, as does street. We must drop these.

unique(d.4$street)
unique(d.4$havecell)
d.4 <- na.omit(d.4, cols = c("street", "havecell"))

```
**From the summary, we see the min value for street is -999. We can think about excluding this value. We also drop all NAs in havecell and street**


a. For simplicity, let's start by measuring the effect of providing a recycling bin, ignoring the SMS message treatment (and ignoring whether there was a sticker on the bin or not).  Run a regression of Y on only the bin treatment dummy, so you estimate a simple difference in means.  Provide a 95% confidence interval for the treatment effect.

```{r}

linReg4a <- lm(avg_bins_treat ~ bin, data = d.4)
coeftest(linReg4a, get_VCOC_HC(linReg4a))
get_CONF(linReg4a)

```
**The treatment effect is 0.13470 (0.02133). We see a 95% interval for bin to be [0.09285793, 0.1765327]**


b. Now add the pre-treatment value of Y as a covariate.  Provide a 95% confidence interval for the treatment effect.  Explain how and why this confidence interval differs from the previous one.

```{r}
linReg4b = lm(avg_bins_treat ~ bin + base_avg_bins_treat, data = d.4)
get_CONF(linReg4b)

```
**Now our confidence interval is [0.09230067, 0.1619122]. Adding the pre treatment Y reduces the SE and thus the CI because it explains for some of the variation.**


c. Now add the street fixed effects.  (You'll need to use the R command factor().) Provide a 95% confidence interval for the treatment effect.  

```{r}

linReg4c = lm(avg_bins_treat ~ bin + factor(street) + base_avg_bins_treat, data = d.4)
get_CONF(linReg4c)[2, ]


```
**We see a 95% confidence interval of [0.07755027, 0.15457796]**

d. Recall that the authors described their experiment as "stratified at the street level," which is a synonym for blocking by street.  Explain why the confidence interval with fixed effects does not differ much from the previous one.

**This is because baseline takes on the effect of the street. A neighborhood might happen to recycle more, which would be seen in the baseline itsself.**

e. Perhaps having a cell phone helps explain the level of recycling behavior. Instead of "has cell phone," we find it easier to interpret the coefficient if we define the variable " no cell phone."  Give the R command to define this new variable, which equals one minus the "has cell phone" variable in the authors' data set.  Use "no cell phone" instead of "has cell phone" in subsequent regressions with this dataset.

```{r}
d.4$no_phone <- 1 - d.4$havecell
```

f. Now add "no cell phone" as a covariate to the previous regression.  Provide a 95% confidence interval for the treatment effect.  Explain why this confidence interval does not differ much from the previous one.

```{r}

linReg4f = lm(avg_bins_treat ~ bin +  base_avg_bins_treat + no_phone + factor(street), data = d.4)
get_CONF(linReg4f)[1:5, ]
```
**We did not expect it to have an effect on the outcome. Having a cell phone was a pre treatment measure that was taken before the experiment was conducted.**

**The 95% confidence interval is [0.07865135, 0.1556875]. Blocking clearly reduces variability.**

g. Now let's add in the SMS treatment.  Re-run the previous regression with "any SMS" included.  You should get the same results as in Table 4A.  Provide a 95% confidence interval for the treatment effect of the recycling bin.  Explain why this confidence interval does not differ much from the previous one.

```{r}

linReg4g = lm(avg_bins_treat ~ bin + sms +  base_avg_bins_treat + no_phone + factor(street), data = d.4)
print("95% CI")
get_CONF(linReg4g)[1:6, ]
print("Coefficients")
linReg4g$coefficients[1:5]

```
**The 95% interval is [0.07843089, 0.15550473]**
**SMS doesn't explain the variance of the treatment, because sending an SMS when there is a bin doesn't do much more.**




h. Now reproduce the results of column 2 in Table 4B, estimating separate treatment effects for the two types of SMS treatments and the two types of recycling-bin treatments.  Provide a 95% confidence interval for the effect of the unadorned recycling bin.  Explain how your answer differs from that in part (g), and explain why you think it differs.

```{r}
linReg4h = lm(avg_bins_treat ~ bin + sms +  base_avg_bins_treat + no_phone + bin_s + bin_g + sms_p + sms_g  + factor(street), data = d.4)
get_CONF(linReg4h)[1:10, ]
linReg4h$coefficients[1:10]
```
**The 95% interval is [0.05464380, 0.15683344]**
**The confidence interval is smaller than above, meaning that bing/bins and smsp/smsg accounted for some of the variation, leading to a smaller confidence interval.**

# 5. A Final Practice Problem 

Now for a fictional scenario. An emergency two-week randomized controlled trial of the experimental drug ZMapp is conducted to treat Ebola. (The control represents the usual standard of care for patients identified with Ebola, while the treatment is the usual standard of care plus the drug.) 

Here are the (fake) data. 

```{r}
d.5 <- fread("./data/Ebola_rct2.csv")
head(d.5)
```

You are asked to analyze it. Patients' temperature and whether they are vomiting is recorded on day 0 of the experiment, then ZMapp is administered to patients in the treatment group on day 1. Vomiting and temperature is again recorded on day 14.

a. Without using any covariates, answer this question with regression: What is the estimated effect of ZMapp (with standard error in parentheses) on whether someone was vomiting on day 14? What is the p-value associated with this estimate?

```{r}
linReg5a <- lm(vomiting_day14 ~ treat_zmapp, data = d.5)
coeftest(linReg5a, get_VCOC_HC(linReg5a))
summary(linReg5a)

```
**The estimateed effect of ZMapp is -0.237702 (0.091459), and the p value is 0.01079 which would make it stat significant at the 5% level.**


b. Add covariates for vomiting on day 0 and patient temperature on day 0 to the regression from part (a) and report the ATE (with standard error). Also report the p-value.

```{r}
linReg5b <- lm(vomiting_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0, data = d.5)
coeftest(linReg5b, get_VCOC_HC(linReg5b))
summary(linReg5b)
```
**The ATE is -0.165537 (0.081976), with a p value of 0.046242, which is significant at the 5% level.**


c. Do you prefer the estimate of the ATE reported in part (a) or part (b)? Why? Report the results of the F-test that you used to form this opinion. 

**I prefer the value in B, because we see that temperature_day is a covariate that explains for some variation, lowering the CI. Also note that the F value from the summaries above (7.705, 14.447) and R squared values are much higher in part B.**

d. The regression from part (b) suggests that temperature is highly predictive of vomiting. Also include temperature on day 14 as a covariate in the regression from part (b) and report the ATE, the standard error, and the p-value.

```{r}
linReg5d <- lm(vomiting_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0 + temperature_day14, data = d.5)
coeftest(linReg5d, get_VCOC_HC(linReg5d))
summary(linReg5d)

```
**The ATE is -0.120101 (0.085798) with a p value of 0.164829. We can see that temperature_day14 does help predict the vomitting, and we also see that treat_zmapp is not signifcant at the 5% level.**


e. Do you prefer the estimate of the ATE reported in part (b) or part (d)? Why?

**I prefer the estimate in b because using tempature on day 14 could be a result of the medication/other factors. It could also be affected by other variables, and thus is not a control that should be used. We see a bad control, it is an outcome**

f. Now let's switch from the outcome of vomiting to the outcome of temperature, and use the same regression covariates as in part (b). Test the hypothesis that ZMapp is especially likely to reduce mens' temperatures, as compared to womens', and describe how you did so. What do the results suggest?

```{r}
linReg5g <- lm(temperature_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0 + male*treat_zmapp, data = d.5)
coeftest(linReg5g, get_VCOC_HC(linReg5g))
```


**With a stat of -2.076686(0.198386), there is stat significant evidence that being a male would reduce temp by 2.08 degrees more than being a female. I include an interaction term, then proving the result.**


g. Suspend reality for just a moment -- suppose that you had the option of being a man or a woman who was a part of this study. Based on this data, which sex would you rather be? This time, you need to produce evidence (probably from your model estimates) to inform your determination. What does your determination depend on? 

```{r}
male_data <- d.5[d.5$male == 1]
female_data <- d.5[d.5$male == 0]

print("Male")
linReg5fmale <- lm(temperature_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0, data = male_data)
coeftest(linReg5fmale, get_VCOC_HC(linReg5fmale))


print("Female")
linReg5ffemale <- lm(temperature_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0, data = female_data)
coeftest(linReg5ffemale, get_VCOC_HC(linReg5ffemale))


get_CONF(linReg5fmale)
get_CONF(linReg5ffemale)
```
**When looking at male v female, I ran 2 regressions, one with only males and one with only females. We see -2.23910(0.17738) for males and -0.22908(0.11906) for females, suggesting that when looking at CI, we get: [-2.5999924, -1.8782122] for males and [-0.4673268, 0.009167394] for females. It would then tend to suggest that ZMapp reduces Men's temp a lot more than females temp. The two p values we get are 3.517e-14 for males and 0.05918 for females. I would rather be a man.**

h. Suppose that you had not run the regression in part (f). Instead, you speak with a colleague to learn about heterogeneous treatment effects. This colleague has access to a non-anonymized version of the same dataset and reports that he had looked at heterogeneous effects of the ZMapp treatment by each of 10,000 different covariates to examine whether each predicted the effectiveness of ZMapp on each of 2,000 different indicators of health, for 20,000,000 different regressions in total. Across these 20,000,000 regressions your colleague ran, the treatment's interaction with gender on the outcome of temperature is the only heterogeneous treatment effect that he found to be statistically significant. He reasons that this shows the importance of gender for understanding the effectiveness of the drug, because nothing else seemed to indicate why it worked. Bolstering his confidence, after looking at the data, he also returned to his medical textbooks and built a theory about why ZMapp interacts with processes only present in men to cure. Another doctor, unfamiliar with the data, hears his theory and finds it plausible. How likely do you think it is ZMapp works especially well for curing Ebola in men, and why? (This question is conceptual can be answered without performing any computation.)

**We see the multiple comparisons problem in full effect there. The scientist ran 20 million regressions, and about 5% of his results are due to chance. Thus, it is possible that the gender covariate is something he sees because he tested so many different hypothesis. To see if ZMapp works in curing men, a second experiment should be conducted where the only covariate analyzed is men. If we again see a stat significant result, then we can believe the result. It may also make sense to apply Bonferroni correction to the p value required to produce a statistically significant outcome. A true fishing expedition.**


i. Now, imagine that what described in part (g) did not happen, but that you had tested this heterogeneous treatment effect, and only this heterogeneous treatment effect, of your own accord. Would you be more or less inclined to believe that the heterogeneous treatment effect really exists? Why?

**I would be more inclined to believe the heterogeneous treatment effect, if we varied the sex as well as the treatment. Because we are running just 1 regression, we are sure that the result (if stat significant), would only happen by chance 5% of the time.**

j. Another colleague proposes that being of African descent causes one to be more likely to get Ebola. He asks you what ideal experiment would answer this question. What would you tell him?  (*Hint: refer to Chapter 1 of Mostly Harmless Econometrics.*)

**This question is FUQed - it is a fundamentally unanswerable question. If there was a more precise question, then perhaphs it could be answered; however, this one is too vague.**

**What does African Descent mean, and what does it mean to "be more likely." Moreover, this experiment on the face would be incredibly hard to execute because there would be no way to randomize descent, and it would be tough to follow one's life tracking for Ebola. There are also a good amount of covariates that need to be taken into account, and could easily be ignored. One solution would be to change the African descent part to spending x amount of years in Africa, or something along those lines.**

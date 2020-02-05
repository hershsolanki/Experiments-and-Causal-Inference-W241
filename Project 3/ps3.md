Problem Set 3
================
Experiments and Causality

``` r
# load packages 
library(data.table)
library(foreign)
```

# 0\. Write Functions

You’re going to be doing a few things a *number* of times – calculating
robust standard errors, calculating clustered standard errors, and then
calculating the confidence intervals that are built off these standard
errors.

*After* you’ve worked through a few of these questions, I suspect you
will see places to write a function that will do this work for you.
Include those functions here, if you write them.

``` r
library(sandwich)
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
get_VCOC_HC <- function(lm) {
  vcovHC <- vcovHC(lm)
  return (vcovHC)
}

get_ROBUST_SE <- function(lm) {
  print("Robust SE")
  return(sqrt(diag(vcovHC)))
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

# 1\. Replicate Results

Skim [Broockman and Green’s](./readings/brookman_green_ps3.pdf) paper on
the effects of Facebook ads and download an anonymized version of the
data for Facebook users only.

``` r
d.1 <- fread("./data/broockman_green_anon_pooled_fb_users_only.csv")
head(d.1)
```

    ##    studyno treat_ad                   cluster name_recall
    ## 1:       2        0 Study 2, Cluster Number 1           0
    ## 2:       2        0 Study 2, Cluster Number 2           1
    ## 3:       2        0 Study 2, Cluster Number 3           0
    ## 4:       2        0 Study 2, Cluster Number 4           1
    ## 5:       2        1 Study 2, Cluster Number 7           1
    ## 6:       2        1 Study 2, Cluster Number 7           0
    ##    positive_impression
    ## 1:                   0
    ## 2:                   0
    ## 3:                   0
    ## 4:                   0
    ## 5:                   1
    ## 6:                   0

``` r
dim(d.1[d.1$studyno == 1])
```

    ## [1] 1364    5

1.  Using regression without clustered standard errors (that is,
    ignoring the clustered assignment), compute a confidence interval
    for the effect of the ad on candidate name recognition in Study 1
    only (the dependent variable is “name\_recall”).

<!-- end list -->

  - **Note**: Ignore the blocking the article mentions throughout this
    problem.
  - **Note**: You will estimate something different than is reported in
    the study.

<!-- end list -->

``` r
get_CONF(lm(name_recall ~ treat_ad, data = d.1[d.1$studyno == 1]))
```

    ## [1] "CI"

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.15036520 0.21457219
    ## treat_ad    -0.05121351 0.03161774

**We see a 95% confidence interval for the effect of the ad to be
\[-0.05121351, 0.03161774\]**

2.  What are the clusters in Broockman and Green’s study? Why might
    taking clustering into account increase the standard errors?

**Clusters are individuals who have the same age/gender/location.
Because there is not much variation within clusters, but varation
between clusters, we must adjust for this. Thus, taking clustering into
account should increase the standard errors.**

3.  Now estimate a regression that estimates the effect of the ad on
    candidate name recognition in Study 1, but this time take take
    clustering into account. (hint: The estimation of the *model* does
    not change, only the estimation of the standard errors.) If you’re
    not familiar with how to calculate these clustered and robust
    estimates, there is a demo worksheet that is available in our course
    repository:
    [`./week_05/cluster_and_robust.Rmd`](http://datahub.berkeley.edu/hub/user-redirect/git-pull?repo=https://github.com/UCB-MIDS/w241&urlpath=rstudio/).

<!-- end list -->

``` r
library(lmtest)
library(multiwayvcov)

lm1c <- lm(name_recall ~ treat_ad, d.1[d.1$studyno == 1])
summary(lm1c)
```

    ## 
    ## Call:
    ## lm(formula = name_recall ~ treat_ad, data = d.1[d.1$studyno == 
    ##     1])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.1825 -0.1825 -0.1727 -0.1727  0.8273 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.182469   0.016142  11.304   <2e-16 ***
    ## treat_ad    -0.009798   0.021012  -0.466    0.641    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3817 on 1362 degrees of freedom
    ## Multiple R-squared:  0.0001596,  Adjusted R-squared:  -0.0005745 
    ## F-statistic: 0.2174 on 1 and 1362 DF,  p-value: 0.6411

``` r
get_CLUSTER_SE(lm1c, d.1, d.1$studyno, 1)
```

    ## [1] "Clustered SE"

    ## (Intercept)    treat_ad 
    ##  0.01849151  0.02375363

``` r
get_CLUSTER_CONF(lm1c, d.1, d.1$studyno, 1)
```

    ## [1] "Clustered CI"

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.14619376 0.21874363
    ## treat_ad    -0.05639555 0.03679977

**We see the confidence interval is \[0.14619376, 0.21874363\] for the
intercept. We see the confidence interval is \[-0.05639555, 0.03679977\]
for treat\_ad**

4.  Again, run a regression to test for the effect of the ad on
    candidate name recognition using clustered standard errors, but this
    time conduct it only for Study 2. How can you employ some form of
    slicing to make the code you’ve written in parts (c) and (d) very
    similar?

<!-- end list -->

``` r
lm1d <- lm(name_recall ~ treat_ad, d.1[d.1$studyno == 2])
summary(lm1d)
```

    ## 
    ## Call:
    ## lm(formula = name_recall ~ treat_ad, data = d.1[d.1$studyno == 
    ##     2])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.6058 -0.6058  0.3942  0.3942  0.3970 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.605788   0.015454  39.199   <2e-16 ***
    ## treat_ad    -0.002803   0.030874  -0.091    0.928    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4892 on 1335 degrees of freedom
    ##   (5 observations deleted due to missingness)
    ## Multiple R-squared:  6.176e-06,  Adjusted R-squared:  -0.0007429 
    ## F-statistic: 0.008245 on 1 and 1335 DF,  p-value: 0.9277

``` r
coeftest(lm1d, vcov = get_VCOC_CL(lm1d, d.1, d.1$studyno, 2))
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.6057884  0.0181889  33.305   <2e-16 ***
    ## treat_ad    -0.0028033  0.0355033  -0.079   0.9371    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
get_CLUSTER_CONF(lm1d, d.1, d.1$studyno, 2)
```

    ## [1] "Clustered CI"

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.57010652 0.64147033
    ## treat_ad    -0.07245159 0.06684489

**We see the confidence interval is \[0.57010652, 0.64147033\] for the
intercept. We see the confidence interval is \[-0.07245159, 0.06684489\]
for treat\_ad. It is similar to the last part in that all we do is
change the study no.**

5.  Run a regression to test for the effect of the ad on candidate name
    recognition, but this time the entire sample from both studies. Do
    not take into account which study the data is from (more on this in
    a moment), but just pool the data. What is the treatment effect
    estimate? Are you surprised of where it this estimate compared to
    the estimate on the two subsets of data? What is the p-value
    associated with this test?

<!-- end list -->

``` r
total_lin_reg <- lm(name_recall ~ treat_ad, d.1)
coefci(total_lin_reg, vcov. = vcovCL(total_lin_reg, cluster=d.1[ , cluster]))
```

    ##                  2.5 %     97.5 %
    ## (Intercept)  0.4177710  0.4906211
    ## treat_ad    -0.2074875 -0.1026590

``` r
coeftest(total_lin_reg, vcov = vcovCL(total_lin_reg, cluster=d.1[ , cluster]))
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  0.454196   0.018576 24.4504 < 2.2e-16 ***
    ## treat_ad    -0.155073   0.026730 -5.8014 7.344e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**The 95% Cluster CI for Intercept is \[0.4177710, 0.4906211\]. The 95%
Cluster CI for treat\_ad is \[-0.2074875, -0.1026590\]**

**The treatment effect is: -0.155073 he p value is 7.344e-09**

**Increased advertising had a negative effect, so I am surprised. It is
extremely significant when you pool the studies, but it doesn’t make
sense to pool because they are seperate studies.**

6.  Now, the last question-part, but this time include a variable that
    identifies whether an observation was generated during Study 1 or
    Study 2. What is estimated in the “Study 1 Fixed Effect”? What is
    the treatment effect estimate and associated p-value? Think a little
    bit more about the treatment effect that you’ve estimated: can this
    treatment effect be *different* between Study 1 and Study 2? Why or
    why not?

<!-- end list -->

``` r
d.1$indicator[d.1$studyno == 1] <- 0
d.1$indicator[d.1$studyno == 2] <- 1

lin_reg_indicator <- lm(name_recall ~ treat_ad + indicator, d.1)
coeftest(lin_reg_indicator, vcov = vcovCL(lin_reg_indicator, cluster=d.1[ , cluster]))
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.1806848  0.0169702 10.6472   <2e-16 ***
    ## treat_ad    -0.0067752  0.0204154 -0.3319     0.74    
    ## indicator    0.4260988  0.0206969 20.5875   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
get_CONF(lin_reg_indicator)
```

    ## [1] "CI"

    ##                   2.5 %     97.5 %
    ## (Intercept)  0.15145341 0.20991620
    ## treat_ad    -0.04196815 0.02841765
    ## indicator    0.39073933 0.46145831

**The treatment effect is -0.0067752, with a p value of 0.74 The cluster
CI from Intercept is \[0.15145341, 0.20991620\]. The cluster CI from
treat\_ad is \[-0.04196815, 0.02841765\]. The cluster CI from indicator
is \[0.39073933, 0.46145831\]. The study 1 fixed effect is 0.4260988.
The treatment effect can be different because they are inherently
different studies\!**

7.  Conduct a formal test – it must have a p-value associated with the
    test – for whether the treatment effects are different in Study 1
    than Study 2. If they are different, why do you suppose they differ?
    Is one of the results “biased”? Why or why not? (Hint: see pages
    75-76 of Gerber and Green, with more detailed discussion optionally
    available on pages
116-121.)

<!-- end list -->

``` r
lin_reg_g<- lm(name_recall ~ treat_ad + indicator + treat_ad*indicator, d.1)
coeftest(lin_reg_g, vcov =vcovCL(lin_reg_g, cluster=d.1[ , cluster]))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)         0.1824687  0.0184880  9.8696   <2e-16 ***
    ## treat_ad           -0.0097979  0.0237491 -0.4126   0.6800    
    ## indicator           0.4233197  0.0259296 16.3257   <2e-16 ***
    ## treat_ad:indicator  0.0069945  0.0427010  0.1638   0.8699    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
get_CONF(lin_reg_g)
```

    ## [1] "CI"

    ##                          2.5 %     97.5 %
    ## (Intercept)         0.15037933 0.21455806
    ## treat_ad           -0.05119529 0.03159951
    ## indicator           0.37918443 0.46745503
    ## treat_ad:indicator -0.06646311 0.08045219

**We see the p value to be 0.0069945. One difference is that we used
indicator variable that told us the study\#, and then turned out to be
stat significant. This was omitted with part e, but included in part f.
We know that e is biased because we failed to include the study number,
which accounted for a massive part of the variation. This was all given
to the treatment.**

**In either study, the treatment was not significant. ALl we can say is
there is a difference in the two studies.**

# 2\. Peruvian Recycling

Look at [this article](./readings/recycling_peru.pdf) about encouraging
recycling in Peru. The paper contains two experiments, a “participation
study” and a “participation intensity study.” In this problem, we will
focus on the latter study, whose results are contained in Table 4 in
this problem. You will need to read the relevant section of the paper
(starting on page 20 of the manuscript) in order to understand the
experimental design and variables. (*Note that “indicator variable” is a
synonym for “dummy variable,” in case you haven’t seen this language
before.*)

1.  In Column 3 of Table 4A, what is the estimated ATE of providing a
    recycling bin on the average weight of recyclables turned in per
    household per week, during the six-week treatment period? Provide a
    95% confidence interval.

**The ATE is 0.187kg, with a confidence interval of .187 +/ 1.96 \*
(0.032) = \[.124, .250\]. This result is stat significant**

2.  In Column 3 of Table 4A, what is the estimated ATE of sending a text
    message reminder on the average weight of recyclables turned in per
    household per week? Provide a 95% confidence interval.

**The ATE is -0.024 kg for any household with a text messaged, with a
confidence interval of -0.024 +/ 1.96 \* (0.039) = \[-.100, .052\]. This
result is not stat significant.**

3.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of providing a recycling bin?

**Everything except average contamination per week is significant. This
would include, average market value of recyclables, average weight of
recyclables, \# of bins, and % of visits turned into baf.**

4.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of sending text messages?

**None of the outcomes measures of sending text messages are stat
significant.**

5.  Suppose that, during the two weeks before treatment, household A
    turns in 2kg per week more recyclables than household B does, and
    suppose that both households are otherwise identical (including
    being in the same treatment group). From the model, how much more
    recycling do we predict household A to have than household B, per
    week, during the six weeks of treatment? Provide only a point
    estimate, as the confidence interval would be a bit complicated.
    This question is designed to test your understanding of slope
    coefficients in regression.

**We see that for recylcable weight, there is a covariate that is 0.281.
Thus, a house (house A) that has 2 more kg of recyclable weight during
the pre treatment weeks would then have 2x(0.281) = 0.562 kg more than
recyclable waste in comparison to house B.**

6.  Suppose that the variable “percentage of visits turned in bag,
    baseline” had been left out of the regression reported in Column 1.
    What would you expect to happen to the results on providing a
    recycling bin? Would you expect an increase or decrease in the
    estimated ATE? Would you expect an increase or decrease in the
    standard error? Explain your reasoning.

**It is likely that the SE would increase, and the ATE would increase as
well. Without the covraiate, we know there would be a higher variance
when the covariate is left out. We can compare the covariate value of
0.0374 to the any bin value of 0.045. Given how much bigger the
covariate value is, we know that it is stat significant. There is some
correlation between \#of bins and percentags of bags. Thus, we know the
ATE would be overestimated**

7.  In column 1 of Table 4A, would you say the variable “has cell phone”
    is a bad control? Explain your reasoning.

**Has cellphone is not a bad control because it is measured before the
experiment occurs, and thus is not affected by the result of the
experiment itsself. We are ultimately trying to find out if sending a
text message does anything to recycling, thus if a subject doesn’t have
a phone, they would not have receieved the text message.**

8.  If we were to remove the “has cell phone” variable from the
    regression, what would you expect to happen to the coefficient on
    “Any SMS message”? Would it go up or down? Explain your reasoning.

**We are simply going to increase the variance without the covariate.
Thus, the coefficeint of “Any SMS message” would increase, to take part
of the variation that “has cellphone” was explaning.**

# 3\. Multifactor Experiments

Staying with the same experiment, now lets think about multifactor
experiments.

1.  What is the full experimental design for this experiment? Tell us
    the dimensions, such as 2x2x3. (Hint: the full results appear in
    Panel 4B.)

**We see a 3x3 experiment design. There is bin treatment, where subjects
get no bin, bin without sticker, and bin with informational sticker.
There is also an SMS treatment, where subjects get no SMS, generic SMS
reminder, and a personalized SMS reminder. You can think of it as a
3x3x2 if you look at cell phone v no cell phone, but it was taken before
the study, so if that’s ok, you can think of it as 3x3x2.**

2.  In the results of Table 4B, describe the baseline category. That is,
    in English, how would you describe the attributes of the group of
    people for whom all dummy variables are equal to zero?

**The baseline category represent variables that were taken 2 weeks
before the experiment. In terms of numbers, no bins = .374 bins turned
in, no kg weight leads to .281 bins in, no MV for recyclables results in
0.233 bins, and no % contamination leads to an additional 0.292 bins. We
include them in the regression.**

3.  In column (1) of Table 4B, interpret the magnitude of the
    coefficient on “bin without sticker.” What does it mean?

**We see a value of 0.35, which is statistically significant because its
more than twice its SE. A confidence interval at the 95th percentage
would be .035 +/ 2 \* (0.015) which is \[0.0644, 0.0056\]. Thus, we know
there is a positive effect on \# of bins turned in. Specifically,
someone with a bin w/o sticker would turn in 0.35 more bins than someone
without a bin w/o sticker.**

4.  In column (1) of Table 4B, which seems to have a stronger treatment
    effect, the recycling bin with message sticker, or the recycling bin
    without sticker? How large is the magnitude of the estimated
    difference?

**The first one, recycling bin with message has a treatment effect of
0.055, vs bin without sticker is at 0.035. The difference is 0.02. Note
that bin w/ sticker is significant at the 1% level, vs bin w/o sticker
which is significant at the 5% level.**

5.  Is this difference you just described statistically significant?
    Explain which piece of information in the table allows you to answer
    this question.

**No the difference is not statistically significant. We see the
difference being 0.02, but 2 \* SE is actually 0.03. Thus, we do not see
a statistically significant difference.**

6.  Notice that Table 4C is described as results from “fully saturated”
    models. What does this mean? Looking at the list of variables in the
    table, explain in what sense the model is “saturated.”

**A fully saturated model has dummy/indicator variables for every mix of
the treatment. All interaction terms are included. In this case, we see
no phone, SMS, Bin, bin w/o sticker, and bin w/ sticker**

# 4\. Now\! Do it with data

Download the data set for the recycling study in the previous problem,
obtained from the authors. We’ll be focusing on the outcome variable
Y=“number of bins turned in per week” (avg\_bins\_treat).

``` r
d.4 <- read.dta("./data/karlan_data_subset_for_class.dta")
d.4 <- data.table(d.4)
head(d.4)
```

    ##    street havecell avg_bins_treat base_avg_bins_treat bin sms bin_s bin_g
    ## 1:      7        1      1.0416666               0.750   1   1     1     0
    ## 2:      7        1      0.0000000               0.000   0   1     0     0
    ## 3:      7        1      0.7500000               0.500   0   0     0     0
    ## 4:      7        1      0.5416667               0.500   0   0     0     0
    ## 5:      6        1      0.9583333               0.375   1   0     0     1
    ## 6:      8        0      0.2083333               0.000   1   0     0     1
    ##    sms_p sms_g
    ## 1:     0     1
    ## 2:     1     0
    ## 3:     0     0
    ## 4:     0     0
    ## 5:     0     0
    ## 6:     0     0

``` r
# Turn anything less than 0 into NA for street
d.4$street[d.4$street < 0] <- NA

# Check all the unique values. Havecell has a couple NAs, as does street. We must drop these.

unique(d.4$street)
```

    ##   [1]   7   6   8   5   9  10  NA  11  17   3  45  46  47  63  62  64  78
    ##  [18]  80  70  77  66  81  73  88  86  91  89 124 138 109 125 132 121 131
    ##  [35] 149 136 106 166 196 198 188 191 216 233 225 222 221 241 244 243 236
    ##  [52]   2  22  21  20  23  37  40  41  38  61  60  75  82  67  69  74  85
    ##  [69]  79  83  84  94  96  93 137 111 115 134 105 113 112 118 110 133 107
    ##  [86] 128 130 117 126 160 153 154 157 158 156 152 155 164 163 172 171 170
    ## [103] 180 183 182 192 189 185 197 200 193 207 203 206 208 213 209 202 230
    ## [120] 232 223 240 242 253 254 263 261 260 262   4  15  44  43  42  68  72
    ## [137]  98 119 148 151 147 120 122 175 187 186 190 229 228 217 235 238 255
    ## [154] 250 248 249 247 256 259 258 257  26  53  32  58  99 103 100 102 101
    ## [171] 127 129 168 165 179 215 210 220 227 246

``` r
unique(d.4$havecell)
```

    ## [1]  1  0 NA

``` r
d.4 <- na.omit(d.4, cols = c("street", "havecell"))
```

**From the summary, we see the min value for street is -999. We can
think about excluding this value. We also drop all NAs in havecell and
street**

1.  For simplicity, let’s start by measuring the effect of providing a
    recycling bin, ignoring the SMS message treatment (and ignoring
    whether there was a sticker on the bin or not). Run a regression of
    Y on only the bin treatment dummy, so you estimate a simple
    difference in means. Provide a 95% confidence interval for the
    treatment effect.

<!-- end list -->

``` r
linReg4a <- lm(avg_bins_treat ~ bin, data = d.4)
coeftest(linReg4a, get_VCOC_HC(linReg4a))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  0.63515    0.01194 53.1954 < 2.2e-16 ***
    ## bin          0.13470    0.02133  6.3147 3.467e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
get_CONF(linReg4a)
```

    ## [1] "CI"

    ##                  2.5 %    97.5 %
    ## (Intercept) 0.61173387 0.6585719
    ## bin         0.09285793 0.1765327

**The treatment effect is 0.13470 (0.02133). We see a 95% interval for
bin to be \[0.09285793, 0.1765327\]**

2.  Now add the pre-treatment value of Y as a covariate. Provide a 95%
    confidence interval for the treatment effect. Explain how and why
    this confidence interval differs from the previous one.

<!-- end list -->

``` r
linReg4b = lm(avg_bins_treat ~ bin + base_avg_bins_treat, data = d.4)
get_CONF(linReg4b)
```

    ## [1] "CI"

    ##                          2.5 %    97.5 %
    ## (Intercept)         0.31113949 0.3976734
    ## bin                 0.09230067 0.1619122
    ## base_avg_bins_treat 0.32353600 0.4471578

**Now our confidence interval is \[0.09230067, 0.1619122\]. Adding the
pre treatment Y reduces the SE and thus the CI because it explains for
some of the variation.**

3.  Now add the street fixed effects. (You’ll need to use the R command
    factor().) Provide a 95% confidence interval for the treatment
    effect.

<!-- end list -->

``` r
linReg4c = lm(avg_bins_treat ~ bin + factor(street) + base_avg_bins_treat, data = d.4)
get_CONF(linReg4c)[2, ]
```

    ## [1] "CI"

    ##      2.5 %     97.5 % 
    ## 0.07755027 0.15457796

**We see a 95% confidence interval of \[0.07755027, 0.15457796\]**

4.  Recall that the authors described their experiment as “stratified at
    the street level,” which is a synonym for blocking by street.
    Explain why the confidence interval with fixed effects does not
    differ much from the previous one.

**This is because baseline takes on the effect of the street. A
neighborhood might happen to recycle more, which would be seen in the
baseline itsself.**

5.  Perhaps having a cell phone helps explain the level of recycling
    behavior. Instead of “has cell phone,” we find it easier to
    interpret the coefficient if we define the variable " no cell
    phone." Give the R command to define this new variable, which equals
    one minus the “has cell phone” variable in the authors’ data set.
    Use “no cell phone” instead of “has cell phone” in subsequent
    regressions with this dataset.

<!-- end list -->

``` r
d.4$no_phone <- 1 - d.4$havecell
```

6.  Now add “no cell phone” as a covariate to the previous regression.
    Provide a 95% confidence interval for the treatment effect. Explain
    why this confidence interval does not differ much from the previous
    one.

<!-- end list -->

``` r
linReg4f = lm(avg_bins_treat ~ bin +  base_avg_bins_treat + no_phone + factor(street), data = d.4)
get_CONF(linReg4f)[1:5, ]
```

    ## [1] "CI"

    ##                           2.5 %     97.5 %
    ## (Intercept)          0.18088733  0.3943627
    ## bin                  0.07865135  0.1556875
    ## base_avg_bins_treat  0.30514164  0.4285384
    ## no_phone            -0.07953366 -0.0064008
    ## factor(street)3     -0.17741037  0.2720444

**We did not expect it to have an effect on the outcome. Having a cell
phone was a pre treatment measure that was taken before the experiment
was conducted.**

**The 95% confidence interval is \[0.07865135, 0.1556875\]. Blocking
clearly reduces variability.**

7.  Now let’s add in the SMS treatment. Re-run the previous regression
    with “any SMS” included. You should get the same results as in Table
    4A. Provide a 95% confidence interval for the treatment effect of
    the recycling bin. Explain why this confidence interval does not
    differ much from the previous
one.

<!-- end list -->

``` r
linReg4g = lm(avg_bins_treat ~ bin + sms +  base_avg_bins_treat + no_phone + factor(street), data = d.4)
print("95% CI")
```

    ## [1] "95% CI"

``` r
get_CONF(linReg4g)[1:6, ]
```

    ## [1] "CI"

    ##                           2.5 %     97.5 %
    ## (Intercept)          0.16921878 0.38802378
    ## bin                  0.07843089 0.15550473
    ## sms                 -0.03075309 0.06555345
    ## base_avg_bins_treat  0.30545778 0.42870541
    ## no_phone            -0.08047421 0.01362360
    ## factor(street)3     -0.18012392 0.27138357

``` r
print("Coefficients")
```

    ## [1] "Coefficients"

``` r
linReg4g$coefficients[1:5]
```

    ##         (Intercept)                 bin                 sms 
    ##          0.27862128          0.11696781          0.01740018 
    ## base_avg_bins_treat            no_phone 
    ##          0.36708159         -0.03342530

**The 95% interval is \[0.07843089, 0.15550473\]** **SMS doesn’t explain
the variance of the treatment, because sending an SMS when there is a
bin doesn’t do much more.**

8.  Now reproduce the results of column 2 in Table 4B, estimating
    separate treatment effects for the two types of SMS treatments and
    the two types of recycling-bin treatments. Provide a 95% confidence
    interval for the effect of the unadorned recycling bin. Explain how
    your answer differs from that in part (g), and explain why you think
    it
differs.

<!-- end list -->

``` r
linReg4h = lm(avg_bins_treat ~ bin + sms +  base_avg_bins_treat + no_phone + bin_s + bin_g + sms_p + sms_g  + factor(street), data = d.4)
get_CONF(linReg4h)[1:10, ]
```

    ## [1] "CI"

    ##                           2.5 %     97.5 %
    ## (Intercept)          0.17141646 0.38672237
    ## bin                  0.05464380 0.15683344
    ## sms                 -0.02706946 0.08607833
    ## base_avg_bins_treat  0.30573064 0.42914879
    ## no_phone            -0.08029328 0.01397831
    ## bin_s               -0.04133590 0.08758246
    ## sms_p               -0.08367923 0.03798960
    ## factor(street)3     -0.17506298 0.26617244
    ## factor(street)4     -0.19160474 0.12429763
    ## factor(street)5     -0.22040521 0.21431186

``` r
linReg4h$coefficients[1:10]
```

    ##         (Intercept)                 bin                 sms 
    ##          0.27906941          0.10573862          0.02950443 
    ## base_avg_bins_treat            no_phone               bin_s 
    ##          0.36743972         -0.03315749          0.02312328 
    ##               bin_g               sms_p               sms_g 
    ##                  NA         -0.02284481                  NA 
    ##     factor(street)3 
    ##          0.04555473

**The 95% interval is \[0.05464380, 0.15683344\]** **The confidence
interval is smaller than above, meaning that bing/bins and smsp/smsg
accounted for some of the variation, leading to a smaller confidence
interval.**

# 5\. A Final Practice Problem

Now for a fictional scenario. An emergency two-week randomized
controlled trial of the experimental drug ZMapp is conducted to treat
Ebola. (The control represents the usual standard of care for patients
identified with Ebola, while the treatment is the usual standard of care
plus the drug.)

Here are the (fake) data.

``` r
d.5 <- fread("./data/Ebola_rct2.csv")
head(d.5)
```

    ##    temperature_day0 vomiting_day0 treat_zmapp temperature_day14
    ## 1:         99.53168             1           0          98.62634
    ## 2:         97.37372             0           0          98.03251
    ## 3:         97.00747             0           1          97.93340
    ## 4:         99.74761             1           0          98.40457
    ## 5:         99.57559             1           1          99.31678
    ## 6:         98.28889             1           1          99.82623
    ##    vomiting_day14 male
    ## 1:              1    0
    ## 2:              1    0
    ## 3:              0    1
    ## 4:              1    0
    ## 5:              1    0
    ## 6:              1    1

You are asked to analyze it. Patients’ temperature and whether they are
vomiting is recorded on day 0 of the experiment, then ZMapp is
administered to patients in the treatment group on day 1. Vomiting and
temperature is again recorded on day 14.

1.  Without using any covariates, answer this question with regression:
    What is the estimated effect of ZMapp (with standard error in
    parentheses) on whether someone was vomiting on day 14? What is the
    p-value associated with this estimate?

<!-- end list -->

``` r
linReg5a <- lm(vomiting_day14 ~ treat_zmapp, data = d.5)
coeftest(linReg5a, get_VCOC_HC(linReg5a))
```

    ## 
    ## t test of coefficients:
    ## 
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.847458   0.047616  17.798  < 2e-16 ***
    ## treat_zmapp -0.237702   0.091459  -2.599  0.01079 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(linReg5a)
```

    ## 
    ## Call:
    ## lm(formula = vomiting_day14 ~ treat_zmapp, data = d.5)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.84746 -0.03803  0.15254  0.21197  0.39024 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.84746    0.05483  15.456   <2e-16 ***
    ## treat_zmapp -0.23770    0.08563  -2.776   0.0066 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.4212 on 98 degrees of freedom
    ## Multiple R-squared:  0.0729, Adjusted R-squared:  0.06343 
    ## F-statistic: 7.705 on 1 and 98 DF,  p-value: 0.006595

**The estimateed effect of ZMapp is -0.237702 (0.091459), and the p
value is 0.01079 which would make it stat significant at the 5% level.**

2.  Add covariates for vomiting on day 0 and patient temperature on day
    0 to the regression from part (a) and report the ATE (with standard
    error). Also report the
p-value.

<!-- end list -->

``` r
linReg5b <- lm(vomiting_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0, data = d.5)
coeftest(linReg5b, get_VCOC_HC(linReg5b))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                    Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)      -19.469655   7.607812 -2.5592 0.012054 * 
    ## treat_zmapp       -0.165537   0.081976 -2.0193 0.046242 * 
    ## temperature_day0   0.205548   0.078060  2.6332 0.009859 **
    ## vomiting_day0      0.064557   0.178032  0.3626 0.717689   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(linReg5b)
```

    ## 
    ## Call:
    ## lm(formula = vomiting_day14 ~ treat_zmapp + temperature_day0 + 
    ##     vomiting_day0, data = d.5)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.79643 -0.18106  0.04654  0.23122  0.68413 
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)      -19.46966    7.44095  -2.617  0.01032 * 
    ## treat_zmapp       -0.16554    0.07567  -2.188  0.03113 * 
    ## temperature_day0   0.20555    0.07634   2.693  0.00837 **
    ## vomiting_day0      0.06456    0.14635   0.441  0.66013   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3668 on 96 degrees of freedom
    ## Multiple R-squared:  0.311,  Adjusted R-squared:  0.2895 
    ## F-statistic: 14.45 on 3 and 96 DF,  p-value: 7.684e-08

**The ATE is -0.165537 (0.081976), with a p value of 0.046242, which is
significant at the 5% level.**

3.  Do you prefer the estimate of the ATE reported in part (a) or part
    (b)? Why? Report the results of the F-test that you used to form
    this opinion.

**I prefer the value in B, because we see that temperature\_day is a
covariate that explains for some variation, lowering the CI. Also note
that the F value from the summaries above (7.705, 14.447) and R squared
values are much higher in part B.**

4.  The regression from part (b) suggests that temperature is highly
    predictive of vomiting. Also include temperature on day 14 as a
    covariate in the regression from part (b) and report the ATE, the
    standard error, and the
p-value.

<!-- end list -->

``` r
linReg5d <- lm(vomiting_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0 + temperature_day14, data = d.5)
coeftest(linReg5d, get_VCOC_HC(linReg5d))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                     Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)       -22.591585   7.746036 -2.9165 0.004416 **
    ## treat_zmapp        -0.120101   0.085798 -1.3998 0.164829   
    ## temperature_day0    0.176642   0.077024  2.2933 0.024034 * 
    ## vomiting_day0       0.046038   0.173177  0.2658 0.790934   
    ## temperature_day14   0.060148   0.025831  2.3286 0.022002 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(linReg5d)
```

    ## 
    ## Call:
    ## lm(formula = vomiting_day14 ~ treat_zmapp + temperature_day0 + 
    ##     vomiting_day0 + temperature_day14, data = d.5)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.87745 -0.27436  0.04701  0.24801  0.66445 
    ## 
    ## Coefficients:
    ##                    Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)       -22.59159    7.47727  -3.021  0.00323 **
    ## treat_zmapp        -0.12010    0.07768  -1.546  0.12541   
    ## temperature_day0    0.17664    0.07642   2.312  0.02296 * 
    ## vomiting_day0       0.04604    0.14426   0.319  0.75033   
    ## temperature_day14   0.06015    0.02937   2.048  0.04335 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3609 on 95 degrees of freedom
    ## Multiple R-squared:  0.3402, Adjusted R-squared:  0.3124 
    ## F-statistic: 12.24 on 4 and 95 DF,  p-value: 4.545e-08

**The ATE is -0.120101 (0.085798) with a p value of 0.164829. We can see
that temperature\_day14 does help predict the vomitting, and we also see
that treat\_zmapp is not signifcant at the 5% level.**

5.  Do you prefer the estimate of the ATE reported in part (b) or part
    (d)? Why?

**I prefer the estimate in b because using tempature on day 14 could be
a result of the medication/other factors. It could also be affected by
other variables, and thus is not a control that should be used. We see a
bad control, it is an outcome**

6.  Now let’s switch from the outcome of vomiting to the outcome of
    temperature, and use the same regression covariates as in part (b).
    Test the hypothesis that ZMapp is especially likely to reduce mens’
    temperatures, as compared to womens’, and describe how you did so.
    What do the results
suggest?

<!-- end list -->

``` r
linReg5g <- lm(temperature_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0 + male*treat_zmapp, data = d.5)
coeftest(linReg5g, get_VCOC_HC(linReg5g))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                   Estimate Std. Error  t value  Pr(>|t|)    
    ## (Intercept)      48.712690  10.194000   4.7786 6.499e-06 ***
    ## treat_zmapp      -0.230866   0.118272  -1.9520   0.05391 .  
    ## temperature_day0  0.504797   0.104511   4.8301 5.287e-06 ***
    ## vomiting_day0     0.041131   0.194539   0.2114   0.83301    
    ## male              3.085486   0.121773  25.3379 < 2.2e-16 ***
    ## treat_zmapp:male -2.076686   0.198386 -10.4679 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**With a stat of -2.076686(0.198386), there is stat significant evidence
that being a male would reduce temp by 2.08 degrees more than being a
female. I include an interaction term, then proving the result.**

7.  Suspend reality for just a moment – suppose that you had the option
    of being a man or a woman who was a part of this study. Based on
    this data, which sex would you rather be? This time, you need to
    produce evidence (probably from your model estimates) to inform your
    determination. What does your determination depend on?

<!-- end list -->

``` r
male_data <- d.5[d.5$male == 1]
female_data <- d.5[d.5$male == 0]

print("Male")
```

    ## [1] "Male"

``` r
linReg5fmale <- lm(temperature_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0, data = male_data)
coeftest(linReg5fmale, get_VCOC_HC(linReg5fmale))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                  Estimate Std. Error  t value  Pr(>|t|)    
    ## (Intercept)      26.23164   17.55356   1.4944 0.1445787    
    ## treat_zmapp      -2.23910    0.17738 -12.6229 3.517e-14 ***
    ## temperature_day0  0.76699    0.17971   4.2679 0.0001563 ***
    ## vomiting_day0    -0.37907    0.32931  -1.1511 0.2579646    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
print("Female")
```

    ## [1] "Female"

``` r
linReg5ffemale <- lm(temperature_day14 ~ treat_zmapp + temperature_day0 + vomiting_day0, data = female_data)
coeftest(linReg5ffemale, get_VCOC_HC(linReg5ffemale))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)      61.46327   12.77633  4.8107 1.08e-05 ***
    ## treat_zmapp      -0.22908    0.11906 -1.9240  0.05918 .  
    ## temperature_day0  0.37387    0.13095  2.8550  0.00593 ** 
    ## vomiting_day0     0.26549    0.23867  1.1124  0.27048    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
get_CONF(linReg5fmale)
```

    ## [1] "CI"

    ##                       2.5 %     97.5 %
    ## (Intercept)      -9.4813490 61.9446261
    ## treat_zmapp      -2.5999924 -1.8782122
    ## temperature_day0  0.4013649  1.1326223
    ## vomiting_day0    -1.0490573  0.2909192

``` r
get_CONF(linReg5ffemale)
```

    ## [1] "CI"

    ##                       2.5 %       97.5 %
    ## (Intercept)      35.8978915 87.028640435
    ## treat_zmapp      -0.4673268  0.009167394
    ## temperature_day0  0.1118377  0.635910969
    ## vomiting_day0    -0.2120777  0.743059999

**When looking at male v female, I ran 2 regressions, one with only
males and one with only females. We see -2.23910(0.17738) for males and
-0.22908(0.11906) for females, suggesting that when looking at CI, we
get: \[-2.5999924, -1.8782122\] for males and \[-0.4673268,
0.009167394\] for females. It would then tend to suggest that ZMapp
reduces Men’s temp a lot more than females temp. The two p values we get
are 3.517e-14 for males and 0.05918 for females. I would rather be a
man.**

8.  Suppose that you had not run the regression in part (f). Instead,
    you speak with a colleague to learn about heterogeneous treatment
    effects. This colleague has access to a non-anonymized version of
    the same dataset and reports that he had looked at heterogeneous
    effects of the ZMapp treatment by each of 10,000 different
    covariates to examine whether each predicted the effectiveness of
    ZMapp on each of 2,000 different indicators of health, for
    20,000,000 different regressions in total. Across these 20,000,000
    regressions your colleague ran, the treatment’s interaction with
    gender on the outcome of temperature is the only heterogeneous
    treatment effect that he found to be statistically significant. He
    reasons that this shows the importance of gender for understanding
    the effectiveness of the drug, because nothing else seemed to
    indicate why it worked. Bolstering his confidence, after looking at
    the data, he also returned to his medical textbooks and built a
    theory about why ZMapp interacts with processes only present in men
    to cure. Another doctor, unfamiliar with the data, hears his theory
    and finds it plausible. How likely do you think it is ZMapp works
    especially well for curing Ebola in men, and why? (This question is
    conceptual can be answered without performing any computation.)

**We see the multiple comparisons problem in full effect there. The
scientist ran 20 million regressions, and about 5% of his results are
due to chance. Thus, it is possible that the gender covariate is
something he sees because he tested so many different hypothesis. To see
if ZMapp works in curing men, a second experiment should be conducted
where the only covariate analyzed is men. If we again see a stat
significant result, then we can believe the result. It may also make
sense to apply Bonferroni correction to the p value required to produce
a statistically significant outcome. A true fishing expedition.**

1.  Now, imagine that what described in part (g) did not happen, but
    that you had tested this heterogeneous treatment effect, and only
    this heterogeneous treatment effect, of your own accord. Would you
    be more or less inclined to believe that the heterogeneous treatment
    effect really exists? Why?

**I would be more inclined to believe the heterogeneous treatment
effect, if we varied the sex as well as the treatment. Because we are
running just 1 regression, we are sure that the result (if stat
significant), would only happen by chance 5% of the time.**

10. Another colleague proposes that being of African descent causes one
    to be more likely to get Ebola. He asks you what ideal experiment
    would answer this question. What would you tell him? (*Hint: refer
    to Chapter 1 of Mostly Harmless Econometrics.*)

**This question is FUQed - it is a fundamentally unanswerable question.
If there was a more precise question, then perhaphs it could be
answered; however, this one is too vague.**

**What does African Descent mean, and what does it mean to “be more
likely.” Moreover, this experiment on the face would be incredibly hard
to execute because there would be no way to randomize descent, and it
would be tough to follow one’s life tracking for Ebola. There are also a
good amount of covariates that need to be taken into account, and could
easily be ignored. One solution would be to change the African descent
part to spending x amount of years in Africa, or something along those
lines.**

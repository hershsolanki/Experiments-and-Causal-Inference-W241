Problem Set 5
================
Field Experiments

# 1\. Online advertising natural experiment.

These are simulated data (closely, although not entirely) based on a
real example, adopted from Randall Lewis’ dissertation at MIT.

## Problem Setup

Imagine Yahoo\! sells homepage ads to advertisers that are
quasi-randomly assigned by whether the user loads the Yahoo\! homepage
(www.yahoo.com) on an even or odd second of the day. More specifically,
the setup is as follows. On any given week, Monday through Sunday, two
ad campaigns are running on Yahoo\!’s homepage. If a user goes to
www.yahoo.com during an even second that week (e.g., Monday at
12:30:58pm), the ads for the advertiser are shown. But if the user goes
to www.yahoo.com during an odd second during that week (e.g., Monday at
12:30:59), the ads for other products are shown. (If a user logs onto
Yahoo\! once on an even second and once on an odd second, they are shown
the first of the campaigns the first time and the second of the
campaigns the second time. Assignment is not persistent within users.)

This natural experiment allows us to use the users who log onto Yahoo\!
during odd seconds/the ad impressions from odd seconds as a randomized
control group for users who log onto Yahoo\! during even seconds/the ad
impressions from even seconds. (We will assume throughout the problem
there is no effect of viewing advertiser 2’s ads, from odd seconds, on
purchases for advertiser 1, the product advertised on even seconds.)

Imagine you are an advertiser who has purchased advertising from Yahoo\!
that is subject to this randomization on two occasions. Here is a link
to (fake) data on 500,000 randomly selected users who visited Yahoo\!’s
homepage during each of your two advertising campaigns, one you
conducted for product A in March and one you conducted for product B in
August (~250,000 users for each of the two experiments). Each row in the
dataset corresponds to a user exposed to one of these campaigns.

``` r
library(data.table)
library(stargazer)
library(dplyr)
library(multiwayvcov)
library(sandwich)
library(lmtest)

d.1 <- fread('./data/ps5_no1.csv')
head(d.1)
```

    ##    product_b total_ad_exposures_week1 treatment_ad_exposures_week1 week0
    ## 1:         1                        4                            3   5.5
    ## 2:         1                        1                            1   6.2
    ## 3:         1                        3                            1   0.0
    ## 4:         0                        5                            0   0.0
    ## 5:         0                        1                            1   7.6
    ## 6:         1                        4                            4   6.3
    ##    week1 week2 week3 week4 week5 week6 week7 week8 week9 week10
    ## 1:   6.2   0.0   0.0   0.0   0.0   0.0     0   9.7   4.1    0.0
    ## 2:   0.0   8.6   2.4   0.0   7.4   0.0     0   0.0   5.7    0.0
    ## 3:   5.3   0.0   8.1   7.8   3.3   0.0     0   9.4   0.0    0.0
    ## 4:   4.1   0.0   8.8   5.8   5.9   0.0     0   0.0   9.6    0.0
    ## 5:   3.6   4.6   5.5   7.2   7.1   0.0     0   0.0   0.0    0.0
    ## 6:   5.5   9.8   5.0   0.0   0.0   7.7     0  11.0   4.8    6.9

The variables in the dataset are described below:

  - **product\_b**: an indicator for whether the data is from your
    campaign for product A (in which case it is set to 0), sold
    beginning on March 1, or for product B, sold beginning on August 1
    (in which case it is set to 1). That is, there are two experiments
    in this dataset, and this variable tells you which experiment the
    data belong to.
  - **treatment\_ad\_exposures\_week1**: number of ad exposures for the
    product being advertised during the campaign. (One can also think of
    this variable as “number of times each user visited Yahoo\! homepage
    on an even second during the week of the campaign.”)
  - **total\_ad\_exposures\_week1**: number of ad exposures on the
    Yahoo\! homepage each user had during the ad campaign, which is the
    sum of exposures to the “treatment ads” for the product being
    advertised (delivered on even seconds) and exposures to the “control
    ads” for unrelated products (delivered on odd seconds). (One can
    also think of this variable as “total number of times each user
    visited the Yahoo\! homepage during the week of the campaign.”)
  - **week0**: For the treatment product, the revenues from each user in
    the week prior to the launch of the advertising campaign.
  - **week1**: For the treatment product, the revenues from each user in
    the week during the advertising campaign. The ad campaign ends on
    the last day of week 1.
  - **week2-week10**: Revenue from each user for the treatment product
    sold in the weeks subsequent to the campaign. The ad campaign was
    not active during this time.

Simplifying assumptions you should make when answering this problem:

  - The effect of treatment ad exposures on purchases is linear. That
    is, the first exposure has the same effect as the second exposure.
  - There is no effect of being exposed to the odd-second ads on
    purchases for the product being advertised on the even second.
  - Every Yahoo\! user visits the Yahoo\! home page at most six times a
    week.
  - You can assume that treatment ad exposures do not cause changes in
    future ad exposures. That is, assume that getting a treatment ad at
    9:00am doesn’t cause you to be more (or less) likely to visit the
    Yahoo home pages on an even second that afternoon, or on subsequent
    days.

## Questions to Answer

1.  Run a crosstab (`table`) of `total_ad_exposures_week1` and
    `treatment_ad_exposures_week1` to sanity check that the distribution
    of impressions looks as it should. Does it seem reasonable? Why does
    it look like this? (No computation required here, just a brief
    verbal response.)

<!-- end list -->

``` r
table(d.1$total_ad_exposures_week1, d.1$treatment_ad_exposures_week1)
```

    ##    
    ##         0     1     2     3     4     5     6
    ##   0 61182     0     0     0     0     0     0
    ##   1 36754 37215     0     0     0     0     0
    ##   2 21143 42036 20965     0     0     0     0
    ##   3 10683 32073 32314 10726     0     0     0
    ##   4  5044 20003 30432 20223  5115     0     0
    ##   5  2045 10563 20970 20793 10293  2131     0
    ##   6   729  4437 10977 14771 11147  4486   750

``` r
hist(d.1$total_ad_exposures_week1)
```

![](ps5_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
hist(d.1$treatment_ad_exposures_week1)
```

![](ps5_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
d.1$sanity_check <- d.1$total_ad_exposures_week1 < d.1$treatment_ad_exposures_week1
any(d.1$sanity_check == TRUE)
```

    ## [1] FALSE

**When looking at the table, we know that the
total\_ad\_exposures\_week1 must be greater than the
treatment\_ad\_exposures\_week1. Thus, half the cross tab is empty,
which makes sure that our data is correct. If it was not empty, there
could be cases where treatment\_ad\_exposures\_week1 \>
total\_ad\_exposures\_week1, which would mean this data does NOT pass
the sanity check. Moreover, treatment\_ad\_exposures\_week1 is right
skewed while total\_ad\_exposures\_week1 is more normally distributed.
Once again, this is that way the two histograms should look. Finally, I
run “any(d.1$sanity\_check == TRUE)” to see if any of the rows have the
case that reatment\_ad\_exposures\_week1 \> total\_ad\_exposures\_week1.
However, this is FALSE, finally proving the hypothesis.**

2.  Your colleague proposes the code printed below to analyze this
    experiment: `lm(week1 ~ treatment_ad_exposures_week1, data)` You are
    suspicious. Run a placebo test with the prior week’s purchases as
    the outcome and report the results. Did the placebo test “succeed”
    or “fail”? Why do you say so?

<!-- end list -->

``` r
lin.reg.1b.2 <- lm(week0 ~ treatment_ad_exposures_week1, data = d.1)
summary(lin.reg.1b.2)
```

    ## 
    ## Call:
    ## lm(formula = week0 ~ treatment_ad_exposures_week1, data = d.1)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.248 -2.196 -1.670  2.430  8.330 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  1.669685   0.006027   277.0   <2e-16 ***
    ## treatment_ad_exposures_week1 0.263099   0.003155    83.4   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.796 on 499998 degrees of freedom
    ## Multiple R-squared:  0.01372,    Adjusted R-squared:  0.01372 
    ## F-statistic:  6955 on 1 and 499998 DF,  p-value: < 2.2e-16

``` r
#Robust SE
(sqrt(diag(vcovHC(lin.reg.1b.2))))
```

    ##                  (Intercept) treatment_ad_exposures_week1 
    ##                  0.005448638                  0.003354527

**The placebo tests fails, as there is a stat significant effect of
having the treatment predict the previous week’s revenues. There is an
impact on the outcome when in reality there should not be one.**

3.  The placebo test suggests that there is something wrong with our
    experiment or our data analysis. We suggest looking for a problem
    with the data analysis. Do you see something that might be spoiling
    the randomness of the treatment variable? How can you improve your
    analysis to get rid of this problem? Why does the placebo test turn
    out the way it does? What one thing needs to be done to analyze the
    data correctly? Please provide a brief explanation of why, not just
    what needs to be done. (*Note: This question, and verifying that you
    answered it correctly in part d below, may require some thinking. If
    we find many people can’t figure it out, we will post another hint
    in a few days.*)

**One way we can improve is to add total exposures as a covariate, so we
can delineate the specific ad views vs the total ad views. It could be
the case that certain people buy a lot of things, and then are on the
site a lot. However, this does not mean that A/B influenced them. By
adding it as a covariate, we can see the true effect of the
treatment\_ad\_exposures\_week1.**

4.  Implement the procedure you propose from part (c), run the placebo
    test for the Week 0 data again, and report the results. (This
    placebo test should pass; if it does not, re-evaluate your strategy
    before wasting time
proceeding.)

<!-- end list -->

``` r
lin.reg.1d <- lm(week0 ~ treatment_ad_exposures_week1 + total_ad_exposures_week1, data = d.1)
summary(lin.reg.1d)
```

    ## 
    ## Call:
    ## lm(formula = week0 ~ treatment_ad_exposures_week1 + total_ad_exposures_week1, 
    ##     data = d.1)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -2.817 -2.079 -1.589  2.455  7.823 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   1.345375   0.007295 184.436   <2e-16 ***
    ## treatment_ad_exposures_week1 -0.002245   0.004629  -0.485    0.628    
    ## total_ad_exposures_week1      0.245348   0.003149  77.922   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.779 on 499997 degrees of freedom
    ## Multiple R-squared:  0.02555,    Adjusted R-squared:  0.02555 
    ## F-statistic:  6556 on 2 and 499997 DF,  p-value: < 2.2e-16

``` r
#Robust SE
(sqrt(diag(vcovHC(lin.reg.1d))))
```

    ##                  (Intercept) treatment_ad_exposures_week1 
    ##                  0.006133211                  0.005138081 
    ##     total_ad_exposures_week1 
    ##                  0.003372207

**Placebo does not have an effect on prior week’s revenue, as we see a p
value of 0.628. Thus, I know I have done my calculation correctly.**

5.  Now estimate the causal effect of each ad exposure on purchases
    during the week of the campaign itself using the same technique that
    passed the placebo test in part
(d).

<!-- end list -->

``` r
lin.reg.1e <- lm(week1 ~ treatment_ad_exposures_week1 + total_ad_exposures_week1, data = d.1)
summary(lin.reg.1e)
```

    ## 
    ## Call:
    ## lm(formula = week1 ~ treatment_ad_exposures_week1 + total_ad_exposures_week1, 
    ##     data = d.1)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.003 -2.104 -1.542  2.447  8.110 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  1.317960   0.007263  181.47   <2e-16 ***
    ## treatment_ad_exposures_week1 0.056340   0.004609   12.22   <2e-16 ***
    ## total_ad_exposures_week1     0.224478   0.003135   71.61   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.767 on 499997 degrees of freedom
    ## Multiple R-squared:  0.02782,    Adjusted R-squared:  0.02781 
    ## F-statistic:  7153 on 2 and 499997 DF,  p-value: < 2.2e-16

``` r
#Robust SE
(sqrt(diag(vcovHC(lin.reg.1e))))
```

    ##                  (Intercept) treatment_ad_exposures_week1 
    ##                  0.006056016                  0.005137690 
    ##     total_ad_exposures_week1 
    ##                  0.003317352

**We see an increase of 0.056340(0.005137690) in revenue for each add
the user is exposed to, which we know is statistically significant.
Thus, the first week of sales reflect the impact of the ads.**

6.  The colleague who proposed the specification in part (b) challenges
    your results – they make the campaign look less successful. Write a
    paragraph that a layperson would understand about why your
    estimation strategy is superior and his/hers is biased.

**To the layperson, you would say: You need to include a baseline
because you need to see how many sales existed before the ads took
effect. Moreover, a website could have a lot of visits, which lead to
purchases. What we need to see is if visiting a web page on an even/odd
second had an effect on purchases GIVEN the fact that we know how many
total times a person visited a website. We need to make sure we can
attribute the sale to ad, and not just a person who likes to visit a lot
of times (might buy for anothe reason). Our colleague’s strategy may be
biased for exactly this reason–a person who visits a lot and was not
affect by the ad would be shown to be effected by the ad.**

7.  Estimate the causal effect of each treatment ad exposure on
    purchases during and after the campaign, up until week 10 (so, total
    purchases during weeks 1 through
10).

<!-- end list -->

``` r
d.1$total <- d.1$week1 + d.1$week2 + d.1$week3 + d.1$week4 + d.1$week5 + d.1$week6 + d.1$week7 + d.1$week8 + d.1$week9 + d.1$week10

lin.reg.1g <- lm(total ~ treatment_ad_exposures_week1 + total_ad_exposures_week1, data = d.1)
summary(lin.reg.1g)
```

    ## 
    ## Call:
    ## lm(formula = total ~ treatment_ad_exposures_week1 + total_ad_exposures_week1, 
    ##     data = d.1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -30.597  -7.372  -0.731   6.654  59.782 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  17.15081    0.02771 618.949   <2e-16 ***
    ## treatment_ad_exposures_week1  0.01274    0.01758   0.724    0.469    
    ## total_ad_exposures_week1      2.22834    0.01196 186.307   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.56 on 499997 degrees of freedom
    ## Multiple R-squared:  0.1321, Adjusted R-squared:  0.1321 
    ## F-statistic: 3.804e+04 on 2 and 499997 DF,  p-value: < 2.2e-16

``` r
#Robust SE
(sqrt(diag(vcovHC(lin.reg.1g))))
```

    ##                  (Intercept) treatment_ad_exposures_week1 
    ##                   0.02446408                   0.01902183 
    ##     total_ad_exposures_week1 
    ##                   0.01253656

**There is an impact of 0.01274(0.01902183) for the 10 weeks combined.
This value is not stat significant, meaning the lift over 9 weeks was
not significantly affected by the ads.**

8.  Estimate the causal effect of each treatment ad exposure on
    purchases only after the campaign. That is, look at total purchases
    only during week 2 through week 10,
inclusive.

<!-- end list -->

``` r
d.1$total_2_10 <- d.1$week2 + d.1$week3 + d.1$week4 + d.1$week5 + d.1$week6 + d.1$week7 + d.1$week8 + d.1$week9 + d.1$week10

lin.reg.1h <- lm(total_2_10 ~ treatment_ad_exposures_week1 + total_ad_exposures_week1, data = d.1)
summary(lin.reg.1h)
```

    ## 
    ## Call:
    ## lm(formula = total_2_10 ~ treatment_ad_exposures_week1 + total_ad_exposures_week1, 
    ##     data = d.1)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -27.856  -7.097  -0.697   6.382  54.079 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  15.83285    0.02654 596.493  < 2e-16 ***
    ## treatment_ad_exposures_week1 -0.04360    0.01684  -2.588  0.00964 ** 
    ## total_ad_exposures_week1      2.00387    0.01146 174.901  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.11 on 499997 degrees of freedom
    ## Multiple R-squared:  0.1154, Adjusted R-squared:  0.1154 
    ## F-statistic: 3.261e+04 on 2 and 499997 DF,  p-value: < 2.2e-16

``` r
#Robust SE
(sqrt(diag(vcovHC(lin.reg.1h))))
```

    ##                  (Intercept) treatment_ad_exposures_week1 
    ##                   0.02350315                   0.01818730 
    ##     total_ad_exposures_week1 
    ##                   0.01201531

**There was a -0.04360(0.01818730) impact of each ad that was exposed,
which is stat significant. What we see is that sales went down when
there was an ad shown.**

1.  Tell a story that could plausibly explain the result from part (h).

**People buy all their stuff when there is an ad, then you have a
negative effect afterwards because they no longer want to buy anything.
We can prove this because the result on week 1 sales is a positive lift,
but week 2-10 sales is a negative lift.**

10. Test the hypothesis that the ads for product B are more effective,
    in terms of producing additional revenue in week 1 only, than are
    the ads for product A. (*Hint: The easiest way to do this is to
    throw all of the observations into one big regression and specify
    that regression in such a way that it tests this hypothesis.*)
    (*Hint 2: There are a couple defensible ways to answer this question
    that lead to different answers. Don’t stress if you think you have
    an approach you can
defend.*)

<!-- end list -->

``` r
lin.reg.1j <- lm(week1 ~ treatment_ad_exposures_week1 * product_b + total_ad_exposures_week1 * product_b, data = d.1)
summary(lin.reg.1j)
```

    ## 
    ## Call:
    ## lm(formula = week1 ~ treatment_ad_exposures_week1 * product_b + 
    ##     total_ad_exposures_week1 * product_b, data = d.1)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -3.021 -2.183 -1.500  2.437  8.068 
    ## 
    ## Coefficients:
    ##                                         Estimate Std. Error t value
    ## (Intercept)                             1.295811   0.008587 150.902
    ## treatment_ad_exposures_week1            0.068154   0.006515  10.461
    ## product_b                               0.149082   0.016579   8.992
    ## total_ad_exposures_week1                0.204690   0.004357  46.981
    ## treatment_ad_exposures_week1:product_b -0.023922   0.009215  -2.596
    ## product_b:total_ad_exposures_week1      0.013720   0.006493   2.113
    ##                                        Pr(>|t|)    
    ## (Intercept)                             < 2e-16 ***
    ## treatment_ad_exposures_week1            < 2e-16 ***
    ## product_b                               < 2e-16 ***
    ## total_ad_exposures_week1                < 2e-16 ***
    ## treatment_ad_exposures_week1:product_b  0.00943 ** 
    ## product_b:total_ad_exposures_week1      0.03459 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.766 on 499994 degrees of freedom
    ## Multiple R-squared:  0.02848,    Adjusted R-squared:  0.02847 
    ## F-statistic:  2932 on 5 and 499994 DF,  p-value: < 2.2e-16

``` r
(sqrt(diag(vcovHC(lin.reg.1j))))
```

    ##                            (Intercept) 
    ##                            0.006668751 
    ##           treatment_ad_exposures_week1 
    ##                            0.006776312 
    ##                              product_b 
    ##                            0.014975072 
    ##               total_ad_exposures_week1 
    ##                            0.004300857 
    ## treatment_ad_exposures_week1:product_b 
    ##                            0.010272853 
    ##     product_b:total_ad_exposures_week1 
    ##                            0.006894627

**We add an interaction term, which is product\_b. Ultimately, we see
that ads for product B are less effective than ads for product A. We
look at treatment\_ad\_exposures\_week1:product\_b to get this result,
with an estimate of -0.023922(0.010272853) and p value that is stat
significant.**

11. You notice that the ads for product A included celebrity
    endorsements. How confident would you be in concluding that
    celebrity endorsements increase the effectiveness of advertising at
    stimulating immediate purchases?

**How can you conclude this? You have no data to obtain causation. I
would not be confident at all, and would have to run an experiment to
come a conclusion like this. Just because you notice something does not
mean there is a causal effect. Remember to be safe, you always want to
run an experiment when you can to establish causality\!**

# 2\. Vietnam Draft Lottery

A [famous
paper](http://sites.duke.edu/niou/files/2011/06/Angrist_lifetime-earningsmall.pdf)
by Angrist exploits the randomized lottery for the Vietnam draft to
estimate the effect of education on wages. (*Don’t worry about reading
this article, it is just provided to satisfy your curiosity; you can
answer the question below without referring to it. In fact, it may be
easier for you not to, since he has some complications to deal with that
the simple data we’re giving you do not.*)

## Problem Setup

Angrist’s idea is this: During the Vietnam era, draft numbers were
determined randomly by birth date – the army would literally randomly
draw birthdays out of a hat, and those whose birthdays came up sooner
were higher up on the list to be drafted first. For example, all young
American men born on May 2 of a given year might have draft number 1 and
be the first to be called up for service, followed by November 13 who
would get draft number 2 and be second, etc. The higher-ranked (closer
to 1) your draft number, the likelier it was you would be drafted.

We have generated a fake version of this data for your use in this
project. You can find real information
(here)\[<https://www.sss.gov/About/History-And-Records/lotter1>\]. While
we’re defining having a high draft number as falling at 80, in reality
in 1970 any number lower than 195 would have been a “high” draft number,
in 1971 anything lower than 125 would have been “high”.

High draft rank induced many Americans to go to college, because being a
college student was an excuse to avoid the draft – so those with
higher-ranked draft numbers attempted to enroll in college for fear of
being drafted, whereas those with lower-ranked draft numbers felt less
pressure to enroll in college just to avoid the draft (some still
attended college regardless, of course). Draft numbers therefore cause a
natural experiment in education, as we now have two randomly assigned
groups, with one group having higher mean levels of education, those
with higher draft numbers, than another, those with lower draft numbers.
(In the language of econometricians, we say the draft number is “an
instrument for education,” or that draft number is an “instrumental
variable.”)

Some simplifying assumptions:

  - Suppose that these data are a true random sample of IRS records and
    that these records measure every living American’s income without
    error.
  - Assume that the true effect of education on income is linear in the
    number of years of education obtained.
  - Assume all the data points are from Americans born in a single year
    and we do not need to worry about cohort effects of any kind.

<!-- end list -->

    ##    draft_number years_education    income
    ## 1:          267              16  44573.90
    ## 2:          357              13  10611.75
    ## 3:          351              19 165467.80
    ## 4:          205              16  71278.40
    ## 5:           42              19  54445.09
    ## 6:          240              11  32059.12

## Questions to Answer

1.  Suppose that you had not run an experiment. Estimate the “effect” of
    each year of education on income as an observational researcher
    might, by just running a regression of years of education on income
    (in R-ish, `income ~ years_education`). What does this naive
    regression suggest?

<!-- end list -->

    ## 
    ## Call:
    ## lm(formula = income ~ years_education, data = d.2)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -91655 -17459   -837  16346 141587 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -23354.64    1252.74  -18.64   <2e-16 ***
    ## years_education   5750.48      83.34   69.00   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26590 on 19565 degrees of freedom
    ## Multiple R-squared:  0.1957, Adjusted R-squared:  0.1957 
    ## F-statistic:  4761 on 1 and 19565 DF,  p-value: < 2.2e-16

    ##     (Intercept) years_education 
    ##      1197.22617        84.41089

**Every year of added education adds $5750.48 (84.41) of an increase in
income. We know this is stat significant, we our p value being 2e-16.
Here I used Robust SE.**

2.  Continue to suppose that we did not run the experiment, but that we
    saw the result that you noted in part (a). Tell a concrete story
    about why you don’t believe that observational result tells you
    anything causal.

**It could be the case that education year is not the only thing that
leads to wealth. If one were to have family connections that could set
them up for success, then education is irrelevant. If they inherited a
business, once again, education doesn’t matter as much. Another reason
could be that if you went to school (which requires you to be
succesful/motivated), then you could also be succesful in your career.**

3.  Now, let’s get to using the natural experiment. We will define
    “having a high-ranked draft number” as having a draft number of 80
    or below (1-80; numbers 81-365, for the remaining 285 days of the
    year, can be considered “low-ranked”). Create a variable in your
    dataset indicating whether each person has a high-ranked draft
    number or not. Using regression, estimate the effect of having a
    high-ranked draft number, the dummy variable you’ve just created, on
    years of education obtained. Report the estimate and a correctly
    computed standard error. (\*Hint: Pay special attention to
    calculating the correct standard errors here. They should match how
    the draft is conducted.)

<!-- end list -->

    ## 
    ## Call:
    ## lm(formula = years_education ~ rank, data = d.2)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.5601 -1.4343 -0.4343  1.5657  5.5657 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 14.43431    0.01691  853.40   <2e-16 ***
    ## rank         2.12576    0.03790   56.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.117 on 19565 degrees of freedom
    ## Multiple R-squared:  0.1385, Adjusted R-squared:  0.1384 
    ## F-statistic:  3145 on 1 and 19565 DF,  p-value: < 2.2e-16

    ## (Intercept)        rank 
    ##  0.01770332  0.03818784

**The impact of having a high draft number is 2.12576 more years of
education. Given we have clusters (high vs low rank), we use a clustered
SE, which comes out to be 0.03818784. This value is stat significant.**

4.  Using linear regression, estimate the effect of having a high-ranked
    draft number on income. Report the estimate and the correct standard
    error.

<!-- end list -->

    ## 
    ## Call:
    ## lm(formula = income ~ rank, data = d.2)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -67399 -21140  -3002  18005 151306 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  60761.9      235.9  257.56   <2e-16 ***
    ## rank          6637.6      528.7   12.55   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 29530 on 19565 degrees of freedom
    ## Multiple R-squared:  0.007992,   Adjusted R-squared:  0.007941 
    ## F-statistic: 157.6 on 1 and 19565 DF,  p-value: < 2.2e-16

    ## (Intercept)        rank 
    ##    244.3615    511.8992

**The effect of having a a high draft number on income is $6,637.6 in
increased income. Once again we use clustered SE, to get 511.8992. Note
this value is stat significant.**

5.  Divide the estimate from part (d) by the estimate in part (c) to
    estimate the effect of education on income. This is an
    instrumental-variables estimate, in which we are looking at the
    “clean” variation in both education and income that is due to the
    draft status, and computing the slope of the income-education line
    as “clean change in Y” divided by “clean change in X”. What do the
    results suggest?

<!-- end list -->

    ## (Intercept)        rank 
    ##    4209.547    3122.444

**Here, we see our clean variation. In this case, it is the case that a
high rank draft number leads to an increase income of $3,122.44 per
year.**

6.  Natural experiments rely crucially on the “exclusion restriction”
    assumption that the instrument (here, having a high draft rank)
    cannot affect the outcome (here, income) in any other way except
    through its effect on the “endogenous variable” (here, education).
    Give one reason this assumption may be violated – that is, why
    having a high draft rank could affect individuals’ income other than
    because it nudges them to attend school for longer.

**Given one has a higher rank, they could go to war and get injured,
thus being unable to work. As a result, it is the lack of physical
ability, not the lack of schooling, that affect their income negatively.
Some could have PTSD as well, which is another reason the assumption can
be violated.**

7.  Conduct a test for the presence of differential attrition by
    treatment condition. That is, conduct a formal test of the
    hypothesis that the “high-ranked draft number” treatment has no
    effect on whether we observe a person’s income. **(Note, that an
    earning of $0 *actually* means they didn’t earn any money.)**

<!-- end list -->

    ## 
    ## Call:
    ## lm(formula = count ~ rank, data = grouped.d2.count)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -16.986  -4.986   0.014   5.014  24.300 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  54.9860     0.4346 126.532  < 2e-16 ***
    ## rank         -6.2860     0.9282  -6.772 5.13e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.336 on 363 degrees of freedom
    ## Multiple R-squared:  0.1122, Adjusted R-squared:  0.1097 
    ## F-statistic: 45.86 on 1 and 363 DF,  p-value: 5.128e-11

    ## (Intercept)        rank 
    ##   0.4314906   0.9448176

**There are -6.2860 observations for high rank numbers in comparison to
lower rank numbers, which is stat significant. The clusetered SE is
0.9448176. Thus, we reject the hypothesis that high rank has no effect
on if we can observe income.**

8.  Tell a concrete story about what could be leading to the result in
    part (g).

**Those who died cannot have income numbers, and those who had highest
lottery numbers were more likely to be drafted. Thus, we see lower
amounts of observations for higher draft numbers. Thus, they are unable
to get an income, and so the study favors though who did not die.**

1.  Tell a concrete story about how this differential attrition might
    bias our estimates.

**If someone did not die, it could be the case that they were
resilisent, which could translate to working as well. As a result, these
people who are resilient will end up making more money, and will not be
an accurate representation of everyone who had a higher draft number.
Thus, our estimate would be higher than the actual reality, leading to
an overestimation of income of those with a higher draft lottery.**

# 3\. Optional: Think about Treatment Effects

Throughout this course we have focused on the average treatment effect.
Think back to *why* we are concerned about the average treatment effect.
What is the relationship between an ATE, and some individuals’ potential
outcomes? Make the strongest case you can for why this is a *good*
measure.

**We care about the average treatment effect because it tells us on
average, the effect a treatment has on a group of individuals. A
potential outcome deals with looking at counterfactuals–if you are in
the control group, what would have your outcome been had you been in the
treatment group. Thus, the ATE is the average difference in potential
outcomes for the group of individuals. This is a good measure because we
are aggregating potential outcomes, and finding the average difference
between the control and treatment group. This tells us how powerful the
treatment is.**

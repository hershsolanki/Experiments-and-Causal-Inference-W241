Problem Set 2
================
Experiments and Causality

# 1\. What happens when pilgrims attend the Hajj pilgrimage to Mecca?

On the one hand, participating in a common task with a diverse group of
pilgrims might lead to increased mutual regard through processes
identified in *Contact Theories*. On the other hand, media narritives
have raised the spectre that this might be accompanied by “antipathy
toward non-Muslims”. [Clingingsmith, Khwaja and Kremer
(2009)](https://dash.harvard.edu/handle/1/3659699) investigates the
question.

Using the data here, test the sharp null hypothesis that winning the
visa lottery for the pilgrimage to Mecca had no effect on the views of
Pakistani Muslims toward people from other countries. Assume that the
Pakistani authorities assigned visas using complete random assignment.
Use, as your primary outcome the `views` variable, and as your treatment
feature `success`. If you’re ambitious, write your fucntion generally so
that you can also evaluate feeligns toward specific nationalities.

``` r
d <- fread("./data/clingingsmith_2009.csv")
head(d)
```

    ##    success views_saudi views_indonesian views_turkish views_african
    ## 1:       0           1                1             0             0
    ## 2:       0           1                1             0            -1
    ## 3:       0           0                0             0             0
    ## 4:       0           2                2             0             0
    ## 5:       0           1                1             1             1
    ## 6:       0           2                0             0             0
    ##    views_chinese views_european views
    ## 1:             0              0     2
    ## 2:             1             -1     1
    ## 3:             0              0     0
    ## 4:             1              0     5
    ## 5:             1             -2     3
    ## 6:             0              0     2

1.  Using either `dplyr` or `data.table`, group the data by `success`
    and report whether views toward others are generally more positive
    among lottery winners or lottery non-winners.

<!-- end list -->

``` r
grouped <- group_by(d, success)
summarise(grouped, mean(views))
```

    ## # A tibble: 2 x 2
    ##   success `mean(views)`
    ##     <int>         <dbl>
    ## 1       0          1.87
    ## 2       1          2.34

**ATE is 0.478**

2.  But is this a meaningful difference, or could it just be
    randomization noise? Conduct 10,000 simulated random assignments
    under the sharp null hypothesis to find out. (Don’t just copy the
    code from the async, think about how to write this yourself.)

<!-- end list -->

``` r
control <- d[d$success == 0]
treatment <- d[d$success == 1]

# This is the treatment function, will be referred to many times.
afterTreatment <- function(a, b) {
  treament = a[b==1]
  control = a[b==0]
  return(mean(treament) - mean(control))
  
}

# Get the ATE we are looking for
ate <- afterTreatment(grouped$views, grouped$success) 
viewsVector <- d$views

# See how many 0s and 1s there are
random <- function() {
  sample(c(rep(0, 448), rep(1, 510)))
}


N <- 10000
simulated.trial <- replicate(N, afterTreatment(viewsVector, random()))
simulated.trial.mean <- mean(simulated.trial)

# Show results
hist(simulated.trial, main = "Histogram of Results", breaks = 100)
abline(v = ate)
```

![](ps2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

3.  How many of the simulated random assignments generate an estimated
    ATE that is at least as large as the actual estimate of the ATE?

<!-- end list -->

``` r
num_larger <- simulated.trial >= ate
number.simulated.assingments <- sum(num_larger)
number.simulated.assingments
```

    ## [1] 20

**There are 20**

4.  What is the implied *one-tailed* p-value?

<!-- end list -->

``` r
p_value_one_tailed <- mean(num_larger)
p_value_one_tailed
```

    ## [1] 0.002

**The one tailed p value is 0.002**

5.  How many of the simulated random assignments generate an estimated
    ATE that is at least as large *in absolute value* as the actual
    estimate of the ATE?

<!-- end list -->

``` r
number_more_extreme <- abs(simulated.trial) >= abs(ate)
number_more_extreme.assignments <- sum(number_more_extreme)
number_more_extreme.assignments
```

    ## [1] 35

**There are 42**

6.  What is the implied two-tailed p-value?

<!-- end list -->

``` r
p_value_two_tailed <- mean(number_more_extreme) 
p_value_two_tailed
```

    ## [1] 0.0035

**The implied 2 tail p value is 0.0042**

# 2\. Randomization Inference Practice

McElhoe and Conner (1986) suggest using a *new* instrument called a
“Visiplume” measure pollution. The EPA has a standard method for
measuring pollution. Because they’re good scientists, McElhoe and Conner
want to validate that their instrument is measuring the same levels of
pollution as the EPA instrument.

To do so, they take six readings – one with each instrument – at a
single site. The recorded response is the ratio of the Visiplume reading
to the EPA standard reading, and the values that are recorded are:
0.950, 0.978, 0.762, 0.733, 0.823, and 1.011.

Suppose that we want to test the question, “Do the Visiplume readings
and the EPA standard readings produce similar enough estimates?”

> (The point of this question is to demonstrate that randomization
> inference works as a general inferrential paradigm, without
> *necessairily* being tied to an experiment.)

1.  How would you structure the sharp-null hypothesis – that Visiplume
    and the EPA reaings are the same – in this case?

**There should be no difference; thus, the numbers should be 1 all
across. A ratio of 1 means that Visiplume and EPA are the same, thus the
sharp null hypotheis.**

2.  Suppose that our summary of the data is the sum of the ratios. That
    is, in the test that we conducted, we obsered
    \(0.95 + ... + 1.011 = 5.257\). Using randomization inference, test
    the sharp-null hypothesis that you formed in the first part of the
    question. Produce a histogram of the test statistic under the sharp
    null that compares against the 5.257 value from the test, and also
    produce a two-sided p-value.

<!-- end list -->

``` r
input <- c(.950, .978, .762, .733, .823, 1.011) 
sumInput <- sum(input)

# We have the inverse list because this made of ratios
inverse <- 1/input
sumInverse <- sum(inverse)

# Take the total set which is input and inverse
total <- c(input, inverse)

# Pick 6 out of 12 values without reaplcement
random.sample <- function() {
  random <- sample(c(total), 6, replace = FALSE)
  return(sum(random))
}

N <- 1000000

trials <- replicate(N, random.sample())

# Show histogram and two vertical lines
hist(trials, breaks = N/10000)
abline(v=sumInput, col = "blue")
abline(v=sumInverse, col = "red")
```

![](ps2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
p_value <- mean(sumInput >= trials)
p_value
```

    ## [1] 0.002081

**The p value is 0.002118**

# 3\. Term Limits Aren’t Good.

Naturally occurring experiments sometimes involve what is, in effect,
block random assignment. For example, [Rocio
Titiunik](https://sites.google.com/a/umich.edu/titiunik/publications) ,
in [this
paper](http://www-personal.umich.edu/~titiunik/papers/Titiunik2016-PSRM.pdf)
studies the effect of lotteries that determine whether state senators in
TX and AR serve two-year or four-year terms in the aftermath of
decennial redistricting. These lotteries are conducted within each
state, and so there are effectively two distinct experiments on the
effects of term length.

The “thoery” in the news (such as it is), is that legislators who serve
4 year terms have more time to slack off and not produce legislation. If
this were true, then it would stand to reason that making terms shorter
would increase legislative production.

One way to measure legislative production is to count the number of
bills (legislative proposals) that each senator introduces during a
legislative session. The table below lists the number of bills
introduced by senators in both states during 2003.

``` r
library(foreign)

data <- read.dta("./data/titiunik_2010.dta")
data <- data.table(data)
head(data)
```

    ##    term2year bills_introduced texas0_arkansas1
    ## 1:         0               18                0
    ## 2:         0               29                0
    ## 3:         0               41                0
    ## 4:         0               53                0
    ## 5:         0               60                0
    ## 6:         0               67                0

1.  Using either `dplyr` or `data.table`, group the data by state and
    report the mean number of bills introduced in each state. Does Texas
    or Arkansas seem to be more productive? Then, group by two- or
    four-year terms (ignoring states). Do two- or four-year terms seem
    to be more productive? **Which of these effects is causal, and which
    is not?** Finally, using `dplyr` or `data.table` to group by state
    and term-length. How, if at all, does this change what you learn?

<!-- end list -->

``` r
summarize(group_by(data, texas0_arkansas1), mean(bills_introduced))
```

    ## # A tibble: 2 x 2
    ##   texas0_arkansas1 `mean(bills_introduced)`
    ##              <int>                    <dbl>
    ## 1                0                     68.8
    ## 2                1                     25.5

**Texas clearly seems more productive, with an average of 68.77 bills
produced, vs 25.51. There might be causality here.**

``` r
summarize(group_by(data, term2year), mean(bills_introduced))
```

    ## # A tibble: 2 x 2
    ##   term2year `mean(bills_introduced)`
    ##       <int>                    <dbl>
    ## 1         0                     53.1
    ## 2         1                     38.6

**4 year seems to be way more productive than 2 years, with 4 year being
53.09 vs 2 year being 38.58. There might be causality
here.**

``` r
summarize(group_by(data, term2year, texas0_arkansas1), mean(bills_introduced))
```

    ## # A tibble: 4 x 3
    ## # Groups:   term2year [2]
    ##   term2year texas0_arkansas1 `mean(bills_introduced)`
    ##       <int>            <int>                    <dbl>
    ## 1         0                0                     76.9
    ## 2         0                1                     30.7
    ## 3         1                0                     60.1
    ## 4         1                1                     20.6

**It seems that the values when we are in Arkansas are lower than when
we are in Texas. We should look to investigate into this.**

2.  For each state, estimate the standard error of the estimated ATE.

<!-- end list -->

``` r
# Get the variances && m/m
var.0.texas <- var((data[data$texas0_arkansas1 == 0 & data$term2year == 0]$bills_introduced))
var.1.texas <- var((data[data$texas0_arkansas1 == 0 & data$term2year == 1]$bills_introduced))
N.texas <- nrow(data[data$texas0_arkansas1 == 0])
m.texas <-  nrow(data[data$texas0_arkansas1 == 0 & data$term2year == 1])
se.texas <- sqrt((var.0.texas/(N.texas-m.texas)) + (var.1.texas)/m.texas)
se.texas
```

    ## [1] 9.345871

``` r
# Get the variances && m/m
var.0.arkansas <- var((data[data$texas0_arkansas1 == 1 & data$term2year == 0]$bills_introduced))
var.1.arkansas <- var((data[data$texas0_arkansas1 == 1 & data$term2year == 1]$bills_introduced))
N.arkansas <- nrow(data[data$texas0_arkansas1 == 1])
m.arkansas <-  nrow(data[data$texas0_arkansas1 == 1 & data$term2year == 1])
se.arkansas <- sqrt((var.0.arkansas/(N.arkansas-m.arkansas)) + (var.1.arkansas)/m.arkansas)
se.arkansas
```

    ## [1] 3.395979

``` r
# Getting ATE for future use
states.split <- group_by(data, texas0_arkansas1)

texas.split <- states.split[states.split$texas0_arkansas1 == 0, c(1,2,3)]
arkansas.split <- states.split[states.split$texas0_arkansas1 == 1, c(1,2,3)]

texas.split.ate <- afterTreatment(texas.split$bills_introduced, texas.split$term2year)
arkansas.split.ate <- afterTreatment(arkansas.split$bills_introduced, arkansas.split$term2year)
```

**Texas SE is 9.34, and Arkansas ATE is 3.4**

3.  Use equation (3.10) to estimate the overall ATE for both states
    combined.

<!-- end list -->

``` r
overall_ate <- texas.split.ate*(31/66) + arkansas.split.ate*(35/66)
overall_ate
```

    ## [1] -13.2168

**Was able to calculate the split by hand. Got value of -13.22.**

4.  Explain why, in this study, simply pooling the data for the two
    states and comparing the average number of bills introduced by
    two-year senators to the average number of bills introduced by
    four-year senators leads to biased estimate of the overall ATE.

**We have to look at the total amount of bills produced in the states.
If the bills were the same, then we could have pooled the data. This was
not the case when you look at 2/4years and at states.**

5.  Insert the estimated standard errors into equation (3.12) to
    estimate the stand error for the overall ATE.

<!-- end list -->

``` r
se_overall_ate <- sqrt((31*se.texas/66)^2 + (35*se.arkansas/66)^2)
se_overall_ate
```

    ## [1] 4.74478

**Using formula 3.12, was able to calculate ratios by hand. Thus, got
4.74**

6.  Use randomization inference to test the sharp null hypothesis that
    the treatment effect is zero for senators in both states. Here we
    mean: estimate the *overall ate* (which is, the weighted average of
    the block ate) as the internal part of your RI loop.

**Ran out of time.**

7.  **IN Addition:** Plot histograms for both the treatment and control
    groups in each state (for 4 histograms in
total).

<!-- end list -->

``` r
hist(texas.split[texas.split$term2year == 0, ]$bills_introduced, main = "Control Texas", breaks=10)
```

![](ps2_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
hist(texas.split[texas.split$term2year == 1, ]$bills_introduced, main = "Treatment Texas", breaks=10)
```

![](ps2_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
hist(arkansas.split[texas.split$term2year == 0, ]$bills_introduced, main = "Control Arkansas", breaks=10)
```

    ## Warning: Length of logical index must be 1 or 35, not 31

![](ps2_files/figure-gfm/unnamed-chunk-17-3.png)<!-- -->

``` r
hist(arkansas.split[texas.split$term2year == 1, ]$bills_introduced, main = "Treatment Arkansas", breaks=10)
```

    ## Warning: Length of logical index must be 1 or 35, not 31

![](ps2_files/figure-gfm/unnamed-chunk-17-4.png)<!-- -->

# 3\. Cluster Randomization

Use the data in the table below to explore the consequences of cluster
random assignment. (Assume that there are three clusters in treatment
and four clusters in control.) Note that there is no randomization
inference that is necessary to complete this problem because we have
observed the *impossible* **science table**.

``` r
## load data 
d <- fread("./data/clustering_data.csv")
d
```

    ##     id y0 y1
    ##  1:  0  0  0
    ##  2:  1  2  0
    ##  3:  2  2  1
    ##  4:  3  4  4
    ##  5:  4  5  0
    ##  6:  5  6  0
    ##  7:  6  7  3
    ##  8:  7  9  3
    ##  9:  8 14 12
    ## 10:  9 15  8
    ## 11: 10 16  8
    ## 12: 11 17 15
    ## 13: 12 17  6
    ## 14: 13 19 17

1.  Suppose the clusters are formed by grouping observations {1,2},
    {3,4}, {5,6}, … , {13,14}. Use equation (3.22) to calculate the
    standard error. Note that, because we have the full schedule of
    potential outcomes – the science table – it is possible to estimate
    \(cov(\bar{Y}_{j}(0), \bar{Y}_{j}(1))\). If we did not posess this
    information, then we would need to work with equation 3.23.

<!-- end list -->

``` r
d$cluster.name <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7)
clusters <- c(1, 2, 3, 4, 5, 6, 7)

randomize.cluster <- function() {
  control <- sample(clusters, 4, FALSE)
  return(control)
}

control.group <- randomize.cluster()
treatment.group <- setdiff(clusters, control.group)
control.group
```

    ## [1] 5 2 7 4

``` r
treatment.group
```

    ## [1] 1 3 6

``` r
# Assign treatment and control
d$treatment <- 0
d$treatment[d$cluster.name %in% treatment.group] <- 1
d
```

    ##     id y0 y1 cluster.name treatment
    ##  1:  0  0  0            1         1
    ##  2:  1  2  0            1         1
    ##  3:  2  2  1            2         0
    ##  4:  3  4  4            2         0
    ##  5:  4  5  0            3         1
    ##  6:  5  6  0            3         1
    ##  7:  6  7  3            4         0
    ##  8:  7  9  3            4         0
    ##  9:  8 14 12            5         0
    ## 10:  9 15  8            5         0
    ## 11: 10 16  8            6         1
    ## 12: 11 17 15            6         1
    ## 13: 12 17  6            7         0
    ## 14: 13 19 17            7         0

``` r
# Get the cluster based means
means <-  aggregate(d[, 2:3], list(d$cluster.name), mean)
means
```

    ##   Group.1   y0   y1
    ## 1       1  1.0  0.0
    ## 2       2  3.0  2.5
    ## 3       3  5.5  0.0
    ## 4       4  8.0  3.0
    ## 5       5 14.5 10.0
    ## 6       6 16.5 11.5
    ## 7       7 18.0 11.5

``` r
# Use built in formulas
var.0 <- var(means$y0)
var.1 <- var(means$y1)
cov.0 <- cov(means$y0, means$y1)
# cov.0

# Given by Alex
m <- 6
N <- 14
k <- 7
se <- sqrt((((m*var.0)/(N-m)) + (((N-m)*var.1)/(m)) + 2*cov.0)/(k-1))
se
```

    ## [1] 4.854122

**The SE is 4.854122**

2.  Suppose that clusters are instead formed by grouping observations
    {1,14}, {2,13}, {3,12}, … , {7,8}. Use equation (3.22) to calculate
    the standard error assuming half of the clusters are randomly
    assigned to treatment.

<!-- end list -->

``` r
# Basically everything we did above but change the clusters
d$cluster.name.1 <- c(1, 2, 3, 4, 5, 6, 7, 7, 6, 5, 4, 3, 2, 1)
clusters.1 <- c(1, 2, 3, 4, 5, 6, 7)

randomize.cluster.1 <- function() {
  control <- sample(clusters, 4, FALSE)
  return(control)
}

control.group.1 <- randomize.cluster()
treatment.group.1 <- setdiff(clusters, control.group)
control.group.1
```

    ## [1] 2 5 1 4

``` r
treatment.group.1
```

    ## [1] 1 3 6

``` r
# Assign treatment and control
d$treatment.1 <- 0
d$treatment.1[d$cluster.name.1 %in% treatment.group.1] <- 1
d
```

    ##     id y0 y1 cluster.name treatment cluster.name.1 treatment.1
    ##  1:  0  0  0            1         1              1           1
    ##  2:  1  2  0            1         1              2           0
    ##  3:  2  2  1            2         0              3           1
    ##  4:  3  4  4            2         0              4           0
    ##  5:  4  5  0            3         1              5           0
    ##  6:  5  6  0            3         1              6           1
    ##  7:  6  7  3            4         0              7           0
    ##  8:  7  9  3            4         0              7           0
    ##  9:  8 14 12            5         0              6           1
    ## 10:  9 15  8            5         0              5           0
    ## 11: 10 16  8            6         1              4           0
    ## 12: 11 17 15            6         1              3           1
    ## 13: 12 17  6            7         0              2           0
    ## 14: 13 19 17            7         0              1           1

``` r
# Get the cluster based means
means.1 <-  aggregate(d[, 2:3], list(d$cluster.name.1), mean)
means.1
```

    ##   Group.1   y0  y1
    ## 1       1  9.5 8.5
    ## 2       2  9.5 3.0
    ## 3       3  9.5 8.0
    ## 4       4 10.0 6.0
    ## 5       5 10.0 4.0
    ## 6       6 10.0 6.0
    ## 7       7  8.0 3.0

``` r
# Use built in formulas
var.0.1 <- var(means.1$y0)
var.1.1 <- var(means.1$y1)
cov.0.1 <- cov(means.1$y0, means.1$y1)
# cov.0

# Given by Alex
m.1 <- 6
N.1 <- 14
k.1 <- 7
se.1 <- sqrt((((m.1*var.0.1)/(N.1-m.1)) + (((N.1-m.1)*var.1.1)/(m.1)) + 2*cov.0.1)/(k.1-1))
se.1
```

    ## [1] 1.177529

**The SE is 1.177529**

3.  Why do the two methods of forming clusters lead to different
    standard errors? What are the implications for the design of cluster
    randomized experiments?

**We see differences in SE due to the differences in clusters. In part
B, the cluster means were a lot closer together, meaning the SE would be
lower. In terms of inplications, we should try to cluster to the point
where the outcome averages are pretty similar. If not, we get an
inherently biased group, and thus massive swings ini SE. Thus, we would
like the clusters to be relatively similar.**

# 4\. Sell Phones?

Suppose that you are working for a company that sells online display
advertisements. (The generation’s smartest minds, lost to chasing those
clicks…) On client, a consumer electronics company is considering using
your ad network to run a large campaign. In order to evaluate its
effectiveness, they want to run a smaller experiment to estimate the
causal impact of the ads on sales of one of their smartphones.

**The facts**

  - The experiment campaign will run for one week within a randomly
    samples sub-population of 800,000 users
  - The cost per *impression* – someone seeing the ad – is $0.20.
  - The client tells you that they make a profit of $100 every time
    someone purchases one of their smarphones (e.g. the device costs
    $400 to manufacture, and are sold for $500.)
  - When they are **not** running the advertising campaign, the historic
    rate of purchasing has been that 0.004 of the population (0.4%)
    makes a purchase of this smartphone.
  - Assume that everyone who is assigned to the treatment group actually
    sees the ad.
  - Suppose there are no long-run effects and all the effects are
    measured within that week.

<!-- end list -->

1.  How large does the treatment effect need to be in order for the
    campaign to have positive value for the company?

<!-- end list -->

``` r
# Facts given
users <- 800000
profit <- 100
CPM <- 0.2
percent.bought <- 0.004

# Get break even volume
total.weekly.bought <- percent.bought*users
total.weekly.profit <- total.weekly.bought * profit
total.ad.spend <- CPM * users
break.even.volume <- total.ad.spend/profit

# Divide it per person
break.even.volume.perperson <- break.even.volume/users
break.even.volume.perperson
```

    ## [1] 0.002

**Treatment effect needs to be big enough to convert 0.2% of users, or
1600 users, in order to cover the cost of advertising.**

2.  Suppose the measured effect were to be 0.3 percentage points. If
    users are split 50:50 between the treatment group (exposed to iPhone
    ads) and control group (exposed to unrelated advertising or nothing;
    something you can assume has no effect), what will be the confidence
    interval of your estimate on whether people purchase the phone?

<!-- end list -->

``` r
# Given info
controlgroup <- users/2
treatmentgroup <- users/2
measured.effect <- 0.003

# Get p value from formula below
p <- controlgroup * percent.bought + treatmentgroup * (percent.bought + measured.effect)
p <- p/(controlgroup + treatmentgroup)

# Get SE from formula below
standard.error <- sqrt(p*(1-p) * ((1/controlgroup) + (1/treatmentgroup)))


# Get the CI
c.i.top <- measured.effect + 1.96*standard.error
c.i.bottom <- measured.effect - 1.96*standard.error
c.i.top
```

    ## [1] 0.003324134

``` r
c.i.bottom
```

    ## [1] 0.002675866

**The Confidence interval is between \[0.0026, 0.0033\]**

  - **Hint:** The standard error for a two-sample proportion test is
    \(\sqrt{p(1-p)*(\frac{1}{n_{1}}+\frac{1}{n_{2}})}\) where
    \(p=\frac{x_{1}+x_{2}}{n_{1}+n_{2}}\), where \(x\) and \(n\) refer
    to the number of “successes” (here, purchases) over the number of
    “trials” (here, site visits). The length of each tail of a 95%
    confidence interval is calculated by multiplying the standard error
    by 1.96.

<!-- end list -->

3.  Based on this confidence interval, if the effect were 0.3 percentage
    points, would you recommend running the production campaign among
    the whole population? Why or why not?

**Yes, because there is a 95% chance that the true estimate falls within
our confidence interval. It is indeed between \[0.0026, 0.0033\].
Remember to take the 0.03 and divide it by 100.**

4.  Your boss at the newspaper, worried about potential loss of revenue,
    says he is not willing to hold back a control group any larger than
    1% of users. What would be the width of the confidence interval for
    this experiment if only 1% of users were placed in the control group
    and 99% were placed in the treatment
group?

<!-- end list -->

``` r
# Do the same thing as above, but just change the break of the control and test group
control.group <- .01 * users
test.group <- (1-0.01)*users

# P Value formula above
p.boss <-  control.group * percent.bought + test.group * (percent.bought + measured.effect)
p.boss <- p.boss/ (control.group + test.group)

# SE formula above
standard.error.boss <- sqrt(p.boss*(1-p.boss) * ((1/control.group) + (1/test.group)))
c.i.top.boss <- measured.effect + 1.96*standard.error.boss
c.i.bottom.boss <- measured.effect - 1.96*standard.error.boss
c.i.top.boss
```

    ## [1] 0.004832277

``` r
c.i.bottom.boss
```

    ## [1] 0.001167723

**The confidence interval is much bigger, at: \[0.00117, 0.00483\]. This
is bigger than the first interval we saw. To be precise, it is 5.6x as
big.**

# 5\. Sports Cards

Here you will find a set of data from an auction experiment by John List
and David Lucking-Reiley
([2000](https://drive.google.com/file/d/0BxwM1dZBYvxBNThsWmFsY1AyNEE/view?usp=sharing)).

``` r
d <- fread('./data/list_data_2019.csv')
head(d)
```

    ##    bid uniform_price_auction
    ## 1:   5                     1
    ## 2:   5                     1
    ## 3:  20                     0
    ## 4:   0                     1
    ## 5:  20                     1
    ## 6:   0                     1

In this experiment, the experimenters invited consumers at a sports card
trading show to bid against one other bidder for a pair trading cards.
We abstract from the multi-unit-auction details here, and simply state
that the treatment auction format was theoretically predicted to produce
lower bids than the control auction format. We provide you a relevant
subset of data from the experiment.

In this question, we are asking you to produce p-values and confidence
intervals in three different ways: (1) Using a `t.test`, using a
regression, and using randomization inference.

1.  Using a `t.test`, compute a 95% confidence interval for the
    difference between the treatment mean and the control mean.

<!-- end list -->

``` r
control.group.sports <- d$bid[d$uniform_price_auction==0]
treatment.group.sports <- d$bid[d$uniform_price_auction==1]
t_test_result <- t.test(control.group.sports, treatment.group.sports)
t_test_result
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  control.group.sports and treatment.group.sports
    ## t = 2.8211, df = 61.983, p-value = 0.006421
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   3.557141 20.854624
    ## sample estimates:
    ## mean of x mean of y 
    ##  28.82353  16.61765

You should be able to look into `str(t_test_result)` to find the pieces
that you want to pull to include in your written results.

**\[3.56, 20.85\]**

2.  In plain language, what does this confidence interval mean? (Put
    your answer in bold\!)

**We are 95% sure that that the difference for the means between the
treatment and control groups is between \[3.56, 20.85\]**

3.  Regression on a binary treatment variable turns out to give one the
    same answer as the standard analytic formula you just used.
    Demonstrate this by regressing the bid on a binary variable equal to
    0 for the control auction and 1 for the treatment auction.

<!-- end list -->

``` r
mod <- lm(bid ~ uniform_price_auction, data = d)
summary(mod)
```

    ## 
    ## Call:
    ## lm(formula = bid ~ uniform_price_auction, data = d)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.824 -11.618  -3.221   8.382  58.382 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             28.824      3.059   9.421 7.81e-14 ***
    ## uniform_price_auction  -12.206      4.327  -2.821  0.00631 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.84 on 66 degrees of freedom
    ## Multiple R-squared:  0.1076, Adjusted R-squared:  0.09409 
    ## F-statistic: 7.959 on 1 and 66 DF,  p-value: 0.006315

4.  Calculate the 95% confidence interval you get from the regression.
    There is a built in,

<!-- end list -->

``` r
confint(mod, level = 0.95)
```

    ##                           2.5 %    97.5 %
    ## (Intercept)            22.71534 34.931716
    ## uniform_price_auction -20.84416 -3.567603

**We care about the treatment value here, which comes out to be
\[-20.84, -3.57\]**

5.  On to p-values. What p-value does the regression report? Note:
    please use two-tailed tests for the entire problem. (Should be able
    to pull this from the summary. And, you should try to do so with a
    call that *name* calls for the parameter you’re interested in, in
    this case, `uniform_price_auction`.)

<!-- end list -->

``` r
summary(mod)
```

    ## 
    ## Call:
    ## lm(formula = bid ~ uniform_price_auction, data = d)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -28.824 -11.618  -3.221   8.382  58.382 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)             28.824      3.059   9.421 7.81e-14 ***
    ## uniform_price_auction  -12.206      4.327  -2.821  0.00631 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 17.84 on 66 degrees of freedom
    ## Multiple R-squared:  0.1076, Adjusted R-squared:  0.09409 
    ## F-statistic: 7.959 on 1 and 66 DF,  p-value: 0.006315

**We can see the p value is 0.00631, which is the p value for a 2 sided
regression test**

6.  Now compute the same p-value using randomization inference.

<!-- end list -->

``` r
summarize(group_by(d, uniform_price_auction), count=n())
```

    ## # A tibble: 2 x 2
    ##   uniform_price_auction count
    ##                   <int> <int>
    ## 1                     0    34
    ## 2                     1    34

``` r
# Count the amount of 1s and 0s from the summarize function
random1 <- function() {
  sample(c(rep(0, 34), rep(1, 34)))
}
# Run 10000 trials on the distribution 
N1 <- 10000
distribution <- replicate(N1, afterTreatment(d$bid, random1()))

# Get a two sided p test by using an absolute value and compare it against ATE
p_value <- mean(abs(distribution) >= abs(afterTreatment(d$bid, d$uniform_price_auction)))
p_value
```

    ## [1] 0.007

**We get a p value of 0.006**

7.  Pull the same p-value from the `t.test`.

<!-- end list -->

``` r
p.value.sports.ttest <- 0.006421
p.value.sports.ttest
```

    ## [1] 0.006421

**Pulled it from the table above, and got 0.006421**

8.  Compare the two p-values in parts (e) and (f). Are they much
    different? Why or why not? How might your answer to this question
    change if the sample size were different?

**We can see they are extremeley close. If we increase the sample size,
they may converge even more. Because we are sampling 10,000 times, we
know that randomization inference gives us a better estimate. **

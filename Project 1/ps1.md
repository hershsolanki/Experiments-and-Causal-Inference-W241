Problem Set 1
================
Alex, Daniel and Micah
8/27/2019

# Potential Outcomes Notation

1.  Explain the notation \(Y_i(1)\).

*\(Y_i(1)\) is the potential outcome of the treatment group on the ith
subject.*

2.  Explain the notation \(Y_1(1)\).

*\(Y_i(1)\) is the potential outcome of the 1st subject that is
treated.*

3.  Explain the notation \(E[Y_i(1)|d_i=0]\).

**This is the expected value of the treated potential outcome on the ith
subject among subjects that do not receieve any treatment**

4.  Explain the difference between the notation \(E[Y_i(1)]\) and
    \(E[Y_i(1)|d_i=1]\).

**The first is is the expected potential outcome of subject i in the
treatment group, and the second is the expected potential outcome of
subject i in the treatment group when the subject is is treated. d\_i=1
means we have actively assigned the values to certain groups rather than
a random assingment.**

# Potential Outcomes and Treatment Effects

1.  Use the values in the table below to illustrate that
    \(E[Y_i(1)]-E[Y_i(0)] = E[Y_i(1)- [Y_i(0)]\).
2.  Is it possible to collect all necessary values and construct a table
    like the one below in real life? Explain why or why not.

<!-- end list -->

``` r
kable(table)
```

| subject | y\_0 | y\_1 | tau |
| ------: | ---: | ---: | --: |
|       1 |   10 |   12 |   2 |
|       2 |   12 |   12 |   0 |
|       3 |   15 |   18 |   3 |
|       4 |   11 |   14 |   3 |
|       5 |   10 |   15 |   5 |
|       6 |   17 |   18 |   1 |
|       7 |   16 |   16 |   0 |

**1. 105/7 - 91/7 = 2, AND (2+0+3+3+5+1+0)/7 = 14/7 = 2. We can see
these values are equivalent**

**2. It is impossible to make a table like this, because for any
particular individual, you cannot know \(y_0\) and \(y_1\) at the same
time. It is like Schrodinger’s cat –\> if its dead, it can’t be alive,
and vice versa. If one was to be in the control group, it is impossible
to know how they would have simoltaenously reacted in the treatment
group.**

# Visual Acuity

Suppose we are interested in the hypothesis that children playing
outside leads them to have better eyesight.

Consider the following population of ten representative children whose
visual acuity we can measure. (Visual acuity is the decimal version of
the fraction given as output in standard eye exams. Someone with 20/20
vision has acuity 1.0, while someone with 20/40 vision has acuity 0.5.
Numbers greater than 1.0 are possible for people with better than
“normal” visual acuity.)

``` r
kable(d)
```

| child | y\_0 | y\_1 |
| ----: | ---: | ---: |
|     1 |  1.2 |  1.2 |
|     2 |  0.1 |  0.7 |
|     3 |  0.5 |  0.5 |
|     4 |  0.8 |  0.8 |
|     5 |  1.5 |  0.6 |
|     6 |  2.0 |  2.0 |
|     7 |  1.3 |  1.3 |
|     8 |  0.7 |  0.7 |
|     9 |  1.1 |  1.1 |
|    10 |  1.4 |  1.4 |

In this table, `y_1` means means the measured *visual acuity* if the
child were to play outside at least 10 hours per week from ages 3 to 6.
`y_0` means the measured *visual acuity* if the child were to play
outside fewer than 10 hours per week from age 3 to age 6. Both of these
potential outcomes *at the child level* would be measured at the same
time, when the child is 6.

1.  Compute the individual treatment effect for each of the ten
    children.

In order: **{0, 0.6, 0, 0, -0.9, 0, 0, 0, 0, 0}**

2.  Tell a “story” that could explain this distribution of treatment
    effects. In particular, discuss what might cause some children to
    have different treatment effects than others.

**Some kids may be accesing technlogy when they are not outside,
irregardless of how much time they spend outside. Thus, the kid’s
eyesight that got worse was likely using technology to a greater amount.
Other kid who improved his eyesight mught be due to a healtheir diet.
The rest of the kids had no difference, meaning nothing else in the way
they lived their lives significantly changed.**

3.  For this population, what is the true average treatment effect (ATE)
    of playing outside.

**(1.2 + 0.7 + 0.5 + 0.8 + 0.6 + 2.0 + 1.3 + 0.7 + 1.1 + 1.4)/10 =
1.03**  
**(1.2 + 0.1 + 0.5 + 0.8 + 1.5 + 2.0 + 1.3 + 0.7 + 1.1 + 1.4)/10 =
1.06**  
**1.03 - 1.06 = -0.03**

4.  Suppose we are able to do an experiment in which we can control the
    amount of time that these children play outside for three years. We
    happen to randomly assign the odd-numbered children to treatment and
    the even-numbered children to control. What is the estimate of the
    ATE you would reach under this assignment? (Please describe your
    work.)

**y\_0 = c(1.2, 0.1, 0.5, 0.8, 1.5, 2.0, 1.3, 0.7, 1.1, 1.4)**  
**y\_1 = c(1.2, 0.7, 0.5, 0.8, 0.6, 2.0, 1.3, 0.7, 1.1, 1.4)**

**y\_0\_odd = 1.2, 0.5, 1.5, 1.3, 1.1**  
**y\_1\_odd = 1.2, 0.5, 0.6, 1.3, 1.1**  
**y\_0\_even = 0.1, 0.8, 2.0, 0.7, 1.4**  
**y\_1\_even = 0.7, 0.8, 2.0, 0.7, 1.4**

**ATE odd = (1.2 + 0.5 + 0.6 + 1.3 + 1.1)/5 - (1.2 + 0.5 + 1.5 + 1.3 +
1.1)/5 = -0.18**  
**ATE even = (0.7 + 0.8 + 2.0 + 0.7 + 1.4)/5 - (0.1 + 0.8 + 2.0 + 0.7 +
1.4)/5 = 0.12**

5.  How different is the estimate from the truth? Intuitively, why is
    there a difference?

**We can see the estimates are an equidistant value of 15 from the
truth. There is a difference because for both the odd and even, there
were only a singular value that changed. For the odd, it was the third
value (1.5 to 0.6), and for the even it was the first value (.1 to 0.7).
Thus, these two changes new out, and we get an equidistant value.**

6.  We just considered one way (odd-even) an experiment might split the
    children. How many different ways (every possible ways) are there to
    split the children into a treatment versus a control group (assuming
    at least one person is always in the treatment group and at least
    one person is always in the control group)?

**We think about using the choose function. We know there must be a min
of 1 person in each group, so we add all the poosbilities where there
are between 1 and 9 memebers in a group:**

**10 choose 1 = 10** **10 choose 2 = 45** **10 choose 3 = 120** **10
choose 4 = 210** **10 choose 5 = 252** **10 choose 6 = 210** **10 choose
7 = 120** **10 choose 8 = 45** **10 choose 9 = 10**

**Total is 1,022 ways.**

7.  Suppose that we decide it is too hard to control the behavior of the
    children, so we do an observational study instead. Children 1-5
    choose to play an average of more than 10 hours per week from age 3
    to age 6, while Children 6-10 play less than 10 hours per week.
    Compute the difference in means from the resulting observational
    data.

**If we look at observational data, we get the following:**

**Children 1-5: 1.2, 0.7, 0.5, 0.8, 0.6 -\> average of 0.76** **Children
6-10: 2.0, 1.3, 0.7, 1.1, 1.4 -\> average of 1.3**

**We see the difference to be 1.3 - 0.76 = 0.54**

8.  Compare your answer in (g) to the true ATE. Intuitively, what causes
    the difference?

**The difference comes from the fact that both the values that changed
were in the first group. This brought down the average significantly,
and was balanced out by the second group (children 6-10)**

# Randomization and Experiments

1.  Assume that researcher takes a random sample of elementary school
    children and compare the grades of those who were previously
    enrolled in an early childhood education program with the grades of
    those who were not enrolled in such a program. Is this an experiment
    or an observational study? Explain\!

**This is clearly an observational study. Don’t let the random sample
fool you, that has more to do with the analysis you can do that relates
the sample to the population. There is no test or control group, and
people are NOT randomly assinged to these groups. Thus, this can’t be an
experiment\! There is no way to reproduce these results because we don’t
know the coufounding variables that had the kids end up in 1 of the 2
programs.**

2.  Assume that the researcher works together with an organization that
    provides early childhood education and offer free programs to
    certain children. However, which children that received this offer
    was not randomly selected by the researcher but rather chosen by the
    local government. (Assume that the government did not use random
    assignment but instead gives the offer to students who are deemed to
    need it the most) The research follows up a couple of years later by
    comparing the elementary school grades of students offered free
    early childhood education to those who were not. Is this an
    experiment or an observational study? Explain\!

**Again, we see an observational study for many of the same reasons as
the last problem. The biggest issues is that we do not know if the
control/test groups are comparable because they do not reflect the same
type of students. It could be the case that the local government just
happened to put the smarter kids in the free program. Thus, the grades
are a function of the kids, and not the program. This flaw means you
cannot have an experiment.**

3.  Does your answer to part (2) change if we instead assume that the
    government assigned students to treatment and control by “coin toss”
    for each student?

# Moral Panic

Suppose that a researcher finds that high school students who listen to
death metal music at least once per week are more likely to perform
badly on standardized test. As a consequence, the researcher writes an
opinion piece in which she recommends parents to keep their kids away
from “dangerous, satanic music”. Let \(Y_i(0)\) be each student’s test
score when listening to death metal at least one time per week. Let
\(Y_i(1)\) be the test score when listening to death metal less than one
time per week.

1.  Explain the statement \(E[Y_i(0)|D_i=0] = E[Y_i(0)|D_i=1]\) in
    words. First, state the rote english language translation; but then,
    second, tell us the *meaning* of this statement.

**The statement in English means the expected value of the ith student
who listens to death music atleast 1x a week given the dosage is 0 is
equivalent to expected value of the ith student who listens to death
music atleast 1x a week given the dosage is 1. Dosage 0 means death
metal was listened to atleast 1x a week, and dosage 1 is the converse.
In other words, this means that there is no selection bias, because if
there was an ATE, then the two sides would not be equivalent**

2.  Do you expect the above condition to hold in this case? Explain why
    or why not.

**I do not expect the condition to hold, as there will clearly be an
effect of listening to music. If there was no affect, then there is no
point of the study in the first place**

# MIDS Admission

Suppose a researcher at UC Berkeley wants to test the effect of taking
the MIDS program on future wages. The researcher convinces the School of
Information to make admission into the MIDS program random among those
who apply. The idea is that since admission is random, it is now
possible to later obtain an unbiased estimate of the effect by comparing
wages of those who where admitted to a random sample of people who did
not take the MIDS program. Do you believe this experimental design would
give you an unbiased estimate? Explain why or why not. Assume that
everybody who gets offer takes it and that prospective students do not
know admission is random.

**This would not give an unbiased estimate, as there are other
confounding factors to those applying and getting into MIDS. It would be
impossible to attribute the program to the wage gap (if there is any).
It may play a part, but it would be hard to quantify because it is
difficult to quantify the confounding variables. An unbiased estimate is
one that estimates values equal to what the true parameter is, but that
would not be the case here.**

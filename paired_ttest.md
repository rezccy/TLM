Paired T-Test
================

We can conduct the paired t-test in R with a few simple commands. Most of these are just straight forward implementation of the mathematics behind the hypothesis test. Let start with the data.

``` r
before = c(164, 137, 133, 157, 173, 143, 144, 180, 178, 154) #Data collected before treatment
after = c(129, 127, 122, 120, 129, 122, 125, 139, 145, 139) #Data collected after treatment
D0 = 0 #hypothesized difference
```

To calculate the test statistics, we can just directly use the test statistics formula:

``` r
di = before - after #Calculate the differences for each pair
d.bar = mean(di) #Mean of differences
s.d = sd(di) #Sample standard deviation of the differences
n = length(before) #Number of data pairs
test.stat = (d.bar - D0)/(s.d/sqrt(n))
print(test.stat)
```

    ## [1] 6.576427

To get the critical value, we can use the `qt()` function (for the t-distribution):

``` r
crit.val = qt(p = 0.01, df = n - 1, lower.tail = F)
print(crit.val)
```

    ## [1] 2.821438

The argument `df` is for the degrees of freedom, where the argument `lower.tail = F` is to tell R that we want to find the critical value which gives a probability of 0.01 on the upper tail. If we were to do a lower-tailed test, the command will be `qt(p = 0.01, df = n - 1, lower.tail = T)` (or by default, `lower.tail` is TRUE if there is no input for this argument).

To obtain the p-value, we can use the `pt()` function:

``` r
p.val = pt(test.stat, df = n - 1, lower.tail = F)
print(p.val)
```

    ## [1] 5.10044e-05

If this were a two-tailed test, remember we need to multiply the p-value by 2:

``` r
p.val2 = 2*pt(abs(test.stat), df = n - 1, lower.tail = F) #If we were to do a two-tailed test
print(p.val2)
```

    ## [1] 0.0001020088

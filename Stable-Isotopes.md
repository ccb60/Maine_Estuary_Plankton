Stable Isotope Data
================

-   [Introduction](#introduction)
-   [Load Libraries](#load-libraries)
-   [Stable Isotope Data](#stable-isotope-data)
-   [Data Prevalence](#data-prevalence)
    -   [Review of delta\_n data](#review-of-delta_n-data)
-   [Calculate Average Values](#calculate-average-values)
    -   [Graphic Exploration of Site and
        Date](#graphic-exploration-of-site-and-date)
        -   [Delta N Data](#delta-n-data)
        -   [Delta C Data](#delta-c-data)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Introduction

In this notebook I am looking at the stable isotope data and doing some
very lightweight modeling, looking principally at seasonal patterns.

# Load Libraries

``` r
library(tidyverse)
#> -- Attaching packages --------------------------------------- tidyverse 1.3.1 --
#> v ggplot2 3.3.5     v purrr   0.3.4
#> v tibble  3.1.6     v dplyr   1.0.7
#> v tidyr   1.1.4     v stringr 1.4.0
#> v readr   2.1.1     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(readxl)

library(emmeans)

theme_set(theme_minimal())
```

# Stable Isotope Data

This data is found in two different Tabs. It appears there are some
replicate analyses here, labeled in confusing ways….

I focus only on the stable isotope values.

`Sample_ID` codes do NOT match the `Sample_ID` codes from other tabs.
The difference is small, but would interfere with matches. We correct
them here.

``` r
POC1_data <- read_excel("SEANET_Phyto Data_Bigelow.xlsx", 
                        sheet = "POC1", skip = 11, n_max = 30) %>%
  rename(Sample_ID = `Sample ID`) %>%
  mutate(Replicate = substr(Sample_ID, nchar(Sample_ID), nchar(Sample_ID)),
         Sample_ID = substr(Sample_ID, 1, nchar(Sample_ID)-2)) %>%
  relocate(Replicate, .after = 'Sample_ID') %>%
  select(c(1,2,9,10, 11))
  
names(POC1_data)[3] <- 'delta_n'
names(POC1_data)[4] <- 'delta_c'
```

``` r
POC2_data <- read_excel("SEANET_Phyto Data_Bigelow.xlsx", 
                        sheet = "POC2", skip = 11, n_max = 30) %>%
  rename(Sample_ID = `Sample ID`) %>%
  mutate(Replicate = 'C') %>%
  relocate(Replicate, .after = 'Sample_ID') %>%
  select(c(1,2,10, 11, 12))

POC2_data$Replicate[19] <- 'D'  # Only apparent duplicate in second round of analysis

names(POC2_data)[3] <- 'delta_n'
names(POC2_data)[4] <- 'delta_c'
```

``` r
POC_data <- POC1_data %>%
  bind_rows(POC2_data) %>%
  mutate(date = as.Date(substr(Sample_ID, 1,10)),
         site = substr(Sample_ID, 12, nchar(Sample_ID))) %>%
  mutate(Sample_ID = paste0('SNT_', Sample_ID)) %>%
  relocate(date, site, .after = Sample_ID)
```

``` r
rm(POC1_data, POC2_data)
```

# Data Prevalence

``` r
xtabs(~ date + site, data = POC_data)
#>             site
#> date         LOBO1 LOBO2 MOORING UNE3
#>   2016-05-17     1     1       1    1
#>   2016-05-31     1     1       1    1
#>   2016-06-14     1     1       1    1
#>   2016-06-21     2     2       1    1
#>   2016-07-05     2     2       1    1
#>   2016-07-19     2     2       1    1
#>   2016-08-02     2     2       1    1
#>   2016-08-16     2     2       1    0
#>   2016-08-30     2     2       1    1
#>   2016-09-13     2     1       1    1
#>   2016-09-27     1     1       1    0
#>   2016-10-11     1     1       1    1
#>   2016-10-25     1     0       0    0
```

I have no geographic data on the Mooring site, and “Mooring” is not a
site designation in other tabs.

## Review of delta\_n data

``` r
ggplot(POC_data, aes(Sample_ID, delta_n)) +
  geom_point(aes(color = ! is.na(Comments), shape = Replicate)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
```

<img src="Stable-Isotopes_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

We see two outliers, both of which were flagged in the analytic results
as “unusually high.” They are wildly different from oth replicate
analyses and other observed values. While there is no other indication
of WHY they are so high, I will omit them from further analysis. Note
that I chose to drop only the delta\_n values, not the related delta\_c
values. I do not know enough about lab procedures to be sure that is
correct.

``` r
POC_data <- POC_data %>%
  mutate(delta_n = if_else(row_number() %in% c(16,17), NA_real_, delta_n))
```

# Calculate Average Values

``` r
POC_data_sum <- POC_data %>%
  group_by(Sample_ID) %>%
  summarize(delta_n_mean = mean(delta_n, na.rm = TRUE),
            delta_n_SD =   sd(delta_n, na.rm = TRUE),
            Deta_N_n  =    sum(! is.na(delta_n)),
            delta_c_mean = mean(delta_c, na.rm = TRUE),
            delta_c_SD =   sd(delta_c, na.rm = TRUE),
            Deta_C_n  =    sum(! is.na(delta_c)))
```

We have only a single measurement of most samples, yet the standard
errors are fairly large for those samples where we **do** have
replicates. That raises interesting questions about preecision of these
data.

## Graphic Exploration of Site and Date

### Delta N Data

``` r
POC_data_test <- POC_data %>%
  select(-Comments) %>%
  mutate(site = factor(site),
         date_factor = factor(date))
```

``` r
ggplot(POC_data_test, aes(date, delta_n)) +
  geom_line(aes(color = site)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
```

<img src="Stable-Isotopes_files/figure-gfm/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

### Delta C Data

``` r
ggplot(POC_data_test, aes(date, delta_c)) +
  geom_line(aes(color = site)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2))
```

<img src="Stable-Isotopes_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

So, looking at Carbon, it looks like late summer samples tend to be high
Delta C.

#### Summarize by Station

``` r
POC_sum <- POC_data %>%
  group_by(Sample_ID, site, date) %>%
  summarize(across(c(delta_n, delta_c), .fns = c(mean = mean, 
                                                 sd = sd,
                                                 count = ~sum(! is.na(.))), 
                na.rm = TRUE, .names = '{.col}_{.fn}'),
            .groups = 'drop') %>%
  mutate(delta_n_cv = delta_n_sd / delta_n_mean,
         delta_c_cv = delta_c_sd/delta_c_mean) %>%
  relocate(delta_n_cv, .after = delta_n_sd)
```

``` r
ggplot(POC_sum, aes(delta_c_mean, delta_n_mean, color = site)) +
  geom_point() + 
  #coord_fixed() +
  
  geom_linerange(aes(ymin = delta_n_mean - delta_n_sd, 
                     ymax = delta_n_mean + delta_n_sd)) +
  geom_linerange(aes(xmin = delta_c_mean - delta_c_sd, 
                     xmax = delta_c_mean + delta_c_sd))
#> Warning: Removed 36 rows containing missing values (geom_segment).
#> Warning: Removed 34 rows containing missing values (geom_segment).
```

<img src="Stable-Isotopes_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

#### Models For Estimates of Error

It’s not obvious how to best estimate the error associated with these
estimates. We only have replicate lab samples for a few sites.

This is a convenient way of creating an aggregate error.

``` r
delta_n_lm <- lm(delta_n ~ Sample_ID,  data = POC_data)
anova(delta_n_lm)
#> Analysis of Variance Table
#> 
#> Response: delta_n
#>           Df  Sum Sq Mean Sq F value   Pr(>F)   
#> Sample_ID 46 1472.66  32.014  6.1944 0.001208 **
#> Residuals 11   56.85   5.168                    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

delta_c_lm <- lm(delta_c ~ Sample_ID,  data = POC_data)
anova(delta_c_lm)
#> Analysis of Variance Table
#> 
#> Response: delta_c
#>           Df  Sum Sq Mean Sq F value    Pr(>F)    
#> Sample_ID 46 223.371  4.8559  8.4951 8.365e-05 ***
#> Residuals 13   7.431  0.5716                      
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
my_emms_n <- as.data.frame(emmeans(delta_n_lm, "Sample_ID")) %>%
  rename(delta_n_mean = emmean, delta_n_se = SE) %>%
  select(-lower.CL, -upper.CL) %>%
  select(-df)

my_emms_c <- as.data.frame(emmeans(delta_c_lm, "Sample_ID")) %>%
  rename(delta_c_mean = emmean, delta_c_se = SE) %>%
  select(-lower.CL, -upper.CL) %>%
  select(-df)

my_emms <- my_emms_n %>% 
  left_join (my_emms_c, by = 'Sample_ID') %>%
  mutate(sample_id_txt = as.character(Sample_ID),
         date = as.Date(substr(sample_id_txt, 5,14)),
         site = substr(sample_id_txt, 16, nchar(sample_id_txt))) %>%
  select(-sample_id_txt)
```

The following plot shows the means +/- one standard deviation. These are
NOT 95% confidence intervals, which would be even wider.

``` r
ggplot(my_emms, aes(delta_c_mean, delta_n_mean, color = site)) +
  geom_point() +
  geom_linerange(aes(ymin = delta_n_mean - delta_n_se, 
                     ymax = delta_n_mean + delta_n_se)) +
  geom_linerange(aes(xmin = delta_c_mean - delta_c_se, 
                     xmax = delta_c_mean + delta_c_se))
```

<img src="Stable-Isotopes_files/figure-gfm/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

This gives a fairly good picture of measurement uncertainty, although I
suspect it somewhat underestimates uncertainty, for technical reasons.
What jumps out at me is the clear separation of the UNE site from the
other sites, and its high (largely seasonal, judging by prior graphics)
variation.

Let’s look at that a little more closely.

``` r
my_emms %>%
  filter(site == 'UNE3') %>%
  mutate(month = format(date, format = '%m')) %>%
  ggplot(aes(delta_c_mean, delta_n_mean, color = month)) +
  geom_point() +
  geom_linerange(aes(ymin = delta_n_mean - delta_n_se, 
                     ymax = delta_n_mean + delta_n_se)) +
  geom_linerange(aes(xmin = delta_c_mean - delta_c_se, 
                     xmax = delta_c_mean + delta_c_se))
```

<img src="Stable-Isotopes_files/figure-gfm/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

Looking at eh Damariscotta Estuary
================

-   [Zooplankton Data](#zooplankton-data)
    -   [Community Data Matrix](#community-data-matrix)
    -   [Reorder columns by mean count](#reorder-columns-by-mean-count)
-   [Load Corresponding Phytoplankton
    Data](#load-corresponding-phytoplankton-data)
    -   [Collection Data](#collection-data)
    -   [Data on Picoplankton and
        Nanoplankton](#data-on-picoplankton-and-nanoplankton)
    -   [Chlorophyll and Phaeophytin](#chlorophyll-and-phaeophytin)
    -   [PMAX Data](#pmax-data)
    -   [Nutrients](#nutrients)
    -   [C N Data and Stable Isotope
        Data](#c-n-data-and-stable-isotope-data)
    -   [Flowcam Data](#flowcam-data)
    -   [Total\_GT\_20 is sum of other biomass
        numbers](#total_gt_20-is-sum-of-other-biomass-numbers)
-   [Assembling the Predictor Data
    Set](#assembling-the-predictor-data-set)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

\#Introduction In this notebook I am looking at multiple sources of
data, looking only at data from the Damariscotta estuary. The goal is to
model analysis of multiple community and environmental predictor
variables.

\#Load Libraries

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

## Zooplankton Data

The zooplankton Data for the Damariscotta Estuary is coded consistently
with “DRE, making it easy to filter to those sites only. Then, samples
are coded with a ’Station Number”. It appears from data in the
“Phyto\_Envrionmental” tab that I can infer which stations match
phytoplankton data, and which do not.

Here I create a code that matches the `Sample_ID` in the phytoplankton
data. that code will allow me to import data from the phytoplankton file
using a “join” function that ensures I get samples lined up correctly.,
where data is available from both data sources.

``` r
zoopl_data_long <- read_excel("Seanet ALL DATA FILE.xlsx", 
    sheet = "Zoop_Composition") %>%
  arrange(Estuary, STATION, DATE) %>%
  filter(Estuary == "DRE") %>%
  mutate(phyto_site = case_when(STATION == 1 ~ 'NA1',
                          STATION == 2 ~ 'LOBO1',
                          STATION == 3 ~ 'NA3',
                          STATION == 4 ~ 'LOBO2',
                          STATION == 5 ~ 'UNE3')) %>%
  mutate(new_id = paste('SNT' , DATE, phyto_site, sep = '_') ) %>%
  select(-phyto_site) %>%
  relocate(new_id)
```

### Community Data Matrix

I spend a fair amount of time here trying to construct consistent sample
ID codes so I can join the zooplankton and environmental data together
correctly. I’ve made a BUNCH of assumptions here, and I don’t know if
they are correct or not.

``` r
zoopl_community <- zoopl_data_long %>%
  rename(count = QUANTITY,
         name = NAME) %>%
  select(new_id, name, count) %>%
  pivot_wider(names_from = name, values_from = count, values_fill = 0)
```

### Reorder columns by mean count

(I could also reorder by how often each species was observed ina
sample….).

``` r
my_order <- zoopl_community %>%
  summarize(across(c(-1), mean)) %>%
  unlist %>%
  sort(decreasing = TRUE) %>%
  names
my_order
#>  [1] "Acartia"         "Eurytemora"      "Pseudocalanus"   "Pseudodiaptomus"
#>  [5] "Centropages"     "Temora"          "Balanus"         "Podonidae"      
#>  [9] "Gastropod"       "Evadne"          "Oithona"
```

## Load Corresponding Phytoplankton Data

### Collection Data

``` r
collection_data <- read_excel("SEANET_Phyto Data_Bigelow.xlsx", 
    sheet = "COLLECTION", col_types = c("date", 
        "date", "text", "numeric", "text", 
        "text", "text", "text", "date", "text", 
        "numeric", "numeric", "text", "text", 
        "text", "text", "text", "text", "skip", 
        "skip", "skip", "skip"), 
    skip = 20, na = '-999') %>%
  rename_with(.fn = ~sub(" \\(.*", "", .x) ) %>%
  rename_with(.fn = ~gsub(" ", "_", .x)) %>%
  rename(date = Date_Collected,
         site = Station)
```

#### Look at Station names

It’s clear we have a problem. Once we drop Ram“,”WOOD“, and”BUOY“, we
are left with only three locations – all corresponding to locations in
the Damariscotta. Since almost all data in this file corresponds to
these samples, it looks like we have no emvironmental data from
the”Mooring" site.

``` r
unique(collection_data$site)
#> [1] "LOBO1"    "LOBO2"    "UNE3"     "BUOY"     "RAM"      "WOOD"     "BUOY (6)"
#> [8] "RAM (4)"  "WOOD (5)"
```

``` r
collection_data <- collection_data %>%
  filter(site %in% c('LOBO1', 'LOBO2', 'UNE3'))
```

### Data on Picoplankton and Nanoplankton

``` r
pico_data <- read_excel("SEANET_Phyto Data_Bigelow.xlsx", 
                        sheet = "PICO-NANO")  %>%
  rename_with(.fn = ~sub(" \\(.*", "", .x) ) %>%
  rename_with(.fn = ~gsub(" ", "_", .x)) %>%
  mutate(date = as.Date(substr(Sample_ID, 5,14)),
         site = substr(Sample_ID, 16, nchar(Sample_ID))) %>%
  filter(site %in% c('LOBO1', 'LOBO2', 'UNE3'))
```

We note that we have two MORE samples here than indicated in the data on
sample collection. This is because two sample IDs are duplicated.

``` r
pico_data$Sample_ID[duplicated( pico_data$Sample_ID)]
#> [1] "SNT_2016-08-02_UNE3"  "SNT_2016-08-30_LOBO1"
```

These need to be resolved before we can continue with the pico data.

### Chlorophyll and Phaeophytin

We drop the “pre calculated” means and standard deviations for the time
being. they are simple to recalculate in R, and that way I have full
documentaiton of methods and assumptions.

``` r
chl_data <-   read_excel("SEANET_Phyto Data_Bigelow.xlsx", 
                        sheet = "CHL", skip = 25) %>%
  select(-starts_with('Avg')) %>%
  rename_with(.fn = ~sub(" \\(.*", "", .x) ) %>%
  rename_with(.fn = ~gsub(" ", "_", .x)) %>%
  mutate(Replicate = substr(Sample_ID, nchar(Sample_ID), nchar(Sample_ID)),
         Sample_ID = substr(Sample_ID, 1, nchar(Sample_ID)-2)) %>%
  relocate(Replicate, .after = 'Sample_ID') %>%
  mutate(date = as.Date(substr(Sample_ID, 5,14)),
         site = substr(Sample_ID, 16, nchar(Sample_ID))) %>%
  filter(site %in% c('LOBO1', 'LOBO2', 'UNE3')) %>%
  relocate(site, date, .after = Replicate)
```

``` r
chl_sum <- chl_data %>%
  select(-c(Date_Read:Ra, Comments)) %>%
  group_by(Sample_ID, site, date) %>%
  summarize(across(c(Chl_a, Pheo), .fns = c(mean = mean, 
                                            sd = sd), 
                na.rm = TRUE, .names = '{.col}_{.fn}'),
            .groups = 'drop') %>%
  mutate(Chl_a_cv = Chl_a_sd / Chl_a_mean,
         Pheo_cv = Pheo_sd/Pheo_mean) %>%
  relocate(Chl_a_cv, .after = Chl_a_sd)
```

### PMAX Data

``` r
pmax_data <- read_excel("SEANET_Phyto Data_Bigelow.xlsx", 
    sheet = "PMAX", skip = 12) 
names(pmax_data) <- c('Sample_ID', 'pmax', 'pmax_chl', 'Comments')


pmax_data <-  pmax_data %>%
  mutate(date = as.Date(substr(Sample_ID, 5,14)),
         site = substr(Sample_ID, 16, nchar(Sample_ID))) %>%
  filter(site %in% c('LOBO1', 'LOBO2', 'UNE3')) %>%
  mutate(pmax = if_else(pmax == -999, NA_real_, pmax),
         pmax_chl = if_else(pmax_chl == -999, NA_real_, pmax_chl)) %>%
    relocate(site, date, .after = Sample_ID)
```

### Nutrients

``` r
nutrients_data <-  read_excel("SEANET_Phyto Data_Bigelow.xlsx",
                              sheet = "DIN", skip = 5)
names(nutrients_data) <- c('Sample_ID', 'NOx', 'NO2', 'NO3',
                           'NH4', 'PO4', 'Si')
```

#### Add Organic Nitrogen Data

``` r
DON_data <- read_excel("SEANET_Phyto Data_Bigelow.xlsx", 
    sheet = "DON", skip = 5)
names(DON_data) <- c("Sample_ID", "DON")

nutrients_data <- nutrients_data %>%
  full_join(DON_data, by = "Sample_ID") %>%
  relocate(DON, .after = NH4) %>%
  mutate(date = as.Date(substr(Sample_ID, 5,14)),
         site = substr(Sample_ID, 16, nchar(Sample_ID))) %>%
  filter(site %in% c('LOBO1', 'LOBO2', 'UNE3'))  %>%
    relocate(site, date, .after = Sample_ID)
```

``` r
rm(DON_data)
```

### C N Data and Stable Isotope Data

This is found in two different Tabs. It appears these are some replicate
analyses here, labeled in confusing ways….

The Elemental N and C values are labeled as “not blank corrected”. I am
not sure whether they should be blank corrected, and if so, how to do so
correctly. My tendency would be to “correct” by the mean value of the
blanks. This would have a small effect on elemental carbon, but a
sizable effect on elemental nitrogen.

But because of these uncertainties, I focus only on the stable isotope
values.

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
  geom_linerange(aes(ymin = delta_n_mean - delta_n_sd, 
                     ymax = delta_n_mean + delta_n_sd)) +
  geom_linerange(aes(xmin = delta_c_mean - delta_c_sd, 
                     xmax = delta_c_mean + delta_c_sd))
#> Warning: Removed 34 rows containing missing values (geom_segment).

#> Warning: Removed 34 rows containing missing values (geom_segment).
```

<img src="Damariscotta-Estuary-Analysis_files/figure-gfm/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

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
#>           Df  Sum Sq Mean Sq F value Pr(>F)
#> Sample_ID 46 2462.45  53.531  0.8061 0.7167
#> Residuals 13  863.33  66.410

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

``` r
ggplot(my_emms, aes(delta_c_mean, delta_n_mean, color = site)) +
  geom_point() +
  geom_linerange(aes(ymin = delta_n_mean - delta_n_se, 
                     ymax = delta_n_mean + delta_n_se)) +
  geom_linerange(aes(xmin = delta_c_mean - delta_c_se, 
                     xmax = delta_c_mean + delta_c_se))
```

<img src="Damariscotta-Estuary-Analysis_files/figure-gfm/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

### Flowcam Data

``` r
flowcam_data <- read_excel("SEANET_Phyto Data_Bigelow.xlsx", 
                               sheet = "FCAM FCM", skip = 59) %>%
  rename_with(.fn = ~sub(" \\(.*", "", .x) ) %>%
  rename_with(.fn = ~sub("FCM ", "", .x) ) %>%
  rename_with(.fn = ~sub("Vol", "", .x) ) %>%
  rename_with(.fn = ~sub(" *$", "", .x) ) %>%
  rename_with(.fn = ~gsub(" ", "_", .x)) %>%
  rename_with(.fn = ~gsub("\\.", "", .x)) %>%
  rename('Total_GT_20' = starts_with('Total_>')) %>%
  mutate(Sample_ID = if_else(grepl('_F$', Sample_ID),
                           substr(Sample_ID, 1, nchar(Sample_ID)-2),
                           Sample_ID)) %>%
  mutate(date = as.Date(substr(Sample_ID, 5,14)),
         site = substr(Sample_ID, 16, nchar(Sample_ID))) %>%
  filter(site %in% c('LOBO1', 'LOBO2', 'UNE3'))  %>%
  relocate(site, date, .after = Sample_ID)
```

#### Data Groups

Notice that not all groups are equivalent, so these should not be
analyzed as one large group.

``` r
cat ('\n**************Basic Phytoplankton Volumes******************\n')
#> 
#> **************Basic Phytoplankton Volumes******************
names(flowcam_data)[4:33]
#>  [1] "Mixed_Diatoms"        "Other_Centric"        "Short_Centric_Chains"
#>  [4] "Long_Centric_Chains"  "Curly_Centric_Chains" "Single_Centric"      
#>  [7] "Chaetoceros"          "C_socialis"           "Rhizosolenia"        
#> [10] "Mediopyxis"           "Other_Pennate"        "Single_Pennate"      
#> [13] "Chain_Pennate"        "Thallassionema"       "Pseudo-nitzschia"    
#> [16] "Asternionellopsis"    "Other_Dinos"          "Dinophysis"          
#> [19] "Ceratium"             "C_longipes"           "C_lineatum"          
#> [22] "C_fusus"              "Prorocentrum"         "Other_Ciliates"      
#> [25] "Strom-Strob"          "Mesodinium"           "Laboea"              
#> [28] "Dictyocha"            "Other_and_UID"        "LT20"
cat ('\n**************Non Phytoplankton ******************\n')
#> 
#> **************Non Phytoplankton ******************
names(flowcam_data)[34:36]
#> [1] "Fecal_Pellets" "Phytodetritus" "Zooplankton"
cat ('\n**************Group Total Volumes******************\n')
#> 
#> **************Group Total Volumes******************
names(flowcam_data)[37:40]
#> [1] "Total_Diatom"        "Total_Dino"          "Total_Ciliate"      
#> [4] "Total_Other_and_UID"
cat ('\n**************Group Total Biomass******************\n')
#> 
#> **************Group Total Biomass******************
names(flowcam_data)[41:45]
#> [1] "Total_Diatom_Biomass"        "Total_Dino_Biomass"         
#> [3] "Total_Ciliate_Biomass"       "Total_Other_and_UID_Biomass"
#> [5] "Total_GT_20"
```

### Total\_GT\_20 is sum of other biomass numbers

``` r
flowcam_data[41:45] %>%
  mutate(check = Total_Diatom_Biomass + Total_Dino_Biomass + 
           Total_Ciliate_Biomass + Total_Other_and_UID_Biomass)
#> # A tibble: 44 x 6
#>    Total_Diatom_Biomass Total_Dino_Biomass Total_Ciliate_Bio~ Total_Other_and_U~
#>                   <dbl>              <dbl>              <dbl>              <dbl>
#>  1                 9.81              0.187               1.60               2.75
#>  2                14.1               0.209               5.99               7.18
#>  3                 5.92              0.119              51.7                4.01
#>  4                 4.84              0.148               6.14               4.70
#>  5                10.5               0.542              13.6                7.69
#>  6                 6.87              0.393              38.8                7.70
#>  7                 6.62              0.863              16.0                4.51
#>  8                 7.64              0.944               6.57               7.26
#>  9                 6.66              0.675               1.65               2.96
#> 10                 4.08              0.823               9.62               3.79
#> # ... with 34 more rows, and 2 more variables: Total_GT_20 <dbl>, check <dbl>
```

# Assembling the Predictor Data Set

``` r
predictors <- collection_data %>%
  select(-c(Time_Collected, Collected_By, `Collection_Notes/Weather_Conditions`,
            Time_Received, POC_Vol_Filt, Chl_a_Vol_Filt, Nutrients_Filtration,
            FlowCAM_Operator:Fluorometer_Operator)) %>%
  left_join(chl_sum, by = c('Sample_ID', 'site', 'date')) %>%
  left_join(pmax_data, by = c('Sample_ID', 'site', 'date')) %>%
  select(-Comments) %>%
  left_join(nutrients_data, by = c('Sample_ID', 'site', 'date')) %>%
  #select(-Comments) %>%
  left_join(my_emms, by = c('Sample_ID', 'site', 'date')) #%>%
   #select(-Comments)
```

Location and Correspondence of Sampling Locations
================
Curtis C. Bohlen, Casco Bay Estuary Partnership

-   [Load Libraries](#load-libraries)
-   [Load Zooplankton Data](#load-zooplankton-data)
    -   [All Locations](#all-locations)
    -   [Matching PhytoPlankton
        Locations](#matching-phytoplankton-locations)

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

\#Introduction This notebook generates CSV files to import into ArcGIS ,
and a simple crosswalk table to facilitate consistent matching of
zooplankton data and phytoplankton data. The two organizations
collecting data used somewhat different site and sample naming
conventions. This is my effort to standardize conversion.

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
```

# Load Zooplankton Data

We load this data first because our goal is to translate from the
Zooplankton data sample coding to the phytoplankton data coding scheme,
as the phytoplankton data has a clear and consistent sample coding
scheme.

Also, the one clue I have to the “translation” is in the first two
columns of the “Phyto\_Environment” tab in the Zooplankton data

But the match is not 100%, so we need to figure out how to handle the
non-matches.

``` r
zoopl_sample_codes <- read_excel("Seanet ALL DATA FILE.xlsx", 
    sheet = "Phyto_Enviromental", range = "a1:h53") %>%
  select(-c(year, month, `time h`)) %>%
  mutate(mock_id = paste0('SNT_',date, '_', bigelow_id )) %>%
  relocate(mock_id)
```

## All Locations

I use the `first()` function here because I notice some inconsistencies
in the latitude and longitude reported from some sites. It looks like
latitude and longitude were copied incorrectly at least once.
Specifically, RAM and Wood appear to have been mislabeled at least once.

Note that I add `estuary' and`estuary\_order\` values here to facilitate
later models.

``` r
all_sites <- zoopl_sample_codes %>%
  select(-date, -mock_id) %>%
  group_by(ID) %>%
  summarize(bigelow_id = first(bigelow_id),
            latitude = first(`latitude decdeg`),
            longitude = -first(`longitude decdeg`),
            is_phyto = ! is.na(bigelow_id)) %>%
  mutate(estuary = case_when(grepl('BRE', ID) ~ 'Bagaduce',
                             grepl('DRE', ID) ~ 'Damariscotta',
                             grepl('NME', ID) ~ 'New Meadows',
                             TRUE         ~ 'Saco'),
         estuary_order = case_when(estuary != 'Saco' ~ as.numeric(substr(ID, nchar(ID), nchar(ID))),
                                   ID == '3/4 BUOY' ~ 3,
                                   ID == 'Wood' ~ 2,
                                   ID == 'RAM' ~ 1)) %>%
  relocate(estuary, estuary_order, .after = 'bigelow_id') 
#> Warning in eval_tidy(pair$rhs, env = default_env): NAs introduced by coercion
all_sites
#> # A tibble: 16 x 7
#>    ID       bigelow_id estuary      estuary_order latitude longitude is_phyto
#>    <chr>    <chr>      <chr>                <dbl>    <dbl>     <dbl> <lgl>   
#>  1 3/4 BUOY BUOY       Saco                     3     43.5     -70.3 TRUE    
#>  2 BRE 1    <NA>       Bagaduce                 1     44.4     -68.7 FALSE   
#>  3 BRE 2    <NA>       Bagaduce                 2     44.4     -68.7 FALSE   
#>  4 BRE 4    <NA>       Bagaduce                 4     44.4     -68.8 FALSE   
#>  5 BRE 5    <NA>       Bagaduce                 5     44.4     -68.8 FALSE   
#>  6 DRE 1    <NA>       Damariscotta             1     44.0     -69.5 FALSE   
#>  7 DRE 2    LOBO1      Damariscotta             2     44       -69.5 TRUE    
#>  8 DRE 3    <NA>       Damariscotta             3     44.0     -69.6 FALSE   
#>  9 DRE 4    LOBO2      Damariscotta             4     43.9     -69.6 TRUE    
#> 10 DRE 5    UNE3       Damariscotta             5     43.8     -69.6 TRUE    
#> 11 NME CB4  <NA>       New Meadows              4     43.9     -69.9 FALSE   
#> 12 NME CBO1 <NA>       New Meadows              1     NA        NA   FALSE   
#> 13 NME CBO2 <NA>       New Meadows              2     43.8     -69.9 FALSE   
#> 14 NME CBO3 <NA>       New Meadows              3     NA        NA   FALSE   
#> 15 RAM      RAM        Saco                     1     43.5     -70.4 TRUE    
#> 16 Wood     WOOD       Saco                     2     43.5     -70.3 TRUE
```

We need the `na = ''` code to facilitate reading this file into ArcGIS.
Otherwise, the code outputs “NA” for missing values, which ArcGIS reads
as a character value.

``` r
write.csv(all_sites, "all_sites.csv", na = '')
```

## Matching PhytoPlankton Locations

I base latitude and longitude on the first reported locations, as RAM
and Wood appear to have been mislabeled at least once.

``` r
site_crosswalk <- zoopl_sample_codes %>%
  select(-date, -mock_id) %>%
  group_by(ID) %>%
  summarize(bigelow_id = first(bigelow_id),
            latitude = first(`latitude decdeg`),
            longitude = -first(`longitude decdeg`)) %>%
  filter(! is.na(bigelow_id))
site_crosswalk
#> # A tibble: 6 x 4
#>   ID       bigelow_id latitude longitude
#>   <chr>    <chr>         <dbl>     <dbl>
#> 1 3/4 BUOY BUOY           43.5     -70.3
#> 2 DRE 2    LOBO1          44       -69.5
#> 3 DRE 4    LOBO2          43.9     -69.6
#> 4 DRE 5    UNE3           43.8     -69.6
#> 5 RAM      RAM            43.5     -70.4
#> 6 Wood     WOOD           43.5     -70.3
```

``` r
write.csv(site_crosswalk, "site_crosswalk.csv")
```

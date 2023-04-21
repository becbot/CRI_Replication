CRI_Replication_Git
================
Rebecca
2023-04-21

# Required Packages

\#Session Paramters

``` r
options(scipen=999)
```

# Import Data

# Descriptives

## Open Science

``` r
freq(dat_unique$open_access)
```

    ## Frequencies  
    ## dat_unique$open_access  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0    168     56.76          56.76     56.76          56.76
    ##           1    128     43.24         100.00     43.24         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$ethics)
```

    ## Frequencies  
    ## dat_unique$ethics  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0    122     41.22          41.22     41.22          41.22
    ##           1    172     58.11          99.32     58.11          99.32
    ##        N.A.      2      0.68         100.00      0.68         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$consent)
```

    ## Frequencies  
    ## dat_unique$consent  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0     92     31.08          31.08     31.08          31.08
    ##           1    204     68.92         100.00     68.92         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$prereg)
```

    ## Frequencies  
    ## dat_unique$prereg  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0    286     96.62          96.62     96.62          96.62
    ##           1     10      3.38         100.00      3.38         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$disciplines_availability)
```

    ## Frequencies  
    ## dat_unique$disciplines_availability  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0     80     27.03          27.03     27.03          27.03
    ##         0.5     67     22.64          49.66     22.64          49.66
    ##           1    149     50.34         100.00     50.34         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$open_materials)
```

    ## Frequencies  
    ## dat_unique$open_materials  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0    118     39.86          39.86     39.86          39.86
    ##         0.5     70     23.65          63.51     23.65          63.51
    ##           1    108     36.49         100.00     36.49         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$open_measures)
```

    ## Frequencies  
    ## dat_unique$open_measures  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0     84     28.38          28.38     28.38          28.38
    ##         0.5     40     13.51          41.89     13.51          41.89
    ##           1    172     58.11         100.00     58.11         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$open_code)
```

    ## Frequencies  
    ## dat_unique$open_code  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0    262     88.51          88.51     88.51          88.51
    ##         0.5      4      1.35          89.86      1.35          89.86
    ##           1     13      4.39          94.26      4.39          94.26
    ##        N.A.     17      5.74         100.00      5.74         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$code_repository)
```

    ## Frequencies  
    ## dat_unique$code_repository  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0    205     69.26          69.26     69.26          69.26
    ##         0.5      1      0.34          69.59      0.34          69.59
    ##           1     13      4.39          73.99      4.39          73.99
    ##        N.A.     23      7.77          81.76      7.77          81.76
    ##        N.R.     54     18.24         100.00     18.24         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$open_materials)
```

    ## Frequencies  
    ## dat_unique$open_materials  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0    118     39.86          39.86     39.86          39.86
    ##         0.5     70     23.65          63.51     23.65          63.51
    ##           1    108     36.49         100.00     36.49         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$open_data)
```

    ## Frequencies  
    ## dat_unique$open_data  
    ## Type: Character  
    ## 
    ##                Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ------------ ------ --------- -------------- --------- --------------
    ##            0    246     83.11          83.11     83.11          83.11
    ##          0.5     13      4.39          87.50      4.39          87.50
    ##            1     25      8.45          95.95      8.45          95.95
    ##         N.A.      4      1.35          97.30      1.35          97.30
    ##       UpnReq      8      2.70         100.00      2.70         100.00
    ##         <NA>      0                               0.00         100.00
    ##        Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$study_repository)
```

    ## Frequencies  
    ## dat_unique$study_repository  
    ## Type: Character  
    ## 
    ##                     Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------------- ------ --------- -------------- --------- --------------
    ##         ActiveRep     18      6.08           6.08      6.08           6.08
    ##       InactiveRep      5      1.69           7.77      1.69           7.77
    ##            InText      2      0.68           8.45      0.68           8.45
    ##              N.A.     21      7.09          15.54      7.09          15.54
    ##              N.R.    224     75.68          91.22     75.68          91.22
    ##              Supp     11      3.72          94.93      3.72          94.93
    ##            UpnReq     15      5.07         100.00      5.07         100.00
    ##              <NA>      0                               0.00         100.00
    ##             Total    296    100.00         100.00    100.00         100.00

## Population

``` r
dat$mean_age_CODED <- as.numeric(dat$mean_age_CODED)
```

    ## Warning: NAs introduced by coercion

``` r
dat$min_age_CODED <- as.numeric(dat$min_age_CODED)
```

    ## Warning: NAs introduced by coercion

``` r
dat$max_age_CODED <- as.numeric(dat$max_age_CODED)
```

    ## Warning: NAs introduced by coercion

``` r
skim(dat$mean_age_CODED)
```

|                                                  |                     |
|:-------------------------------------------------|:--------------------|
| Name                                             | dat\$mean_age_CODED |
| Number of rows                                   | 325                 |
| Number of columns                                | 1                   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |                     |
| Column type frequency:                           |                     |
| numeric                                          | 1                   |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |                     |
| Group variables                                  | None                |

Data summary

**Variable type: numeric**

| skim_variable | n_missing | complete_rate | mean |   sd |   p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|-----:|-----:|-----:|-----:|-----:|-----:|-----:|:------|
| data          |       149 |          0.54 | 7.69 | 3.08 | 0.77 | 5.49 | 7.65 | 9.31 | 17.4 | ▂▇▇▂▁ |

``` r
stat.desc(dat$mean_age_CODED)
```

    ##      nbr.val     nbr.null       nbr.na          min          max        range 
    ##  176.0000000    0.0000000  149.0000000    0.7700000   17.4000000   16.6300000 
    ##          sum       median         mean      SE.mean CI.mean.0.95          var 
    ## 1354.1983333    7.6500000    7.6943087    0.2319608    0.4578008    9.4698246 
    ##      std.dev     coef.var 
    ##    3.0773080    0.3999460

``` r
freq(dat_unique$application_CODED)
```

    ## Frequencies  
    ## dat_unique$application_CODED  
    ## Type: Character  
    ## 
    ##                    Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ---------------- ------ --------- -------------- --------- --------------
    ##       CREATIVITY      5      1.69           1.69      1.69           1.69
    ##           DESIGN      1      0.34           2.03      0.34           2.03
    ##              EDU    114     38.51          40.54     38.51          40.54
    ##           HEALTH      7      2.36          42.91      2.36          42.91
    ##             HOME      4      1.35          44.26      1.35          44.26
    ##           MENTAL     12      4.05          48.31      4.05          48.31
    ##            PSYCH     28      9.46          57.77      9.46          57.77
    ##           SOCIAL     47     15.88          73.65     15.88          73.65
    ##        TECHNICAL      4      1.35          75.00      1.35          75.00
    ##             THER     74     25.00         100.00     25.00         100.00
    ##             <NA>      0                               0.00         100.00
    ##            Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$population)
```

    ## Frequencies  
    ## dat_unique$population  
    ## Type: Character  
    ## 
    ##                                 Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------------------------- ------ --------- -------------- --------- --------------
    ##                          ADHD      2      0.68           0.68      0.68           0.68
    ##                           ASD     57     19.26          19.93     19.26          19.93
    ##         behavioural disorders      1      0.34          20.27      0.34          20.27
    ##                cerebral palsy      3      1.01          21.28      1.01          21.28
    ##          cognitively impaired      3      1.01          22.30      1.01          22.30
    ##               cystic fibrosis      1      0.34          22.64      0.34          22.64
    ##                          deaf      2      0.68          23.31      0.68          23.31
    ##                      diabetes      1      0.34          23.65      0.34          23.65
    ##                 down syndrome      1      0.34          23.99      0.34          23.99
    ##                    dysgraphia      1      0.34          24.32      0.34          24.32
    ##                  hospitalized      2      0.68          25.00      0.68          25.00
    ##             language disorder      1      0.34          25.34      0.34          25.34
    ##                         mixed     21      7.09          32.43      7.09          32.43
    ##       multiple irregularities      7      2.36          34.80      2.36          34.80
    ##                  neurotypical    192     64.86          99.66     64.86          99.66
    ##           physical disability      1      0.34         100.00      0.34         100.00
    ##                          <NA>      0                               0.00         100.00
    ##                         Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat_unique$country_CODED)
```

    ## Frequencies  
    ## dat_unique$country_CODED  
    ## Type: Character  
    ## 
    ##                      Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ------------------ ------ --------- -------------- --------- --------------
    ##       ,USA,CAN,EUR      1      0.34           0.34      0.34           0.34
    ##                ARE      2      0.68           1.01      0.68           1.01
    ##                AUS     13      4.39           5.41      4.39           5.41
    ##                BEL      4      1.35           6.76      1.35           6.76
    ##                BGD      2      0.68           7.43      0.68           7.43
    ##                BRA      6      2.03           9.46      2.03           9.46
    ##                CAN      4      1.35          10.81      1.35          10.81
    ##                CHE      6      2.03          12.84      2.03          12.84
    ##                CHN      1      0.34          13.18      0.34          13.18
    ##                COL      3      1.01          14.19      1.01          14.19
    ##                DEU      8      2.70          16.89      2.70          16.89
    ##                DNK      2      0.68          17.57      0.68          17.57
    ##                ECU      7      2.36          19.93      2.36          19.93
    ##                ESP      3      1.01          20.95      1.01          20.95
    ##                FIN      3      1.01          21.96      1.01          21.96
    ##                FRA      5      1.69          23.65      1.69          23.65
    ##                GBR     11      3.72          27.36      3.72          27.36
    ##                GRC      9      3.04          30.41      3.04          30.41
    ##            GRC,ESP      1      0.34          30.74      0.34          30.74
    ##                HKG      4      1.35          32.09      1.35          32.09
    ##                HRV      1      0.34          32.43      0.34          32.43
    ##                IND      3      1.01          33.45      1.01          33.45
    ##                IRN      8      2.70          36.15      2.70          36.15
    ##                ISR      1      0.34          36.49      0.34          36.49
    ##                ITA     25      8.45          44.93      8.45          44.93
    ##            ITA,PRT      1      0.34          45.27      0.34          45.27
    ##                JPN     19      6.42          51.69      6.42          51.69
    ##                KAZ      4      1.35          53.04      1.35          53.04
    ##                LKA      1      0.34          53.38      0.34          53.38
    ##                MAR      1      0.34          53.72      0.34          53.72
    ##                MEX      4      1.35          55.07      1.35          55.07
    ##                MYS      1      0.34          55.41      0.34          55.41
    ##               N.R.     18      6.08          61.49      6.08          61.49
    ##                NLD     32     10.81          72.30     10.81          72.30
    ##                NOR      4      1.35          73.65      1.35          73.65
    ##                PAK      7      2.36          76.01      2.36          76.01
    ##                PER      2      0.68          76.69      0.68          76.69
    ##                PHL      1      0.34          77.03      0.34          77.03
    ##                PRT      9      3.04          80.07      3.04          80.07
    ##                QAT      2      0.68          80.74      0.68          80.74
    ##                ROU      2      0.68          81.42      0.68          81.42
    ##                RUS      1      0.34          81.76      0.34          81.76
    ##                SWE      7      2.36          84.12      2.36          84.12
    ##                TUR      4      1.35          85.47      1.35          85.47
    ##                TWN      8      2.70          88.18      2.70          88.18
    ##                USA     34     11.49          99.66     11.49          99.66
    ##                VEN      1      0.34         100.00      0.34         100.00
    ##               <NA>      0                               0.00         100.00
    ##              Total    296    100.00         100.00    100.00         100.00

``` r
freq(dat$age_gender_after_exclusion)
```

    ## Frequencies  
    ## dat$age_gender_after_exclusion  
    ## Type: Character  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           0     27      8.31           8.31      8.31           8.31
    ##           1     79     24.31          32.62     24.31          32.62
    ##        N.A.    149     45.85          78.46     45.85          78.46
    ##        N.R.     70     21.54         100.00     21.54         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    325    100.00         100.00    100.00         100.00

``` r
dat_gender <- dat %>%
  dplyr::select(female_reported_CODED, male_reported_CODED, non_binary_reported_CODED, not_specified_reported_CODED) %>% 
  replace(. == "N.R.", NA)

dat_gender <- sapply(dat_gender, as.numeric)
dat_gender <- as.data.frame(dat_gender)

proportions <- colSums(dat_gender, na.rm = T) / sum(dat_gender, na.rm = T)
proportions
```

    ##        female_reported_CODED          male_reported_CODED 
    ##                 0.4721640091                 0.5267425968 
    ##    non_binary_reported_CODED not_specified_reported_CODED 
    ##                 0.0004555809                 0.0006378132

``` r
dat_mf <- dat_gender %>%
  filter(non_binary_reported_CODED == 0 & not_specified_reported_CODED == 0)

dat_mf$prop <- dat_mf$female_reported_CODED / (dat_mf$male_reported_CODED + dat_mf$female_reported_CODED)
stat.desc(dat_mf$prop)
```

    ##      nbr.val     nbr.null       nbr.na          min          max        range 
    ## 226.00000000  14.00000000   1.00000000   0.00000000   1.00000000   1.00000000 
    ##          sum       median         mean      SE.mean CI.mean.0.95          var 
    ##  94.09835215   0.45009579   0.41636439   0.01307534   0.02576578   0.03863796 
    ##      std.dev     coef.var 
    ##   0.19656540   0.47209945

``` r
generate_pie_chart <- function(df, column_name) {
  
  # Create summary table with percentages
  counts <- table(df[, column_name])
  dat_counts <- data.frame(counts)
  dat_counts$percent <- round(100 * dat_counts$Freq / sum(dat_counts$Freq), 2)
  
  # Order factor levels based on percentages
  dat_counts$Var1 <- factor(dat_counts$Var1, levels = dat_counts$Var1[order(dat_counts$percent, decreasing = TRUE)])
  
  # Create pie chart with ggplot2
  pie_chart <- ggplot(dat_counts, aes(x="", y=percent, fill=Var1)) + 
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    labs(fill=column_name) +
    scale_fill_viridis_d(option = "D", direction = 1) +
    theme_void()
  
  return(pie_chart)
}
```

### Population Graphs

``` r
generate_pie_chart(dat_unique, "population")
```

![](CRI_Replication_Git_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggsave("gg_population.pdf", height = 5)
```

    ## Saving 7 x 5 in image

``` r
generate_pie_chart(dat_unique, "application_CODED")
```

![](CRI_Replication_Git_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
ggsave("gg_application.pdf", height = 5)
```

    ## Saving 7 x 5 in image

``` r
dat$age_round <- round(dat$mean_age_CODED)

dat$min_age_round <- round(dat$min_age_CODED)
dat$max_age_round <- round(dat$max_age_CODED)


# Filter out rows with non-numeric values in min_age and max_age columns
dat_age <- dat %>%
  filter(!is.na(as.numeric(min_age_round)) & !is.na(as.numeric(max_age_round))) %>%
  mutate(min_age = as.numeric(min_age_round), max_age = as.numeric(max_age_round)) %>%
  dplyr::select(ID, expt_num, mean_age_CODED, min_age_round, max_age_round) %>%
  mutate(ID_unique = paste0(ID, "_", expt_num))

# Create a sequence of ages from min_age to max_age for each row
dat_age_TOTAL <- dat_age %>%
  mutate(ages = map2(min_age_round, max_age_round, ~seq(.x, .y, by = 1))) %>%
  unnest(ages)

# Count the occurrences of each age
result <- dat_age_TOTAL %>%
  count(ages)

# Rename the columns
result <- result %>%
  rename(age = ages, count = n)

# Create histogram
gg_age <- ggplot(result, aes(x = age, y = count)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Age", y = "Number of studies including this age") +
    theme(
        panel.grid.major = element_line(color = "#E5E5E5", linetype = "solid", linewidth = 0.3),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9))

gg_age
```

![](CRI_Replication_Git_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggsave("gg_age.pdf", height = 5)
```

    ## Saving 7 x 5 in image

``` r
dat_age <- dat_age %>%
  mutate(age_range = paste(min_age_round, max_age_round, sep = "-"))

age_range <- dat_age %>%
  mutate(age_range = paste(min_age_round, max_age_round, sep = "-")) %>%
  group_by(age_range) %>%
  summarise(study_count = n()) %>%
  mutate(min_age = as.numeric(str_extract(age_range, "^\\d+")),
         max_age = as.numeric(str_extract(age_range, "\\d+$"))) %>%
  arrange(min_age, max_age - min_age) %>%
  mutate(age_range = factor(age_range, levels = age_range))

ggplot(age_range, aes(x = min_age, xend = max_age, y = age_range, yend = age_range)) +
  geom_segment(aes(color = study_count), size = 2) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen", 
                       breaks = seq(min(age_range$study_count), max(age_range$study_count), by = 1),
                       labels = function(x) round(x)) + 
  scale_x_continuous(breaks = seq(min(age_range$min_age), max(age_range$max_age), by = 1)) +
  labs(x = "Age", y = "Age Range", colour = "Number of\nStudies") +
    theme(legend.position = "right",
        legend.title = element_text(face = "bold", size = 9),
        axis.title.y = element_blank(), 
        panel.grid.major = element_line(color = "#E5E5E5", linetype = "solid", linewidth = 0.3),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "bold", size = 9))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.

![](CRI_Replication_Git_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggsave("gg_range.pdf", height = 10)
```

    ## Saving 7 x 10 in image

## Robots / Interaction

``` r
freq(dat$robot_number)
```

    ## Frequencies  
    ## dat$robot_number  
    ## Type: Integer  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           1    288     88.62          88.62     88.62          88.62
    ##           2     21      6.46          95.08      6.46          95.08
    ##           3      9      2.77          97.85      2.77          97.85
    ##           4      4      1.23          99.08      1.23          99.08
    ##           5      2      0.62          99.69      0.62          99.69
    ##          13      1      0.31         100.00      0.31         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    325    100.00         100.00    100.00         100.00

``` r
freq(dat$operation_CODED)
```

    ## Frequencies  
    ## dat$operation_CODED  
    ## Type: Character  
    ## 
    ##                 Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ------------- ------ --------- -------------- --------- --------------
    ##             A     62     19.08          19.08     19.08          19.08
    ##         A,WoZ      2      0.62          19.69      0.62          19.69
    ##         mixed      4      1.23          20.92      1.23          20.92
    ##          N.A.     21      6.46          27.38      6.46          27.38
    ##          N.R.     68     20.92          48.31     20.92          48.31
    ##            PS     46     14.15          62.46     14.15          62.46
    ##            SA     20      6.15          68.62      6.15          68.62
    ##            TO     25      7.69          76.31      7.69          76.31
    ##           WoZ     76     23.38          99.69     23.38          99.69
    ##       WoZ,A,A      1      0.31         100.00      0.31         100.00
    ##          <NA>      0                               0.00         100.00
    ##         Total    325    100.00         100.00    100.00         100.00

``` r
dat_operation <- dat %>%
  separate_rows(operation_CODED, sep = ",") %>%
  dplyr::select(operation_CODED) %>%
  mutate(operation_CODED = as.factor(operation_CODED))

freq(dat_operation$operation_CODED)
```

    ## Frequencies  
    ## dat_operation$operation_CODED  
    ## Type: Factor  
    ## 
    ##               Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------- ------ --------- -------------- --------- --------------
    ##           A     66     20.06          20.06     20.06          20.06
    ##       mixed      4      1.22          21.28      1.22          21.28
    ##        N.A.     21      6.38          27.66      6.38          27.66
    ##        N.R.     68     20.67          48.33     20.67          48.33
    ##          PS     46     13.98          62.31     13.98          62.31
    ##          SA     20      6.08          68.39      6.08          68.39
    ##          TO     25      7.60          75.99      7.60          75.99
    ##         WoZ     79     24.01         100.00     24.01         100.00
    ##        <NA>      0                               0.00         100.00
    ##       Total    329    100.00         100.00    100.00         100.00

``` r
operation_counts <- table(dat_operation$operation_CODED)

operation_dat <- data.frame(operation_counts)
operation_dat$percent <- round(100 * operation_dat$Freq / sum(operation_dat$Freq), 2)

operation_dat$Var1 <- factor(operation_dat$Var1, levels = operation_dat$Var1[order(operation_dat$percent, decreasing = TRUE)])

ggplot(operation_dat, aes(x="", y=percent, fill=Var1)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(fill="Application") +
  scale_fill_viridis_d(option = "D", direction = 1) +
  theme_void()
```

![](CRI_Replication_Git_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggsave("gg_operation.pdf", height = 5)
```

    ## Saving 7 x 5 in image

``` r
generate_pie_chart(dat_unique, "robot_interaction")
```

![](CRI_Replication_Git_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
ggsave("gg_interaction.pdf", height = 5)
```

    ## Saving 7 x 5 in image

``` r
generate_pie_chart <- function(df, column_name) {
  
  # Create summary table with percentages
  counts <- droplevels(table(df[, column_name]))
  dat_counts <- data.frame(counts)
  dat_counts$percent <- round(100 * dat_counts$Freq / sum(dat_counts$Freq), 2)
  
  # Order factor levels based on percentages
  if (any(dat_counts$percent == 0)) {
    message("No observations for some levels of the variable")
    return(ggplot() + theme_void())
  } else {
    dat_counts$Var1 <- factor(dat_counts$Var1, levels = dat_counts$Var1[order(dat_counts$percent, decreasing = TRUE)])
    
    # Create pie chart with ggplot2
    pie_chart <- ggplot(dat_counts, aes(x="", y=percent, fill=Var1)) + 
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta = "y") +
      labs(fill=column_name) +
      scale_fill_viridis_d(option = "D", direction = 1) +
      theme_void()
    
    return(pie_chart)
  }
}
```

``` r
dat_robots <- dat %>%
  separate_rows(robot_model, sep = ",") %>%
  dplyr::select(robot_model)

robot_freq <- freq(dat_robots$robot_model)

value_counts <- dat_robots %>%
  count(robot_model)

# Identify values that appear only once
values_to_replace <- value_counts %>%
  filter(n == 1) %>%
  pull(robot_model)

#Replace values that appear only once with "Other"
dat_robots <- dat_robots %>%
  mutate(robot_CODED = if_else(robot_model %in% values_to_replace, "Other", robot_model))

freq(dat_robots$robot_CODED)
```

    ## Frequencies  
    ## dat_robots$robot_CODED  
    ## Type: Character  
    ## 
    ##                                       Freq   % Valid   % Valid Cum.   % Total   % Total Cum.
    ## ----------------------------------- ------ --------- -------------- --------- --------------
    ##                          Alpha Mini      2      0.50           0.50      0.50           0.50
    ##                   AnRI 1 and AnRI 2      3      0.75           1.25      0.75           1.25
    ##                                 ARC      2      0.50           1.75      0.50           1.75
    ##                                 AV1      2      0.50           2.25      0.50           2.25
    ##                                BB-8      3      0.75           3.00      0.75           3.00
    ##                             Bee-bot      2      0.50           3.50      0.50           3.50
    ##                            Blue-Bot      4      1.00           4.50      1.00           4.50
    ##                                Coji      3      0.75           5.25      0.75           5.25
    ##                               Cozmo     15      3.75           9.00      3.75           9.00
    ##                               Daisy      3      0.75           9.75      0.75           9.75
    ##                              Darwin      3      0.75          10.50      0.75          10.50
    ##                                Dash      2      0.50          11.00      0.50          11.00
    ##                             EMAR V4      2      0.50          11.50      0.50          11.50
    ##                                EMYS      3      0.75          12.25      0.75          12.25
    ##                              Furhat      6      1.50          13.75      1.50          13.75
    ##                          Golden Pup      2      0.50          14.25      0.50          14.25
    ##                                iCub      2      0.50          14.75      0.50          14.75
    ##                                Jibo      5      1.25          16.00      1.25          16.00
    ##                              Kaspar      5      1.25          17.25      1.25          17.25
    ##                     Kebbi Air Robot      3      0.75          18.00      0.75          18.00
    ##                                Kiwi      2      0.50          18.50      0.50          18.50
    ##                       LegoBoost Bot      3      0.75          19.25      0.75          19.25
    ##                                LUCA      2      0.50          19.75      0.50          19.75
    ##                             Matilda      2      0.50          20.25      0.50          20.25
    ##                                mBot      4      1.00          21.25      1.00          21.25
    ##                            MecWilly      2      0.50          21.75      0.50          21.75
    ##                           Mindstorm      5      1.25          23.00      1.25          23.00
    ##                              MiRo-E      2      0.50          23.50      0.50          23.50
    ##                               Misty      2      0.50          24.00      0.50          24.00
    ##                                N.R.      8      2.00          26.00      2.00          26.00
    ##                                 Nao    138     34.50          60.50     34.50          60.50
    ##                           Nao Torso      3      0.75          61.25      0.75          61.25
    ##                               Other     70     17.50          78.75     17.50          78.75
    ##                              Ozobot      4      1.00          79.75      1.00          79.75
    ##                                Paro      2      0.50          80.25      0.50          80.25
    ##                              Pepper     24      6.00          86.25      6.00          86.25
    ##                                Pleo      5      1.25          87.50      1.25          87.50
    ##                      Qobo the snail      3      0.75          88.25      0.75          88.25
    ##                             QTrobot      5      1.25          89.50      1.25          89.50
    ##                          Robosapien      3      0.75          90.25      0.75          90.25
    ##                             Robovie      7      1.75          92.00      1.75          92.00
    ##                                Romo      3      0.75          92.75      0.75          92.75
    ##                              RUBI-6      3      0.75          93.50      0.75          93.50
    ##       SAMBusddy Storytelling Cuddle      2      0.50          94.00      0.50          94.00
    ##                              Skusie      4      1.00          95.00      1.00          95.00
    ##                                Sota      2      0.50          95.50      0.50          95.50
    ##                                Tega      2      0.50          96.00      0.50          96.00
    ##                               Zenbo      4      1.00          97.00      1.00          97.00
    ##                                Zeno      9      2.25          99.25      2.25          99.25
    ##                              Zoomer      3      0.75         100.00      0.75         100.00
    ##                                <NA>      0                               0.00         100.00
    ##                               Total    400    100.00         100.00    100.00         100.00

``` r
robot_counts <- dat_robots %>%
  count(robot_CODED) %>%
  filter(robot_CODED != "N.R.") %>%
  filter(robot_CODED != "Other") %>%
  filter(n >= 5) %>%
  arrange(-n) %>%
  mutate(robot_CODED = factor(robot_CODED, levels = robot_CODED))

gg_robots <- ggplot(robot_counts, aes(x = robot_CODED, y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(x = "Robot", y = "Number of Studies") +
    theme(
        panel.grid.major = element_line(color = "#E5E5E5", linetype = "solid", linewidth = 0.3),
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_text(face = "bold", size = 9),
        axis.title.y = element_text(face = "bold", size = 9)) +
  coord_flip()

gg_robots
```

![](CRI_Replication_Git_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggsave("gg_robots.pdf", height = 5)
```

    ## Saving 7 x 5 in image

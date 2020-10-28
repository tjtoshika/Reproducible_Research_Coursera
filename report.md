---
title: "A descriptive analysis on weather events in the United States that have negative consequences on population health and the economy"
output: 
  html_document: 
    keep_md: yes
---
## Synopsis

In this report, we aim to analyze the impact of different weather events on public health and economy based on the storm database collected from the U.S. National Oceanic and Atmospheric Administration's (NOAA) from 1950 - 2011. We will use the estimates of fatalities, injuries, property and crop damage to decide which types of event are most harmful to the population health and economy. From these data, we found that excessive heat and tornado are most harmful with respect to population health, while flood, drought, and hurricane/typhoon have the greatest economic consequences.

## Data Processing

This script assumes that you have downloaded the data and set the working directory in Rstudio where data is present, if not then set the working directory with setwd() command.


```r
storm_data <- read.csv('repdata_data_StormData.csv.bz2')
dim(storm_data)
```

```
## [1] 902297     37
```

```r
str(storm_data)
```

```
## 'data.frame':	902297 obs. of  37 variables:
##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ BGN_DATE  : chr  "4/18/1950 0:00:00" "4/18/1950 0:00:00" "2/20/1951 0:00:00" "6/8/1951 0:00:00" ...
##  $ BGN_TIME  : chr  "0130" "0145" "1600" "0900" ...
##  $ TIME_ZONE : chr  "CST" "CST" "CST" "CST" ...
##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
##  $ COUNTYNAME: chr  "MOBILE" "BALDWIN" "FAYETTE" "MADISON" ...
##  $ STATE     : chr  "AL" "AL" "AL" "AL" ...
##  $ EVTYPE    : chr  "TORNADO" "TORNADO" "TORNADO" "TORNADO" ...
##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ BGN_AZI   : chr  "" "" "" "" ...
##  $ BGN_LOCATI: chr  "" "" "" "" ...
##  $ END_DATE  : chr  "" "" "" "" ...
##  $ END_TIME  : chr  "" "" "" "" ...
##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ END_AZI   : chr  "" "" "" "" ...
##  $ END_LOCATI: chr  "" "" "" "" ...
##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
##  $ PROPDMGEXP: chr  "K" "K" "K" "K" ...
##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ CROPDMGEXP: chr  "" "" "" "" ...
##  $ WFO       : chr  "" "" "" "" ...
##  $ STATEOFFIC: chr  "" "" "" "" ...
##  $ ZONENAMES : chr  "" "" "" "" ...
##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
##  $ LATITUDE_E: num  3051 0 0 0 0 ...
##  $ LONGITUDE_: num  8806 0 0 0 0 ...
##  $ REMARKS   : chr  "" "" "" "" ...
##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...
```

As shown by the 'dim' command, there are 902297 rows and 37 columns in total. As mentioned, the events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete.We will have a look at this and try to extract the data from years which significantly form the most part of the data.


```r
storm_data$Year <- as.numeric(format(as.Date(storm_data$BGN_DATE, format="%m/%d/%Y"),"%Y"))
hist(storm_data$Year, breaks = 30, xlab = "Year", main  = 'Frequency of data by Year')
```

![](report_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Thus shown by the histogram, we can see that there is significant rise of frequency from year 1995. Therefore, we will use the data starting from year 1995.


```r
data <- storm_data[storm_data$Year >= 1995, ]
dim(data)
```

```
## [1] 681500     38
```

### Question 1 . Across the United States, which types of events are most harmful with respect to population health?

There are 2 columns in the dataset which show the harm caused to the population, 'INJURIES' and 'FATALITIES'.Below are the table of the events(seperate for Injuries and Fatalities) which have caused the most harm.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
data$Total_harm <- data$INJURIES + data$FATALITIES
total_harm <- data %>% group_by(EVTYPE) %>% summarize(Injuries_and_Fatalities = sum(Total_harm))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
total_harm <- total_harm[order(total_harm$Injuries_and_Fatalities, decreasing = TRUE), ]
total_harm <- total_harm[total_harm$Injuries_and_Fatalities > 0 , ]
harmful_events_injuries <- data %>% group_by(EVTYPE) %>% summarize(Injuries = sum(INJURIES))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
harmful_events_fatalities <- data %>% group_by(EVTYPE) %>% summarize(Fatalities = sum(FATALITIES))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
harmful_events_injuries <- harmful_events_injuries[order(harmful_events_injuries$Injuries, decreasing = TRUE), ]
harmful_events_fatalities <- harmful_events_fatalities[order(harmful_events_fatalities$Fatalities, decreasing = TRUE), ]
harmful_events_injuries <- harmful_events_injuries[harmful_events_injuries$Injuries > 0, ]
harmful_events_fatalities <- harmful_events_fatalities[harmful_events_fatalities$Fatalities > 0, ]


head(total_harm, 20)
```

```
## # A tibble: 20 x 2
##    EVTYPE             Injuries_and_Fatalities
##    <chr>                                <dbl>
##  1 TORNADO                              23310
##  2 EXCESSIVE HEAT                        8428
##  3 FLOOD                                 7192
##  4 LIGHTNING                             5360
##  5 TSTM WIND                             3871
##  6 HEAT                                  2954
##  7 FLASH FLOOD                           2668
##  8 THUNDERSTORM WIND                     1557
##  9 WINTER STORM                          1493
## 10 HURRICANE/TYPHOON                     1339
## 11 HIGH WIND                             1334
## 12 WILDFIRE                               986
## 13 HAIL                                   926
## 14 HEAVY SNOW                             866
## 15 FOG                                    779
## 16 RIP CURRENT                            587
## 17 WILD/FOREST FIRE                       557
## 18 RIP CURRENTS                           501
## 19 THUNDERSTORM WINDS                     482
## 20 HEAT WAVE                              469
```

```r
head(harmful_events_injuries, 20)
```

```
## # A tibble: 20 x 2
##    EVTYPE             Injuries
##    <chr>                 <dbl>
##  1 TORNADO               21765
##  2 FLOOD                  6769
##  3 EXCESSIVE HEAT         6525
##  4 LIGHTNING              4631
##  5 TSTM WIND              3630
##  6 HEAT                   2030
##  7 FLASH FLOOD            1734
##  8 THUNDERSTORM WIND      1426
##  9 WINTER STORM           1298
## 10 HURRICANE/TYPHOON      1275
## 11 HIGH WIND              1093
## 12 HAIL                    916
## 13 WILDFIRE                911
## 14 HEAVY SNOW              751
## 15 FOG                     718
## 16 WILD/FOREST FIRE        545
## 17 THUNDERSTORM WINDS      444
## 18 DUST STORM              420
## 19 WINTER WEATHER          398
## 20 BLIZZARD                385
```

```r
head(harmful_events_fatalities, 20)
```

```
## # A tibble: 20 x 2
##    EVTYPE                  Fatalities
##    <chr>                        <dbl>
##  1 EXCESSIVE HEAT                1903
##  2 TORNADO                       1545
##  3 FLASH FLOOD                    934
##  4 HEAT                           924
##  5 LIGHTNING                      729
##  6 FLOOD                          423
##  7 RIP CURRENT                    360
##  8 HIGH WIND                      241
##  9 TSTM WIND                      241
## 10 AVALANCHE                      223
## 11 RIP CURRENTS                   204
## 12 WINTER STORM                   195
## 13 HEAT WAVE                      161
## 14 THUNDERSTORM WIND              131
## 15 EXTREME COLD                   126
## 16 EXTREME COLD/WIND CHILL        125
## 17 HEAVY SNOW                     115
## 18 STRONG WIND                    103
## 19 HIGH SURF                       99
## 20 COLD/WIND CHILL                 95
```

### Question 2. Across the United States, which types of events have the greatest economic consequences?

There are 4 columns in the dataset which tells about the economic consequences: 
1. PROPDMG - Filled with a number which should be multiplied by the number signified by the character in the PROPDMGEXP column 
2. PROPDMGEXP - Filled with either one of the four entries where "H" for hundreds, “K” for thousands, “M” for millions, and “B” for billions
3. CROPDMG - Filled with a number which should be multiplied by the number signified by the character in the cROPDMGEXP column
4. CROPDMGEXP -  Filled with either one of the four entries where "H"for hundreds, “K” for thousands, “M” for millions, and “B” for billions

Using the information given above, 2 different datasets are created for Property damage and Crop damage. 
At last, 20 top values of Event type with the damage associated with them are shown.


```r
data$PROPDMGEXP <- toupper(data$PROPDMGEXP)
data$CROPDMGEXP <- toupper(data$CROPDMGEXP)

new_data <- data %>% filter(PROPDMGEXP == 'H'| PROPDMGEXP == 'K' | PROPDMGEXP == 'M' | PROPDMGEXP == 'B') %>% select(EVTYPE,PROPDMG, PROPDMGEXP)
new_data <- new_data %>% mutate(Prop_damage = ifelse(PROPDMGEXP == 'H', PROPDMG * 100,ifelse(PROPDMGEXP == 'K', PROPDMG * 1000,ifelse(PROPDMGEXP == 'M', PROPDMG * 1000000,ifelse(PROPDMGEXP == 'B', PROPDMG * 1000000000,0)))))

new_data_1 <- data %>% filter(CROPDMGEXP == 'H'| CROPDMGEXP == 'K' | CROPDMGEXP == 'M' | CROPDMGEXP == 'B') %>% select(EVTYPE,CROPDMG, CROPDMGEXP)
new_data_1 <- new_data_1 %>% mutate(Crop_damage = ifelse(CROPDMGEXP == 'H', CROPDMG * 100,ifelse(CROPDMGEXP == 'K', CROPDMG * 1000,ifelse(CROPDMGEXP == 'M', CROPDMG * 1000000,ifelse(CROPDMGEXP == 'B', CROPDMG * 1000000000,0)))))

new_data <- new_data %>% group_by(EVTYPE) %>% summarize(Prop_damage = sum(Prop_damage))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
new_data_1 <- new_data_1 %>% group_by(EVTYPE) %>% summarize(Crop_damage = sum(Crop_damage))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
new_data <- new_data[order(new_data$Prop_damage, decreasing = T), c('EVTYPE', "Prop_damage")]
new_data_1 <- new_data_1[order(new_data_1$Crop_damage, decreasing = T), c('EVTYPE', 'Crop_damage')]

head(new_data, 20)
```

```
## # A tibble: 20 x 2
##    EVTYPE                     Prop_damage
##    <chr>                            <dbl>
##  1 FLOOD                     144022037050
##  2 HURRICANE/TYPHOON          69305840000
##  3 STORM SURGE                43193536000
##  4 TORNADO                    24925719460
##  5 FLASH FLOOD                15365932310
##  6 HAIL                       15045721820
##  7 HURRICANE                  11812819010
##  8 TROPICAL STORM              7653335550
##  9 HIGH WIND                   5259785360
## 10 WILDFIRE                    4759064000
## 11 STORM SURGE/TIDE            4641188000
## 12 TSTM WIND                   4482361440
## 13 ICE STORM                   3643555810
## 14 THUNDERSTORM WIND           3399281740
## 15 HURRICANE OPAL              3172846000
## 16 WILD/FOREST FIRE            3001812500
## 17 HEAVY RAIN/SEVERE WEATHER   2500000000
## 18 WINTER STORM                1538047250
## 19 SEVERE THUNDERSTORM         1200310000
## 20 DROUGHT                     1046106000
```

```r
head(new_data_1,20)
```

```
## # A tibble: 20 x 2
##    EVTYPE            Crop_damage
##    <chr>                   <dbl>
##  1 DROUGHT           13922066000
##  2 FLOOD              5422810400
##  3 HURRICANE          2741410000
##  4 HAIL               2614127050
##  5 HURRICANE/TYPHOON  2607872800
##  6 FLASH FLOOD        1343915000
##  7 EXTREME COLD       1292473000
##  8 FROST/FREEZE       1094086000
##  9 HEAVY RAIN          728399800
## 10 TROPICAL STORM      677836000
## 11 HIGH WIND           633561300
## 12 TSTM WIND           553947350
## 13 EXCESSIVE HEAT      492402000
## 14 THUNDERSTORM WIND   414354000
## 15 HEAT                401411500
## 16 FREEZE              396225000
## 17 TORNADO             296595610
## 18 WILDFIRE            295472800
## 19 DAMAGING FREEZE     262100000
## 20 EXCESSIVE WETNESS   142000000
```


## Results

Below are the results obtained from the above analysis:



Total number of Injuries and Fatalities caused by Weather - 


```
##                EVTYPE Injuries_and_Fatalities
## 1             TORNADO                   23310
## 2      EXCESSIVE HEAT                    8428
## 3               FLOOD                    7192
## 4           LIGHTNING                    5360
## 5           TSTM WIND                    3871
## 6                HEAT                    2954
## 7         FLASH FLOOD                    2668
## 8   THUNDERSTORM WIND                    1557
## 9        WINTER STORM                    1493
## 10  HURRICANE/TYPHOON                    1339
## 11          HIGH WIND                    1334
## 12           WILDFIRE                     986
## 13               HAIL                     926
## 14         HEAVY SNOW                     866
## 15                FOG                     779
## 16        RIP CURRENT                     587
## 17   WILD/FOREST FIRE                     557
## 18       RIP CURRENTS                     501
## 19 THUNDERSTORM WINDS                     482
## 20          HEAT WAVE                     469
```

Events which caused the highest number of Injuries - 


```
##                EVTYPE Injuries
## 1             TORNADO    21765
## 2               FLOOD     6769
## 3      EXCESSIVE HEAT     6525
## 4           LIGHTNING     4631
## 5           TSTM WIND     3630
## 6                HEAT     2030
## 7         FLASH FLOOD     1734
## 8   THUNDERSTORM WIND     1426
## 9        WINTER STORM     1298
## 10  HURRICANE/TYPHOON     1275
## 11          HIGH WIND     1093
## 12               HAIL      916
## 13           WILDFIRE      911
## 14         HEAVY SNOW      751
## 15                FOG      718
## 16   WILD/FOREST FIRE      545
## 17 THUNDERSTORM WINDS      444
## 18         DUST STORM      420
## 19     WINTER WEATHER      398
## 20           BLIZZARD      385
```
Events which caused the highest number of Fatalities - 


```
##                     EVTYPE Fatalities
## 1           EXCESSIVE HEAT       1903
## 2                  TORNADO       1545
## 3              FLASH FLOOD        934
## 4                     HEAT        924
## 5                LIGHTNING        729
## 6                    FLOOD        423
## 7              RIP CURRENT        360
## 8                HIGH WIND        241
## 9                TSTM WIND        241
## 10               AVALANCHE        223
## 11            RIP CURRENTS        204
## 12            WINTER STORM        195
## 13               HEAT WAVE        161
## 14       THUNDERSTORM WIND        131
## 15            EXTREME COLD        126
## 16 EXTREME COLD/WIND CHILL        125
## 17              HEAVY SNOW        115
## 18             STRONG WIND        103
## 19               HIGH SURF         99
## 20         COLD/WIND CHILL         95
```

And the following is the graph for Total(Fatalities and Injuries) caused by the weather:


```r
library(ggplot2)
ggplot(data=total_harm, aes(x=EVTYPE, y = Injuries_and_Fatalities)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + labs(x = 'Event Type', y = 'Total number of Fatalities and Injuries', title = 'Total Fatalities and Injuries by Weather in the U.S.\n from 1995 - 2011')
```

![](report_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


Property Damage caused by weather - 

```
## # A tibble: 10 x 2
##    EVTYPE             Prop_damage
##  * <chr>                    <dbl>
##  1 FLOOD             144022037050
##  2 HURRICANE/TYPHOON  69305840000
##  3 STORM SURGE        43193536000
##  4 TORNADO            24925719460
##  5 FLASH FLOOD        15365932310
##  6 HAIL               15045721820
##  7 HURRICANE          11812819010
##  8 TROPICAL STORM      7653335550
##  9 HIGH WIND           5259785360
## 10 WILDFIRE            4759064000
```

Crop Damage  caused by weather- 

```
## # A tibble: 10 x 2
##    EVTYPE            Crop_damage
##  * <chr>                   <dbl>
##  1 DROUGHT           13922066000
##  2 FLOOD              5422810400
##  3 HURRICANE          2741410000
##  4 HAIL               2614127050
##  5 HURRICANE/TYPHOON  2607872800
##  6 FLASH FLOOD        1343915000
##  7 EXTREME COLD       1292473000
##  8 FROST/FREEZE       1094086000
##  9 HEAVY RAIN          728399800
## 10 TROPICAL STORM      677836000
```


```r
a <- ggplot(data=new_data, aes(x=EVTYPE, y = Prop_damage)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + labs(x = 'Event Type', y = 'Cost of Property Damage in Dollars', title  = 'Property Damage by Weather\n in the U.S.\n from 1995 - 2011') 
b <- ggplot(data=new_data_1, aes(x=EVTYPE, y = Crop_damage)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 45, hjust=1)) + labs(x = 'Event Type', y = 'Cost of Crop Damage in Dollars', title = 'Crop Damage by Weather\n in the U.S.\n from 1995 - 2011')
library(gridExtra)
a <- ggplotGrob(a)
b <- ggplotGrob(b)
grid.arrange(a,b, ncol = 2)
```

![](report_files/figure-html/unnamed-chunk-13-1.png)<!-- -->





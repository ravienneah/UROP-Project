Ravienne UROP - Zoe’s Code
================
Zoe
2025-03-09





> *Hi Ravienne! This is an R Notebook document. You don’t have to use
> this format, but I like to! R Notebook is basically a way of
> presenting your code in a final document which is easier to read and
> can be easily edited within R.*

#### Loading packages

tidyverse - includes dplyr

forcats - i added this bc i was having trouble with factors

#### Here’s where I loaded my data locally

``` r
load(
  "~/Desktop/QPL/urop_ravienne/ICPSR UROP Data/ICPSR_21600 4/DS0005/21600-0005-Data.rda"
)
load(
  "~/Desktop/QPL/urop_ravienne/ICPSR UROP Data/ICPSR_21600/DS0001/21600-0001-Data.rda"
)
load(
  "~/Desktop/QPL/urop_ravienne/ICPSR UROP Data/ICPSR_21600 8/DS0022/21600-0022-Data.rda"
)
```

## **Ravienne’s code as of 03/09/25**

#### *will not evaluate in knitted doc*

``` r
#selecting the variables from each wave and organizing them
library(dplyr)
w1vars <- da21600.0001 %>% select(AID, H1TO12, H1TO17)
w2vars <- da21600.0005 %>% select(AID, H2ED19, H2ED17, H2PR1, H2PR2, H2PR3, H2PR4)
w4vars <- da21600.0022 %>% select(
  AID,
  H4MA1,
  H4MA3,
  H4MA5,
  H4TO51,
  H4TO58,
  H4TO46,
  H4TO48,
  H4TO52,
  H4TO47,
  H4TO53,
  H4TO54,
  H4TO55,
  H4TO56,
  H4TO59,
  H4TO60,
  H4TO61,
  H4TO62
)

#merging the variables by AID
merg1 <- merge(w1vars, w2vars, by = "AID")
merg2 <- merge(merg1, w4vars, by = "AID")

#use dplyr function mutate to combine variables from subvariables e.g. ACEs

#aces
merg2$emotional_abuse <- as.numeric(merg2$H4MA1 == "(5) (5) More than ten times")
merg2$physical_abuse <- as.numeric(
  merg2$H4MA3 == "(3) (3) Three to five times" |
    merg2$H4MA3 == "(4) (4) Six to ten times" |
    merg2$H4MA3 == "(5) (5) More than ten times"
)
merg2$sexual_abuse <- as.numeric(merg2$H4MA5 != "(6) (6) This has never happened")
merg2 <- merg2 %>% mutate(aces = emotional_abuse + physical_abuse + sexual_abuse)

#early life substance use
merg2$early_life_alc_use <- as.numeric(merg2$H1TO12 == "(1) (1) Yes")
merg2$early_life_heavy_drinking <- levels(merg2$H1TO17) <- c(6, 5, 4, 3, 2, 1, 0)
merg2 <- merg2 %>% mutate(early_life_subst_use = early_life_alc_use + early_life_heavy_drinking)

#problematic substance use - create a manual diagnosis variable and send dr liu the codebooks


#perceived discriminiation
merg2$teacher_discrimination <- levels(merg2$H2ED19) = c(1, 2, 3, 4, 5)
merg2$peer_prejudice <- levels(merg2$H2ED17) = c(5, 4, 3, 2, 1)
merg2 <- merg2 %>% mutate(perceived_discrimination = teacher_discrimination + peer_prejudice)

#perceived social support
merg2$adult_support <- levels(merg2$H2PR1) = c(1, 2, 3, 4, 5)
merg2$teacher_support <- levels(merg2$H2PR2) = c(1, 2, 3, 4, 5)
merg2$parent_support <- levels(merg2$H2PR3) = c(1, 2, 3, 4, 5)
merg2$peer_support <- levels(merg2$H2PR4) = c(1, 2, 3, 4, 5)
merg2 <- merg2 %>% mutate(perceived_social_support = adult_support + teacher_support + parent_support + peer_support)
```

------------------------------------------------------------------------

## **Zoe’s first edit of code (tweaked code style + resolved errors)**

Pro tip! pressing control/command if you have a mac + shift + the a key
will reformat your code to make it easier to read :) That was the thing
only I tweaked about R’s code above \<33

### Step 1:selecting the variables from each wave and organizing them

``` r
#wave 1 variables
data_wave1 <- da21600.0001 |> 
  select(
    AID, H1TO12, H1TO17
    )
#wave 2 variables
data_wave2 <- da21600.0005 |> 
  select(
    AID, H2ED19, H2ED17, H2PR1, H2PR2, H2PR3, H2PR4
    )
#wave 4 variables
data_wave4 <- da21600.0022 |>
  select(
    AID,
    H4MA1,
    H4MA3,
    H4MA5,
    H4TO51,
    H4TO58,
    H4TO46,
    H4TO48,
    H4TO52,
    H4TO47,
    H4TO53,
    H4TO54,
    H4TO55,
    H4TO56,
    H4TO59,
    H4TO60,
    H4TO61,
    H4TO62
  )
```

### Step 2: merging the variables by AID

``` r
data_waves1and2 <- merge(
  data_wave1, 
  data_wave2, 
  by = "AID"
  )
data_waves124 <- merge(
  data_waves1and2,
  data_wave4,
  by = "AID"
  )
```

### Step 3: Using the dplyr function mutate to combine variables from subvariables e.g. ACEs

``` r
#aces :
data_waves124$emotional_abuse <- as.numeric(data_waves124$H4MA1 == "(5) (5) More than ten times")

data_waves124$physical_abuse <- as.numeric(
  data_waves124$H4MA3 == "(3) (3) Three to five times" |
    data_waves124$H4MA3 == "(4) (4) Six to ten times" |
    data_waves124$H4MA3 == "(5) (5) More than ten times"
)

data_waves124$sexual_abuse <- as.numeric(data_waves124$H4MA5 != "(6) (6) This has never happened")
data_waves124 <- data_waves124 |>
  mutate(aces = emotional_abuse + physical_abuse + sexual_abuse)
```

``` r
#problematic substance use - create a manual diagnosis variable and send dr liu the codebooks
```

### Still Step 3: These ones are a bit more complicated, so they get their own chunk

``` r
# Ensure factor levels are correctly defined
data_waves124 <- data_waves124 |>
  mutate(
    H1TO17 = factor(H1TO17, levels = c("(0) (0) Never", "(1) (1) Every day/almost every day", 
                                       "(2) (2) 2 or 3 days/week", "(3) (3) 1 or 2 days/week", 
                                       "(4) (4) 2 or 3 days/month", "(5) (5) Once a month or less (3-12 times in past 12 months)", 
                                       "(6) (6) 1 or 2 days in past 12 months", "(7) (7) Never")),
    H2ED19 = factor(H2ED19, levels = c("(1) (1) Strongly agree", "(2) (2) Agree", 
                                       "(3) (3) Neither agree nor disagree", "(4) (4) Disagree", 
                                       "(5) (5) Strongly disagree")),
    H2ED17 = factor(H2ED17, levels = c("(1) (1) Strongly agree", "(2) (2) Agree", 
                                       "(3) (3) Neither agree nor disagree", "(4) (4) Disagree", 
                                       "(5) (5) Strongly disagree")),
    H2PR1 = factor(H2PR1, levels = c("(1) (1) Not at all", "(2) (2) Very little", 
                                     "(3) (3) Somewhat", "(4) (4) Quite a bit", "(5) (5) Very much")),
    H2PR2 = factor(H2PR2, levels = c("(1) (1) Not at all", "(2) (2) Very little", 
                                     "(3) (3) Somewhat", "(4) (4) Quite a bit", "(5) (5) Very much")),
    H2PR3 = factor(H2PR3, levels = c("(1) (1) Not at all", "(2) (2) Very little", 
                                     "(3) (3) Somewhat", "(4) (4) Quite a bit", "(5) (5) Very much")),
    H2PR4 = factor(H2PR4, levels = c("(1) (1) Not at all", "(2) (2) Very little", 
                                     "(3) (3) Somewhat", "(4) (4) Quite a bit", "(5) (5) Very much"))
  )

# Early life substance use
data_waves124 <- data_waves124 |>
  mutate(
    early_life_alc_use = as.numeric(H1TO12 == "(1) (1) Yes"),
    early_life_heavy_drinking = as.numeric(fct_recode(H1TO17,
      `6` = "(6) (6) 1 or 2 days in past 12 months", 
      `5` = "(5) (5) Once a month or less (3-12 times in past 12 months)", 
      `4` = "(4) (4) 2 or 3 days/month", 
      `3` = "(3) (3) 1 or 2 days/week", 
      `2` = "(2) (2) 2 or 3 days/week", 
      `1` = "(1) (1) Every day/almost every day", 
      `0` = "(0) (0) Never"
    )),
    early_life_heavy_drinking = ifelse(is.na(early_life_heavy_drinking), 0, early_life_heavy_drinking),
    early_life_subst_use = early_life_alc_use + early_life_heavy_drinking
  )

# Perceived discrimination
data_waves124 <- data_waves124 |>
  mutate(
    teacher_discrimination = as.numeric(fct_recode(H2ED19, 
      `1` = "(1) (1) Strongly agree", 
      `2` = "(2) (2) Agree", 
      `3` = "(3) (3) Neither agree nor disagree", 
      `4` = "(4) (4) Disagree", 
      `5` = "(5) (5) Strongly disagree"
    )),
    peer_prejudice = as.numeric(fct_recode(H2ED17, 
      `1` = "(1) (1) Strongly agree", 
      `2` = "(2) (2) Agree", 
      `3` = "(3) (3) Neither agree nor disagree", 
      `4` = "(4) (4) Disagree", 
      `5` = "(5) (5) Strongly disagree"
    )),
    teacher_discrimination = ifelse(is.na(teacher_discrimination), 0, teacher_discrimination),
    peer_prejudice = ifelse(is.na(peer_prejudice), 0, peer_prejudice),
    perceived_discrimination = teacher_discrimination + peer_prejudice
  )

# Perceived social support
data_waves124 <- data_waves124 |>
  mutate(
    adult_support = as.numeric(fct_recode(H2PR1, 
      `1` = "(1) (1) Not at all", 
      `2` = "(2) (2) Very little", 
      `3` = "(3) (3) Somewhat", 
      `4` = "(4) (4) Quite a bit", 
      `5` = "(5) (5) Very much"
    )),
    teacher_support = as.numeric(fct_recode(H2PR2, 
      `1` = "(1) (1) Not at all", 
      `2` = "(2) (2) Very little", 
      `3` = "(3) (3) Somewhat", 
      `4` = "(4) (4) Quite a bit", 
      `5` = "(5) (5) Very much"
    )),
    parent_support = as.numeric(fct_recode(H2PR3, 
      `1` = "(1) (1) Not at all", 
      `2` = "(2) (2) Very little", 
      `3` = "(3) (3) Somewhat", 
      `4` = "(4) (4) Quite a bit", 
      `5` = "(5) (5) Very much"
    )),
    peer_support = as.numeric(fct_recode(H2PR4, 
      `1` = "(1) (1) Not at all", 
      `2` = "(2) (2) Very little", 
      `3` = "(3) (3) Somewhat", 
      `4` = "(4) (4) Quite a bit", 
      `5` = "(5) (5) Very much"
    )),
    adult_support = ifelse(is.na(adult_support), 0, adult_support),
    teacher_support = ifelse(is.na(teacher_support), 0, teacher_support),
    parent_support = ifelse(is.na(parent_support), 0, parent_support),
    peer_support = ifelse(is.na(peer_support), 0, peer_support),
    perceived_social_support = adult_support + teacher_support + parent_support + peer_support
  )
```

### Step 4: Check that the composites were created successfully

``` r
summary(data_waves124)
```

## Some tables to preview the composite variables we made :)

| AID      | emotional_abuse | physical_abuse | sexual_abuse | aces | early_life_alc_use |
|:---------|----------------:|---------------:|-------------:|-----:|-------------------:|
| 57101310 |               0 |              0 |            0 |    0 |                  1 |
| 57103869 |               0 |              1 |            1 |    2 |                  0 |
| 57109625 |               0 |              1 |            0 |    1 |                  1 |
| 57111071 |               0 |              0 |            0 |    0 |                  0 |
| 57113943 |               0 |              0 |            0 |    0 |                  0 |
| 57118381 |               0 |              0 |            0 |    0 |                  0 |

| AID | early_life_heavy_drinking | early_life_subst_use | teacher_discrimination | peer_prejudice | perceived_discrimination |
|:---|---:|---:|---:|---:|---:|
| 57101310 | 7 | 8 | 0 | 0 | 0 |
| 57103869 | 0 | 0 | 0 | 0 | 0 |
| 57109625 | 7 | 8 | 3 | 2 | 5 |
| 57111071 | 0 | 0 | 2 | 4 | 6 |
| 57113943 | 0 | 0 | 2 | 3 | 5 |
| 57118381 | 0 | 0 | 3 | 3 | 6 |

| AID | adult_support | teacher_support | parent_support | peer_support | perceived_social_support |
|:---|---:|---:|---:|---:|---:|
| 57101310 | 5 | 1 | 5 | 5 | 16 |
| 57103869 | 5 | 4 | 5 | 5 | 19 |
| 57109625 | 5 | 2 | 5 | 5 | 17 |
| 57111071 | 5 | 3 | 5 | 4 | 17 |
| 57113943 | 5 | 3 | 5 | 3 | 16 |
| 57118381 | 5 | 4 | 5 | 4 | 18 |

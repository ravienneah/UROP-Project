Missing Data Handling
================

#### Loading packages

``` r
library(tidyverse) 
```

#### Here’s where I loaded my data locally

``` r
load("~/Desktop/git/urop_ravienne/ICPSR UROP Data/ICPSR_21600 4/DS0005/21600-0005-Data.rda")
load("~/Desktop/git/urop_ravienne/ICPSR UROP Data/ICPSR_21600/DS0001/21600-0001-Data.rda")
load("~/Desktop/git/urop_ravienne/ICPSR UROP Data/ICPSR_21600 8/DS0022/21600-0022-Data.rda")
```

``` r
#wave 1 variables
data_wave1 <- da21600.0001 |>
  select(AID, H1TO12, H1TO17, S1, S2, S4, S6A, S6B, S6C, S6D, S6E)
```

``` r
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
    H4TO30:H4TO62,
    BIO_SEX4
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

``` r
# 33. Have you had a drink of beer, wine, or liquor more than two or three times? Do not include sips or tastes from someone else's drink
#if else yes - H4TO33 -> skip all other alc questions legit skip :3322
skiplogic33 <- data_waves124 |>
  mutate(lifetime_alc_use = as.numeric(H4TO33 == "(1) (1) Yes")) |>
  filter(lifetime_alc_use == 1)

skiplogic35 <- skiplogic33 |>
  mutate(past_year_alc_use = as.numeric(H4TO35)) |> 
  filter(past_year_alc_use != 1, na.rm = TRUE)

skiplogic43 <- skiplogic33 |>
  mutate(ever_drank_more = as.numeric(H4TO43== "(1) (1) Yes")) |>
  filter(ever_drank_more==1)
```

``` r
skiplogic39 <- skiplogic35 |>
  mutate(drinking_days_last_month = as.numeric(H4TO39)) |>
  filter(drinking_days_last_month != 1, na.rm = TRUE) 

skiplogic40 <- skiplogic39 |>
  filter(!is.na(H4TO40))

skiplogic44 <- skiplogic43 |>
  filter(!is.na(H4TO44)) |>
  mutate(former_alc_use = as.numeric(H4TO44))
```

``` r
#If BIO_SEX4 = 1 then: If Q35 = 5, 6, 7 and Q36 > 3, ask Q46. 
malecriteria1_ask46 <- skiplogic35 |>
  filter(BIO_SEX4 == "(1) (1) Male") |>
  filter(past_year_alc_use > 4) |>
  filter(H4TO36 > 3) |>
  filter(!is.na(H4TO46))

#Else if Q44 = 4, 5, 6, 7 and Q45 > 3, ask Q46. 
malecriteria2_ask46 <- skiplogic44 |>
  filter(BIO_SEX4 == "(1) (1) Male") |>
  filter(former_alc_use > 3) |>
  filter(H4TO45 > 3) |>
  filter(!is.na(H4TO46))
```

``` r
#If BIO_SEX4 = 2 then: If Q35 = 5, 6, 7 and Q36 > 2, ask Q46. Else if Q44 = 4, 5, 6, 7 and Q45 > 2, ask Q46.
femalecriteria1_ask46 <- skiplogic35 |>
  filter(BIO_SEX4 == "(2) (2) Female") |>
  filter(past_year_alc_use > 4) |>
  filter(H4TO36 > 2) |>
  filter(!is.na(H4TO46))

#Else if Q44 = 4, 5, 6, 7 and Q45 > 3, ask Q46. 
femalecriteria2_ask46 <- skiplogic44 |>
  filter(BIO_SEX4 == "(2) (2) Female") |>
  filter(former_alc_use > 3) |>
  filter(H4TO45 > 2) |>
  filter(!is.na(H4TO46))

all_AID <- data_waves124 |>
  select(AID)
```

``` r
male_cx_met_current <- left_join(all_AID, malecriteria1_ask46, by = c("AID" =
                                                                        "AID"))
male_cx_met_former <- left_join(all_AID, malecriteria2_ask46, by = c("AID" =
                                                                       "AID"))
female_cx_met_current <- left_join(all_AID, femalecriteria1_ask46, by = c("AID" =
                                                                            "AID"))
female_cx_met_former <- left_join(all_AID, femalecriteria2_ask46, by = c("AID" =
                                                                           "AID"))
lifetime_cx_list <- list(
  male_cx_met_current,
  female_cx_met_current,
  male_cx_met_former,
  female_cx_met_former
)

cx_met_lifetime <- lifetime_cx_list |>
  reduce(left_join, by = "AID", suffix = c("", "")) |>
  filter(!is.na(H4TO46)) |>
  select(AID,
         H4TO46:H4TO61,
        )
```

``` r
#missing_practice <- data_waves124 |>
#  mutate(lifetime_alc_use = as.numeric(H4TO33 == "(1) (1) Yes"))   |>
#  filter(lifetime_alc_use==1) |>
#  mutate(past_year_alc_use = as.numeric(H4TO35)) |> 
#  filter(past_year_alc_use != 1, na.rm = TRUE) |> 
#  mutate(drinking_days_last_month = as.numeric(H4TO39)) |>
#  filter(drinking_days_last_month != 1, na.rm = TRUE) 
```

``` r
#early life substance use
cx_met_lifetime <- cx_met_lifetime |>
  mutate(early_life_alc_use = (
    as.numeric(H1TO12 == "(1) (1) Yes")))


cx_met_lifetime <- cx_met_lifetime |>
  filter(early_life_alc_use == 1) |>
  mutate(
    early_life_heavy_drinking = as.numeric(fct_rev(H1TO17))
  )

cx_met_lifetime <- cx_met_lifetime |>
  mutate(early_life_subst_use = if_else(
    early_life_heavy_drinking >= 1,
    1,0))
```

``` r
#How often has your drinking ever interfered with your responsibilities at work or school?

cx_met_lifetime <- cx_met_lifetime |>
  filter(!is.na(H4TO46)) |>
  mutate(
    sud_progression3 = as.numeric(
      H4TO46 %in% c("(1) (1) 1 time", "(2) (2) More than 1 time")
    )
  )
#How often have you been under the influence of alcohol when you could have gotten yourself or others hurt, or put yourself or others at risk, including unprotected sex?
cx_met_lifetime <- cx_met_lifetime |>
  mutate(
    sud_preoccupation3 = as.numeric(
    H4TO47 %in% c("(1) (1) 1 time", "(2) (2) More than 1 time")
    )
  )
#How often have you had legal problems because of your drinking, like being arrested for disturbing the peace or driving under the influence of alcohol, or anything else?
cx_met_lifetime <- cx_met_lifetime |>
  mutate(
    sud_preoccupation1 = as.numeric(
      H4TO48 %in% c("(1) (1) 1 time", "(2) (2) More than 1 time")
    )
  )

#Have you ever found that you had to drink more than you used to in order to get the effect you wanted?
cx_met_lifetime <- cx_met_lifetime |>
  mutate(sud_progression1 = as.numeric(H4TO51 == "(1) (1) Yes"))

#Has there ever been a period when you spent a lot of time drinking, planning how you would get alcohol, or recovering from a hangover?
cx_met_lifetime <- cx_met_lifetime |>
  mutate(sud_preoccupation2 = as.numeric(H4TO52 == "(1) (1) Yes"))

# Have you often had more to drink or kept drinking for a longer period of time than you intended?
cx_met_lifetime <- cx_met_lifetime |>
  mutate(sud_losscontrol1 = as.numeric(H4TO53 == "(1) (1) Yes"))

#Have you ever tried to quit or cut down on your drinking?
#If Q54 = 0, 6, 8, ask Q55, else if Q54 = 1, skip to Q56.

#Has there ever been a period of time when you wanted to quit or cut down on your drinking? - 55

#When you decided to cut down or quit drinking, were you able to do so for at least one month? -56

cx_met_lifetime <- cx_met_lifetime %>%
  mutate(
    # Q54: Convert to numeric 0/1 (No/Yes), NA for other responses
    sud_losscontrol3 = case_when(
      H4TO54 == "(0) (0) No"   ~ 0,
      H4TO54 == "(1) (1) Yes"  ~ 1,
      TRUE                     ~ NA_real_  # Refused/Don't Know
    ),
    
    # Q55: Asked ONLY if Q54 ≠ 1 (i.e., Q54 is 0/NA)
    sud_losscontrol2 = if_else(
      H4TO54 != "(1) (1) Yes",  # Condition: Q54 is 0 or NA
      as.numeric(H4TO55 == "(1) (1) Yes"),  # Code 0/1 for Q55
      NA_real_  # Skip if Q54 == 1
    ),
    
    # Q56: Asked to EVERYONE (no skip logic)
    sud_losscontrol4 = as.numeric(fct_rev(H4TO56))  # Direct conversion
  )

#During the first few hours of not drinking do you experience withdrawal symptoms such as the shakes, feeling anxious, trouble getting to sleep or staying asleep, nausea, vomiting, or rapid heartbeats?
  
cx_met_lifetime <- cx_met_lifetime |>
  mutate(sud_progression2 = as.numeric(H4TO58 == "(1) (1) Yes")) 

#Have you ever continued to drink after you realized drinking was causing you any emotional problems (such as feeling irritable, depressed, or uninterested in things or having strange ideas) or causing you any health problems (such as ulcers, numbness in your hands/feet or memory problems)?

cx_met_lifetime <- cx_met_lifetime |>
  mutate(sud_persistence1 = as.numeric(H4TO59 == "(1) (1) Yes"))

#Have you ever given up or cut down on important activities that would interfere with drinking like getting together with friends or relatives, going to work or school, participating in sports, or anything else?

cx_met_lifetime <- cx_met_lifetime |>
  mutate(sud_persistence2 = as.numeric(H4TO60 == "(1) (1) Yes"))
```

cx_met_lifetime \<- cx_met_lifetime \|\> mutate( sud_progression =
sud_progression1 + sud_progression2 + sud_progression3, \#
sud_preoccupation = sud_preoccupation1 + sud_preoccupation2 +
sud_preoccupation3, \# sud_losscontrol = sud_losscontrol1 +
sud_losscontrol2 + sud_losscontrol3 + sud_losscontrol4, sud_persistence
= sud_persistence1 + sud_persistence2 )

# Create composite drinking experiences

cx_met_lifetime \<- cx_met_lifetime \|\> mutate( drinking_exp =
sud_progression + sud_preoccupation + sud_losscontrol + sud_persistence
)

cx_met_lifetime \<- cx_met_lifetime \|\> mutate(substance_misuse =
if_else(H4TO61 == “(1) (1) Yes” & H4TO62 \>= 18, 1, 0)) \#here’s where
we could change the criteria - “drinking experiences \>= 3” is what the
study used as criteria to ask Q61, that could be replaced with other
criteria and potentially leave out Q61

cx_met_lifetime \<- cx_met_lifetime \|\> mutate( substance_misuse_manual
= if_else( drinking_exp \>= 3 & H4TO61 == “(1) (1) Yes” & H4TO62 \>= 18,
1, 0))

AUD_indicators \<- cx_met_lifetime \|\> select(AID,
substance_misuse_manual, substance_misuse, drinking_exp,
starts_with(“sud”), H4TO47:H4TO62, ) write_csv(x = AUD_indicators, file
= “problematic_alcohol_misuse_addhealth.csv”)


    ### Step 4: Check that the composites were created successfully


    ``` r
    addhealth_clean_and_composites <- (data_waves124[, c(1, 27:60)])
    summary(addhealth_clean_and_composites)
    write_csv(x=addhealth_clean_and_composites,"addhealth_clean_and_composites")

## Missing Data handling

``` r
missing_AUD <- as.data.frame(which(is.na(AUD_indicators$sud_progression1)))
count(missing_AUD)
```

#### [google sheet link :)](https://docs.google.com/spreadsheets/d/1Qvp9XRvgWT0kIqNywmpVKmovxEQQrm3WWV7K6oD7JFQ/edit?gid=0#gid=0)

# 33. Have you had a drink of beer, wine, or liquor more than two or three times? Do not include sips or tastes from someone else’s drink

if else yes - H4TO33 -\> skip all other alc questions legit skip \#
:3322

# 35. During the past 12 months, on how many days did you drink alcohol?

H4TO35 -\> if ‘none’ - skip H4TO36:H4TO39 legit skip \# : 3322

# 43. Was there ever a period in your life when you drank more alcohol than you do now?

H4TO43 -\> if ‘no’ skip H4TO45:H4TO46

legit skip \#: 3322 (skipped from H4TO33)

------------------------------------------------------------------------

# 36. Think of all the times you have had a drink during the past **12 months.** How many drinks did you usually have each time? A ‘drink’ is a glass of wine, a can or bottle of beer, a wine cooler, a shot glass of liquor, or a mixed drink.

H4TO36 -\> skip H4TO37:H4TO42T legit skip \# : 4427 + 1105 skipped
(H4TO35 + H4TO33)

# 37. During the past 12 months, on how many days did you drink \[5 or more/4 or more\] drinks in a row?

4427

# 38. During the past 12 months, on how many days have you been drunk or very high on alcohol?

4427

# 39. During the past 30 days, on how many days did you drink?

H4TO39 -\> if ‘none’ - skip H4TO40:H4TO42T legit skip \# : 4427

# 40. Think of all the times you have had a drink during the past **30 days.** How many drinks did you usually have each time? A ‘drink’ is a glass of wine, a can or bottle of beer, a wine cooler, a shot glass of liquor, or a mixed drink.

H4TO40 -\> skip H4TO41:H4TO42T

legit skip \# : 6307

\+ 1880 skipped

(10 more than ‘no’ trying to figure out why)

H4TO42H legit skip \# : 12445

\+ 6138 skipped (skipped from 33 + 39 + 40)

# 44. During the period when you drank the most, on how many days did you drink?

H4TO44 -\> legit skip \#: 8723 (skipped from 33 + 43) + 5401 skipped (23
more than ‘no’)

# 45. During the period when you drank the most, how many drinks did you usually have each time?

legit skip \#: 8723 (skipped from 33 + 43)

if else criteria met -\> skip all rest

------------------------------------------------------------------------

Checkpoint before

Q46: If BIO_SEX4 = 1 then: If Q35 = 5, 6, 7 and Q36 \> 3, ask Q46. Else
if Q44 = 4, 5, 6, 7 and Q45 \> 3, ask Q46. Else skip to Q63. If BIO_SEX4
= 2 then: If Q35 = 5, 6, 7 and Q36 \> 2, ask Q46. Else if Q44 = 4, 5, 6,
7 and Q45 \> 2, ask Q46.

Else skip to Q63. How many times has each of the following things ever
happened?

# 46. How often has your drinking interfered with your responsibilities at work or school?

H4TO46 -\>  
legit skip \#: 9330

- 607 skipped
  - of 607 – 193 refused/don’t know or not asked on pretest 414 did not
    meet inclusion criteria

H4TO47  
\# How often have you been under the influence of alcohol when you could
have gotten yourself or others hurt, or put yourself or others at risk,
including unprotected sex?

legit skip \#: 9330

H4TO48  
\# How often have you had legal problems because of your drinking, like
being arrested for disturbing the peace or driving under the influence
of alcohol, or anything else?

legit skip \#: 9330

H4TO49

\# 49. How often have you had problems with your family, friends, or
people at work or school because of your drinking?

if ‘never’ -\> skip 51

legit skip \#: 9330

H4TO50 \# 50. Did you continue to drink after you realized drinking was
causing you problems with family, friends, or people at work or school?

legit skip \#: 14011

\+ 4681 skipped (‘never’ 49 + skip 45)

H4TO51  
\# Have you ever found that you had to drink more than you used to in
order to get the effect you wanted?

legit skip \#: 9330 (skip 45)

*other missing: 5*

H4TO52  
\# Has there ever been a period when you spent a lot of time drinking,
planning how you would get alcohol, or recovering from a hangover?

other missing: 8

H4TO53  
\# Have you often had more to drink or kept drinking for a longer period
of time than you intended?

other missing: 7

*H4TO54  
\# Have you ever tried to quit or cut down on your drinking? -\>*

*if ‘yes’ - skip H4TO55 (3594)*

*-\> if else - skip H4TO56 (no = 2765)*

*other missing: 12*

H4TO55  
\# 55. Has there ever been a period of time when you wanted to quit or
cut down on your drinking? legit skip \#:12924 + 3594 skipped (‘yes’ 55)

other missing: 11

H4TO56  
\# 56. When you decided to cut down or quit drinking, were you able to
do so for at least one month?

legit skip \#: 12103

\+ 2773 skipped (if else ‘yes’ 54)

if ‘yes’ -\> skip H4TO57

other missing: 5

*H4TO57 \# 57. How many times have you tried but been unable to cut down
or quit drinking for at least one month?*

*legit skip \#: 15434*

*+ 3331 skipped (‘yes’ H4TO56)*

other missing: 7

H4TO58  
\# During the first few hours of not drinking do you experience
withdrawal symptoms such as the shakes, feeling anxious, trouble getting
to sleep or staying asleep, nausea, vomiting, or rapid heartbeats?

legit skip \#: 9330 (skip from 45)

other missing : 9

H4TO59  
\# Have you ever continued to drink after you realized drinking was
causing you any emotional problems (such as feeling irritable,
depressed, or uninterested in things or having strange ideas) or causing
you any health problems (such as ulcers, numbness in your hands/feet or
memory problems)? legit skip \#: 9330 (skip from 45) other missing : 12
H4TO60  
\# Have you ever given up or cut down on important activities that would
interfere with drinking like getting together with friends or relatives,
going to work or school, participating in sports, or anything else?

legit skip \#: 9330 (skip from 45)

(Q51 = 1) - missing 5

\+ (Q52 = 1) - missing 8

\+ (Q53 = 1) - missing 7

\+ (Q56 = 0) - missing 5

\+ (Q55 = 1) - missing 11

\+ (Q58 = 1) - missing 7

\+ (Q59 = 1) - missing 12

\+ (Q60 = 1) - missing 9

all missing not skipped : 64

If drinking experiences \>= 3 then display ‘You said that you…’ … Ask
Q.61. Else -\> skip to Q.63.

If drinking experiences \> 3, insert ‘at least three of.’

Other missing drinking exp: \# 61. Did (at least three of) these
experiences occur together in a 12-month period?

legit skip \#: 13629

\+ 4299 might include NAs

1,392 responded yes to H461

legit skip \#: 13638 invalid total

– \# 62. legit skip \#: 14300 + 671 (‘no’ 61)

3322 excluded 33 onwards -\> can omit

9930 excluded 47 onwards -\> can omit

### Step 3: Using the dplyr function mutate to combine variables from subvariables e.g. ACEs

#### ACES

``` r
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

### Step 3.2: Creating composites for trickier variables

#### perceived discriminiation & social support

``` r
#perceived discriminiation
data_waves124$teacher_discrimination <- data_waves124$H2ED19 |>
  fct_rev() |>
  as.numeric()

data_waves124$peer_prejudice <- data_waves124$H2ED17 |>
  fct_rev() |>
  as.numeric()

data_waves124 <- data_waves124 |> 
  mutate(perceived_discrimination = replace_na(teacher_discrimination, 0) +
               replace_na(peer_prejudice, 0))
#perceived social support
data_waves124$adult_support <- data_waves124$H2PR1 |>
  fct_rev() |>
  as.numeric()
data_waves124$teacher_support <- data_waves124$H2PR2 |>
  fct_rev() |>
  as.numeric()
data_waves124$parent_support <- data_waves124$H2PR3 |>
  fct_rev() |>
  as.numeric()
data_waves124$peer_support <- data_waves124$H2PR4 |>
  fct_rev() |>
  as.numeric()

data_waves124 <- data_waves124 |>
  mutate(perceived_social_support =
    replace_na(adult_support, 0) + replace_na(teacher_support, 0)
    + replace_na(parent_support, 0) + replace_na(peer_support, 0)
  )
```

### Step 3.3: Cleaning + creating composites for questions related to problematic alcohol use

#### Domain approach + study skip logic

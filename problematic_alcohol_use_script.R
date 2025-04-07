# load data
load("~/Desktop/git/urop_ravienne/ICPSR UROP Data/ICPSR_21600 4/DS0005/21600-0005-Data.rda")
load("~/Desktop/git/urop_ravienne/ICPSR UROP Data/ICPSR_21600/DS0001/21600-0001-Data.rda")
load("~/Desktop/QPL/urop_ravienne/ICPSR UROP Data/ICPSR_21600 8/DS0022/21600-0022-Data.rda")
#wave 1 git
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
#aces
data_waves124$emotional_abuse <- as.numeric(data_waves124$H4MA1 == "(5) (5) More than ten times")
data_waves124$physical_abuse <- as.numeric(
  data_waves124$H4MA3 == "(3) (3) Three to five times" |
    data_waves124$H4MA3 == "(4) (4) Six to ten times" |
    data_waves124$H4MA3 == "(5) (5) More than ten times"
)
data_waves124$sexual_abuse <- as.numeric(data_waves124$H4MA5 != "(6) (6) This has never happened")
data_waves124 <- data_waves124 |>
  mutate(aces = emotional_abuse + physical_abuse + sexual_abuse)
#perceived discriminiation
data_waves124$teacher_discrimination <- data_waves124$H2ED19 |>
  fct_rev() |>
  as.numeric() 

data_waves124$peer_prejudice <- data_waves124$H2ED17 |>
  fct_rev() |>
  as.numeric() 
data_waves124$teacher_discrimination <- data_waves124$H2ED19 |>
  fct_rev() |>
  as.numeric()

data_waves124$peer_prejudice <- data_waves124$H2ED17 |>
  fct_rev() |>
  as.numeric()

data_waves124 <- data_waves124 |> mutate(
  (perceived_discrimination = replace_na(teacher_discrimination, 0) +
     replace_na(peer_prejudice, 0)))
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

data_waves124$early_life_alc_use <- as.numeric(data_waves124$H1TO12 == "(1) (1) Yes")
data_waves124$early_life_heavy_drinking <- as.numeric(data_waves124$H1TO17)

#early life substance use
data_waves124$early_life_alc_use <- as.numeric(data_waves124$H1TO12 == "(1) (1) Yes")

data_waves124$early_life_heavy_drinking <- data_waves124$H1TO17 |>
  fct_rev() |>
  as.numeric()

data_waves124 <- data_waves124 |>
  mutate(
    early_life_subst_use = replace_na(early_life_alc_use, 0) + replace_na(early_life_heavy_drinking, 0)
  )
#Have you ever found that you had to drink more than you used to in order to get the effect you wanted?
data_waves124$sud_progression1 <- as.numeric(data_waves124$H4TO51 == "(1) (1) Yes")

#During the first few hours of not drinking do you experience withdrawal symptoms such as the shakes, feeling anxious, trouble getting to sleep or staying asleep, nausea, vomiting, or rapid heartbeats?
data_waves124$sud_progression2 <- as.numeric(data_waves124$H4TO58 == "(1) (1) Yes")

#How often has your drinking ever interfered with your responsibilities at work or school?
data_waves124$sud_progression3 <- data_waves124$H4TO46 |>
  fct_collapse("(1) Once or more"= c("(1) (1) 1 time","(2) (2) More than 1 time"))
data_waves124$sud_progression3 <-as.numeric(
  data_waves124$sud_progression3=="(1) Once or more")

#How often have you had legal problems because of your drinking, like being arrested for disturbing the peace or driving under the influence of alcohol, or anything else?
data_waves124$sud_preoccupation1 <- data_waves124$H4TO48 |>
  fct_collapse("(1) Once or more"= c("(1) (1) 1 time","(2) (2) More than 1 time"))
data_waves124$sud_preoccupation1 <-as.numeric(
  data_waves124$sud_preoccupation1=="(1) Once or more")

#Has there ever been a period when you spent a lot of time drinking, planning how you would get alcohol, or recovering from a hangover?
data_waves124$sud_preoccupation2 <- as.numeric(data_waves124$H4TO52 == "(1) (1) Yes")

#How often have you been under the influence of alcohol when you could have gotten yourself or others hurt, or put yourself or others at risk, including unprotected sex?
data_waves124$sud_preoccupation3 <- data_waves124$H4TO47 |>
  fct_collapse("(1) Once or more"= c("(1) (1) 1 time","(2) (2) More than 1 time"))
data_waves124$sud_preoccupation3 <-as.numeric(
  data_waves124$sud_preoccupation1=="(1) Once or more")

# Have you often had more to drink or kept drinking for a longer period of time than you intended?
data_waves124$sud_losscontrol1 <- as.numeric(data_waves124$H4TO53 == "(1) (1) Yes")

#Has there ever been a period of time when you wanted to quit or cut down on your drinking?
data_waves124$sud_losscontrol2 <- as.numeric(data_waves124$H4TO55 == "(1) (1) Yes")

#Have you ever tried to quit or cut down on your drinking?
data_waves124$sud_losscontrol3 <- as.numeric(data_waves124$H4TO54 == "(1) (1) Yes")

#When you decided to cut down or quit drinking, were you able to do so for at least one month?
data_waves124$sud_losscontrol4 <- data_waves124$H4TO56 |>
  fct_rev() |>
  as.numeric() 

#Have you ever continued to drink after you realized drinking was causing you any emotional problems (such as feeling irritable, depressed, or uninterested in things or having strange ideas) or causing you any health problems (such as ulcers, numbness in your hands/feet or memory problems)?

data_waves124$sud_persistence1 <- as.numeric(data_waves124$H4TO59 == "(1) (1) Yes")

#Have you ever given up or cut down on important activities that would interfere with drinking like getting together with friends or relatives, going to work or school, participating in sports, or anything else?

data_waves124$sud_persistence2 <- as.numeric(data_waves124$H4TO60 == "(1) (1) Yes")

# Create composite domains with NAs replaced by 0 for calculation

data_waves124 <- data_waves124 |>
  mutate(
    sud_progression = replace_na(sud_progression1, 0) + replace_na(sud_progression2, 0) + replace_na(sud_progression3, 0),
    sud_preoccupation = replace_na(sud_preoccupation1, 0) + replace_na(sud_preoccupation2, 0) + replace_na(sud_preoccupation3, 0),
    sud_losscontrol = replace_na(sud_losscontrol1, 0) + replace_na(sud_losscontrol2, 0) + replace_na(sud_losscontrol3, 0) + replace_na(sud_losscontrol4, 0),
    sud_persistence = replace_na(sud_persistence1, 0) + replace_na(sud_persistence2, 0)
  )

# Create composite drinking experiences
data_waves124 <- data_waves124 |>
  mutate(
    drinking_exp = sud_progression + sud_preoccupation + sud_losscontrol + sud_persistence
  )

data_waves124 <- data_waves124 |>
  mutate(substance_misuse = if_else(H4TO61 == "(1) (1) Yes" &
                                      H4TO62 >= 18, 1, 0))

#here's where we could change the criteria - "drinking experiences >= 3" 
#is what the study used as criteria to ask Q61, that could be replaced with 
#other criteria and potentially leave out Q61 

data_waves124 <- data_waves124 |>
  mutate(
    substance_misuse_manual = if_else(
      drinking_exp >= 3 & H4TO61 == "(1) (1) Yes" & H4TO62 >= 18, 
      1,
      0))

AUD_indicators <- data_waves124 |>
  select(AID,
         substance_misuse_manual,
         substance_misuse,
         drinking_exp,
         starts_with("sud"),
         H4TO47:H4TO60
  )

write_csv(x = AUD_indicators, file = "problematic_alcohol_use_addhealth.csv")
#selecting the variables from each wave and organizing them
library(dplyr)
w1vars<-da21600.0001%>%select(AID,H1TO12,H1TO17)
w2vars<-da21600.0005%>%select(AID, H2ED19, H2ED17, H2PR1, H2PR2, H2PR3, H2PR4)
w4vars<-da21600.0022%>%select(AID, H4MA1, H4MA3, H4MA5, H4TO51, H4TO58, H4TO46, H4TO48, H4TO52, H4TO47, H4TO53, H4TO54, H4TO55, H4TO56, H4TO59, H4TO60, H4TO61, H4TO62)

#merging the variables by AID
merg1<-merge(w1vars,w2vars,by="AID")
merg2<-merge(merg1,w4vars,by="AID")

#use dplyr function mutate to combine variables from subvariables e.g. ACEs

#aces
merg2$emotional_abuse<- as.numeric(merg2$H4MA1=="(5) (5) More than ten times")
merg2$physical_abuse<- as.numeric(merg2$H4MA3=="(3) (3) Three to five times"|merg2$H4MA3=="(4) (4) Six to ten times" | merg2$H4MA3=="(5) (5) More than ten times")
merg2$sexual_abuse<- as.numeric(merg2$H4MA5!="(6) (6) This has never happened")
merg2 <- merg2 %>% mutate(aces = emotional_abuse + physical_abuse + sexual_abuse)

#early life substance use
merg2$early_life_alc_use<- as.numeric(merg2$H1TO12=="(1) (1) Yes")
merg2$early_life_heavy_drinking<- levels(merg2$H1TO17)<-c(6,5,4,3,2,1,0)
merg2 <- merg2 %>% mutate(early_life_subst_use = early_life_alc_use + early_life_heavy_drinking)

#problematic substance use - create a manual diagnosis variable and send dr liu the codebooks


#perceived discriminiation
merg2$teacher_discrimination<- levels(merg2$H2ED19)=c(1,2,3,4,5)
merg2$peer_prejudice<- levels(merg2$H2ED17)=c(5,4,3,2,1)
merg2 <- merg2 %>% mutate(perceived_discrimination = teacher_discrimination + peer_prejudice)

#perceived social support
merg2$adult_support<- levels(merg2$H2PR1)=c(1,2,3,4,5)
merg2$teacher_support<- levels(merg2$H2PR2)=c(1,2,3,4,5)
merg2$parent_support<- levels(merg2$H2PR3)=c(1,2,3,4,5)
merg2$peer_support<- levels(merg2$H2PR4)=c(1,2,3,4,5)
merg2 <- merg2 %>% mutate(perceived_social_support = adult_support + teacher_support + parent_support + peer_support)


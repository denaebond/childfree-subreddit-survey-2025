library(dslabs)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ggrepel)

childfree <- read.csv("childfree_survey_2025.csv") %>%
  filter(parenthood == "I do not have children nor want to have them in any capacity (biological, adopted, fostered, step- or other) at any point in the future.")
mixed_ethnicity <- c("","African Descent/Black","Arabic/Middle Eastern/Near Eastern","Caucasian/White","East Asian" ,"Hispanic" ,"Indian/South Asian","Jewish (the ethnicity, not religion)" ,"North American Indigenous or Alaskan Native","Other","Pacific Islander/Melanesian","South-East Asian")
minority_ethnicity <- c("African Descent/Black","Arabic/Middle Eastern/Near Eastern","East Asian" ,"Hispanic" ,"Indian/South Asian","Jewish (the ethnicity, not religion)" ,"North American Indigenous or Alaskan Native","Other","Pacific Islander/Melanesian","South-East Asian")
childfree <- childfree %>% mutate(US = case_when(.$current_location == "United States" ~ "US", .$current_location != "United States" ~ "Non US")) %>% 
  mutate(sterilization_desire = 
           case_when(.$sterilization_status == "Yes" ~ "Sterilized", .$sterilization_status %in% c("No, but desired and looking for a doctor", "No, but desired and not looking for a doctor yet", "No, but waiting for the procedure", "No, but desired and not legal") ~ "Desired", .$sterilization_status %in% c("No, and not needed", "No, and not desired ") ~ "Not Desired", .$sterilization_status %in% c("Sterile due to a medical procedure", "Naturally sterile") ~ "Already Sterile")) %>%
  mutate(relation_conden = case_when(.$relationship_status %in% c("Single and not looking", "Single and open to non-serious options", "Single, looking for something serious") ~ "Single", .$relationship_status == "Short term relationship" ~ "Short term relationship", .$relationship_status %in% c("Long term relationship, not living with together", "Long term relationship, living together", "Engaged", "Married") ~ "Long-Term, Serious", .$relationship_status %in% c("Separated", "Divorced", "Widowed") ~ "Previously Serious", .$relationship_status == "Other" ~ "Other")) %>%
  mutate(ethnicity_conden = case_when(.$ethnicity == "" ~ "", .$ethnicity == "African Descent/Black" ~ "African Descent/Black", .$ethnicity == "Arabic/Middle Eastern/Near Eastern" ~ "Arabic/Middle Eastern/Near Eastern", .$ethnicity == "Caucasian/White" ~ "Caucasian/White", .$ethnicity == "East Asian" ~ "East Asian", .$ethnicity == "Hispanic" ~ "Hispanic", .$ethnicity == "Indian/South Asian" ~ "Indian/South Asian", .$ethnicity == "Jewish (the ethnicity, not religion)" ~ "Jewish (the ethnicity, not religion)", .$ethnicity == "North American Indigenous or Alaskan Native" ~ "North American Indigenous or Alaskan Native", .$ethnicity == "Other" ~ "Other", .$ethnicity == "Pacific Islander/Melanesian" ~ "Pacific Islander/Melanesian", .$ethnicity == "South-East Asian" ~ "South-East Asian", !(.$ethnicity %in% mixed_ethnicity) ~ "Mixed Ethincity")) %>%
  mutate(doctor_refusals_num = case_when(.$doctor_refusals == "" ~ NA, .$doctor_refusals == "0" ~ 0, .$doctor_refusals == "1" ~ 1, .$doctor_refusals == "2" ~ 2,.$doctor_refusals == "3" ~ 3,.$doctor_refusals == "4" ~ 4, .$doctor_refusals == "5" ~ 5, .$doctor_refusals == "6" ~ 6, .$doctor_refusals == "8" ~ 8, .$doctor_refusals == "I haven't found a doctor willing to sterilize me yet" ~ NA, .$doctor_refusals == "10+" ~ 10)) %>%
  mutate(racial_minorities = case_when(.$ethnicity == "" ~ "", .$ethnicity == "Caucasian/White" ~ "Caucasian/White", !(.$ethnicity %in% mixed_ethnicity) ~ "Mixed Ethincity", .$ethnicity %in% minority_ethnicity ~ "Ethinc Minority"))
childfree <- childfree %>% mutate(age_group = factor(age_group, levels = c("18 years old or younger","19 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 or older"), ordered = TRUE)) %>% 
  mutate(childfree_age = factor(childfree_age, levels = c("18 years old or younger","19 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49"), ordered = TRUE)) %>%
  mutate(gender = factor(gender, ordered = FALSE)) %>%
  mutate(sexual_orientation = factor(sexual_orientation, ordered = FALSE)) %>%
  mutate(education = factor(education, levels = c("Did not complete high school","Graduated high school / GED","Trade / Technical / Vocational training","Some college / university","Associate's degree","Bachelor's degree","Some grad school","Professional degree","Master's degree","Doctorate degree" ,"Post Doctorate"), ordered = TRUE)) %>%
  mutate(major = factor(major, ordered = FALSE)) %>%
  mutate(current_industry = factor(current_industry, ordered = FALSE)) %>%
  mutate(work_hours = factor(work_hours, levels = c("I'm retired","I'm a student (high school, university, college etc) who doesn't have a paid job on the side","I'm  unemployed","I'm out of the work force for health or other reasons","I work 1 to 9 hours per week","I work 10 to 19 hours per week","I work 20 to 29 hours per week","I work 30 to 39 hours per week","I work 40 to 49 hours per week","I work 50 to 59 hours per week","I work 60 to 69 hours per week","I work 70 hours or more per week"), ordered = TRUE)) %>%
  mutate(industry_position = factor(industry_position, ordered = FALSE)) %>%
  mutate(retired = factor(retired, ordered = FALSE)) %>%
  mutate(old_age_plan = factor(old_age_plan, ordered = FALSE)) %>%
  mutate(nationality = factor(nationality, ordered = FALSE)) %>%
  mutate(current_location = factor(current_location, ordered = FALSE)) %>%
  mutate(ethnicity = factor(ethnicity, ordered = FALSE)) %>%
  mutate(childhood_faith = factor(childhood_faith, ordered = FALSE)) %>%
  mutate(religious_practice = factor(religious_practice, ordered = FALSE)) %>%
  mutate(relationship_status = factor(relationship_status, levels = c("Single and not looking","Single and open to non-serious options","Single, looking for something serious","Short term relationship", "Long term relationship, not living with together","Long term relationship, living together","Engaged","Married", "Separated","Divorced","Widowed", "Other"), ordered = TRUE)) %>%
  mutate(partner_childfree = factor(partner_childfree, levels = c("I don't have a partner" ,"No", "I have more than one partner and none are childfree","I have more than one partner and some are childfree","I have more than one partner and they are all childfree","Yes"), ordered = TRUE)) %>%
  mutate(sterilization_status = factor(sterilization_status, levels = c("Naturally sterile","Sterile due to a medical procedure","No, and not needed","No, and not desired","No, but desired and not legal", "No, but desired and not looking for a doctor yet","No, but desired and looking for a doctor", "No, but waiting for the procedure", "Yes"), ordered = TRUE)) %>%
  mutate(doctor_shopping_age = factor(doctor_shopping_age, levels = c("I have not addressed sterilization with my doctor", "18 or younger", "19 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54"), ordered = TRUE)) %>% 
  mutate(sterilization_age = factor(sterilization_age, levels = c("I haven't been sterilized yet", "18 or younger", "19 to 24","25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49"), ordered = TRUE)) %>%
  mutate(sterilization_time = factor(sterilization_time, levels = c("I haven't been sterilized yet","Less than 3 months","Between 3 and 6 months","Between 6 and  9 months","Between 9 and 12 months","Between 12 and 18 months","Between 18 and 24 months","Between 24 and 30 months","Between 30 and 36 months","Between 3 and 5 years","Between 5 and 7 years","More than 7 years"), ordered = TRUE)) %>%
  mutate(doctor_refusals = factor(doctor_refusals, levels = c("I haven't found a doctor willing to sterilize me yet","0","1","2","3","4","5","6","8","10+"), ordered = TRUE)) %>%
  mutate(doctor_search_strat = factor(doctor_search_strat, ordered = FALSE)) %>%
  mutate(childfree_main_reason = factor(childfree_main_reason, ordered = FALSE)) %>%
  mutate(child_focused_job = factor(child_focused_job, levels = c("No","Yes","Student, but yes in the future","Unemployed, but yes in the past","Retired, but yes in the past","Other"), ordered = TRUE)) %>%
  mutate(childfree_support = factor(childfree_support, levels = c("Fully unsupportive","Mostly unsupportive","Somewhat unsupportive","Neither supportive nor unsupportive","Somewhat supportive","Mostly supportive", "Fully supportive", "It varies", "Other","No-one knows that I'm childfree offline"), ordered = TRUE)) %>%
  mutate(openly_childfree_loved_ones = factor(openly_childfree_loved_ones, ordered = FALSE)) %>%
  mutate(bingo_frequency = factor(bingo_frequency, levels = c("I've never been bingoed", "Once a year or less often", "Once every 6 months", "Once every 3 months", "Once a month" , "Once a week", "Once a day or more", "I'm not sure whether or not I get bingod."), ordered = TRUE)) %>%
  mutate(pro.choice = factor(pro.choice, levels = c("Yes", "Yes, but not for myself", "Yes, but with caveats", "No, but with caveats", "Other", "I'm not sure"), ordered = TRUE)) %>%
  mutate(financial_abortion = factor(financial_abortion,levels = c("Yes","Yes, if abortion is legal and safe","No","I'm not sure"), ordered = TRUE)) %>%
  mutate(pet_ownership = factor(pet_ownership, levels = c("Yes","No, but I help care for someone else\x92s", "No, but live with someone else\x92s","No"), ordered = TRUE)) %>%
  mutate(reddit_activity = factor(reddit_activity, levels = c("Lurker","Commenter","Poster","Participant","I don't know / I'm not sure"), ordered = TRUE)) %>%
  mutate(US = factor(US, ordered = FALSE)) %>%
  mutate(sterilization_desire = factor(sterilization_desire, levels = c("Already Sterile","Not Desired","Desired","Sterilized"), ordered = TRUE)) %>%
  mutate(relation_conden = factor(relation_conden, levels = c("Single","Short term relationship","Long-Term, Serious","Previously Serious","Other"), ordered = TRUE)) %>%
  mutate(ethnicity_conden = factor(ethnicity_conden, ordered = FALSE)) %>%
  mutate(racial_minorities = factor(racial_minorities, ordered = FALSE))

sterilized <- childfree %>% filter(sterilization_status == "Yes" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet")

#Those who realized they're childfree at a younger age are more likely to be sterilized
childfree %>% filter(childfree_age != "" & sterilization_desire != "") %>% 
  ggplot(aes(childfree_age, fill = sterilization_desire)) + 
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(angle= 25, hjust = 1))

childfree %>% filter(childfree_age != "" & sterilization_status != "") %>% 
  ggplot(aes(childfree_age, fill = sterilization_status)) + 
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(angle= 25, hjust = 1))

#
childfree %>% filter(age_group != "" & sterilization_status != "") %>% 
  ggplot(aes(age_group, fill = sterilization_status)) + 
  geom_bar(position = position_fill())

childfree %>% filter(age_group != "" & sterilization_status != "") %>% 
  ggplot(aes(sterilization_status, fill = age_group)) + 
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 5, angle= 25, hjust = 1))

#Those currently sterilized are mostly 30 or older at sterilization age = False, mostly 25 to 29
childfree %>% filter(sterilization_age != "" & sterilization_status == "Yes") %>%
  ggplot(aes(sterilization_age)) + 
  geom_bar()

#Those currently sterilized under 30 had to talk to more doctors = False
childfree %>% filter(sterilization_status == "Yes" & sterilization_age != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(doctor_refusals, fill = sterilization_age)) +
  geom_bar()

childfree %>% filter(sterilization_status == "Yes" & sterilization_age != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(doctor_refusals, fill = sterilization_age)) +
  geom_bar(position = position_fill())

childfree %>% filter(sterilization_status == "Yes" & sterilization_age != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(sterilization_age, fill = doctor_refusals)) +
  geom_bar()

childfree %>% filter(sterilization_status == "Yes" & sterilization_age != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(sterilization_age, fill = doctor_refusals)) +
  geom_bar(position = position_fill())
  
#Women had to talk to more doctors than men = True 
childfree %>% filter(sterilization_status == "Yes" & gender != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(doctor_refusals, fill = gender)) +
  geom_bar()

childfree %>% filter(sterilization_status == "Yes" & gender != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(doctor_refusals, fill = gender)) +
  geom_bar(position = position_fill())

childfree %>% filter(sterilization_status == "Yes" & gender != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(gender, fill = doctor_refusals)) +
  geom_bar()

childfree %>% filter(sterilization_status == "Yes" & gender != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(gender, fill = doctor_refusals)) +
  geom_bar(position = position_fill())

childfree %>% filter(sterilization_status %in% c("Yes", "No, but waiting for the procedure") & gender != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(doctor_refusals, fill = gender)) +
  geom_bar()

childfree %>% filter(sterilization_status %in% c("Yes", "No, but waiting for the procedure") & gender != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(doctor_refusals, fill = gender)) +
  geom_bar(position = position_fill())

childfree %>% filter(sterilization_status %in% c("Yes", "No, but waiting for the procedure") & gender != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(gender, fill = doctor_refusals)) +
  geom_bar()

childfree %>% filter(sterilization_status %in% c("Yes", "No, but waiting for the procedure") & gender != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(gender, fill = doctor_refusals)) +
  geom_bar(position = position_fill())

#Women were more likely to be or want to be sterilized than men
childfree %>% filter(sterilization_desire != "" & gender != "") %>%
  ggplot(aes(gender, fill = sterilization_desire)) +
  geom_bar()

childfree %>% filter(sterilization_desire != "" & gender != "") %>%
  ggplot(aes(gender, fill = sterilization_desire)) +
  geom_bar(position = position_fill())

childfree %>% filter(sterilization_desire %in% c("Desired", "Sterilized") & gender != "") %>% 
  ggplot(aes(gender, fill = sterilization_desire)) + 
  geom_bar()

c_gender <- childfree %>% filter(gender != "")
C_man <- c_gender %>% filter(.$gender == "Man")
C_woman <- c_gender %>% filter(.$gender == "Woman")
mean(c_gender %>% .$sterilization_desire %in% c("Sterilized", "Desired"))
mean(C_man %>% .$sterilization_desire %in% c("Sterilized", "Desired"))
mean(C_woman %>% .$sterilization_desire %in% c("Sterilized", "Desired"))

#Women in the US were more likely to be or want to be sterilized than in other countries
childfree %>% filter(current_location != "" & sterilization_desire != "" & gender != "") %>%
  ggplot(aes(gender, fill = sterilization_desire)) +
  geom_bar() +
  facet_grid(.~US)

childfree %>% filter(current_location != "" & sterilization_desire != "" & gender != "") %>%
  ggplot(aes(gender, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_grid(.~US) + 
  theme(axis.text.x=element_text(angle= 25, hjust = 1))

c_curr_loc_woman <- childfree %>% filter(current_location != "") %>% filter(gender == "Woman")
C_US_woman <- c_curr_loc_woman %>% filter(.$US == "US")
C_Non_US_woman <- c_curr_loc_woman %>% filter(.$US == "Non US")
mean(c_curr_loc_woman %>% .$sterilization_desire %in% c("Sterilized", "Desired"))
mean(C_US_woman %>% .$sterilization_desire %in% c("Sterilized", "Desired"))
mean(C_Non_US_woman %>% .$sterilization_desire %in% c("Sterilized", "Desired"))

#People in relationships were more likely to desire sterilization than single people
c_relation <- childfree %>% filter(relationship_status != "" & sterilization_desire != "")

c_relation %>% ggplot(aes(relationship_status, fill = sterilization_desire)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 5, angle= 25, hjust = 1))

c_relation %>% ggplot(aes(relationship_status, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 5, angle= 25, hjust = 1))

c_relation %>% ggplot(aes(relation_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 8, angle= 10, hjust = 1))


#Where are the people located where sterilization is illegal?
c_illegal <- childfree %>% filter(sterilization_status == "No, but desired and not legal")

c_illegal %>% ggplot(aes(current_location)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_illegal %>% ggplot(aes(current_location, fill = relationship_status)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

#People under 21 rated it not legal, less based on location = FALSE
c_illegal %>% ggplot(aes(current_location, fill = age_group)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

#Correlation between Reddit activity level and introversion
c_activity <- childfree %>% filter(reddit_activity != "" & introversion_level != "")

c_activity %>% ggplot(aes(introversion_level, reddit_activity)) +
  geom_boxplot()

#Correlation between Reddit activity level and age
c_activity %>% filter(age_group != "") %>%
  ggplot(aes(reddit_activity, fill = age_group)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_activity %>% filter(age_group != "") %>%
  ggplot(aes(age_group, fill = reddit_activity)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_activity %>% filter(age_group != "") %>%
  ggplot(aes(age_group, fill = reddit_activity)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_activity %>% filter(age_group != "") %>%
  ggplot(aes(reddit_activity, fill = age_group)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_activity %>% filter(age_group != "") %>%
  ggplot(aes(age_group, fill = reddit_activity)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

#Correlation between Reddit activity level and realization age
c_activity %>% filter(childfree_age != "") %>%
  ggplot(aes(reddit_activity, fill = childfree_age)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_activity %>% filter(childfree_age != "") %>%
  ggplot(aes(childfree_age, fill = reddit_activity)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_activity %>% filter(childfree_age != "") %>%
  ggplot(aes(reddit_activity, fill = childfree_age)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_activity %>% filter(childfree_age != "") %>%
  ggplot(aes(childfree_age, fill = reddit_activity)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

#finding myself
childfree %>% filter(age_group == "19 to 24" & childfree_age == "18 years old or younger" & gender == "Woman" & sexual_orientation == "Heterosexual" & education == "Bachelor's degree" & nationality == "United States" & current_location == "United States" & ethnicity == "African Descent/Black" & current_industry == "Retail")

#Ethnicity and sterilization status 
ethin_ster <- childfree %>% filter(ethnicity_conden != "" & sterilization_status != "" & sterilization_desire != "" & sterilization_desire != "NA")

ethin_ster %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ethin_ster %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ethin_ster %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ethin_ster %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Ethnicity and doctor refusals, Minorities have an easier time 
ethin_ster %>% filter(doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(ethnicity_conden, fill = doctor_refusals)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ethin_ster %>% filter(doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(ethnicity_conden, fill = doctor_refusals)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ethin_ster %>% filter(doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(racial_minorities, fill = doctor_refusals)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ethin_ster %>% filter(doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet") %>%
  ggplot(aes(racial_minorities, fill = doctor_refusals)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#SES and sterilization status
SES_ster <- childfree %>% filter(SES != "" & sterilization_status != "" & sterilization_desire != "NA")
SES_ster %>% ggplot(aes(SES, sterilization_desire)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Childhood ratings and realization age 
childhood_real_age <- childfree %>% filter(happy_childhood != "NA" & childfree_age != "")
childhood_real_age %>% ggplot(aes(happy_childhood, childfree_age)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Those with tokophobia want to be or are sterilized more than those with other main reasons = TRUE
reason_ster_desire <- childfree %>% filter(childfree_main_reason != "" & sterilization_desire != "")

reason_ster_desire %>% ggplot(aes(childfree_main_reason, fill = sterilization_desire)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

reason_ster_desire %>% ggplot(aes(childfree_main_reason, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Job importance and child focused job 
job_import_child_job <- childfree %>% filter(!(is.na(job_importance)) & child_focused_job != "")

job_import_child_job %>% ggplot(aes(job_importance, child_focused_job)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Intersectionality with sterilization status
identity_ster <- childfree %>% filter(gender != "" & ethnicity_conden != "" & !(is.na(SES)) & sterilization_desire != "")
#Gender and ethnicity 
identity_ster %>% filter(gender %in% c("Man", "Woman")) %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~gender) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% filter(gender %in% c("Man", "Woman")) %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~gender) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% filter(gender %in% c("Man", "Woman")) %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~gender) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% filter(gender %in% c("Man", "Woman")) %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~gender) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Gender and SES
identity_ster %>% filter(gender %in% c("Man", "Woman")) %>% ggplot(aes(SES, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~gender) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% filter(gender %in% c("Man", "Woman")) %>% ggplot(aes(SES, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~gender) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
#SES and ethnicity 
identity_ster %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~SES) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~SES) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~SES) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~SES) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% ggplot(aes(SES, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~racial_minorities) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

identity_ster %>% ggplot(aes(SES, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~racial_minorities) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Childhood ratings and child focused job
happy_child_job <- childfree %>% filter(!(is.na(happy_childhood)) & child_focused_job != "")

happy_child_job %>% ggplot(aes(happy_childhood, child_focused_job)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Childhood faith and bingo frequency
chi_faith_bingo <- childfree %>% filter(childhood_faith != "" & bingo_frequency != "")

chi_faith_bingo %>% ggplot(aes(childhood_faith, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
  
chi_faith_bingo %>% ggplot(aes(childhood_faith, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
                           
#Current faith and bingo frequency
cur_faith_bingo <- childfree %>% filter(current_faith != "" & bingo_frequency != "")

cur_faith_bingo %>% ggplot(aes(current_faith, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

cur_faith_bingo %>% ggplot(aes(current_faith, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#current location and childfree location
location_childfree <- childfree %>% filter(current_location != "" & !(is.na(childfree_location.)))

location_childfree %>% ggplot(aes(childfree_location., current_location)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

location_childfree %>% 
  mutate(current_location = reorder(current_location, childfree_location., FUN = median)) %>%
  ggplot(aes(childfree_location., current_location)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#childfree location and bingo frequency
childfree_location_bingo <- childfree %>% filter(!(is.na(childfree_location.)) & bingo_frequency != "")

childfree_location_bingo %>% ggplot(aes(childfree_location., bingo_frequency)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#childfree location and Sterilization_time
childfree_loc_ster_time <- childfree %>% filter(!(is.na(childfree_location.)) & sterilization_time != "" & sterilization_time != "I haven't been sterilized yet")

childfree_loc_ster_time %>% ggplot(aes(childfree_location., sterilization_time)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#childfree location and Doctor_refusals
childfree_loc_doc_refusals <- childfree %>% filter(!(is.na(childfree_location.)) & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet")

childfree_loc_doc_refusals %>% ggplot(aes(childfree_location., doctor_refusals)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

childfree_loc_doc_refusals %>% ggplot(aes(childfree_location., doctor_refusals_num)) +
  geom_jitter(alpha = 0.2)

#Doctor_search_strat and Doctor_refusals
doc_strat_refusals <- childfree %>% filter(doctor_search_strat != "" & doctor_refusals != "" & doctor_refusals != "I haven't found a doctor willing to sterilize me yet")

doc_strat_refusals %>% ggplot(aes(doctor_refusals_num, doctor_search_strat)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Age and job importance
age_job_importance <- childfree %>% filter(age_group != "" & !(is.na(job_importance)))

age_job_importance %>% ggplot(aes(job_importance, age_group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#SES and job importance
ses_job_importance <- childfree %>% filter(!(is.na(SES)) & !(is.na(job_importance)))

ses_job_importance %>% ggplot(aes(job_importance, SES)) +
  geom_jitter(alpha = 0.2)

#Gender and bingo frequency
gender_bingo <- childfree %>% filter(gender != "" & bingo_frequency != "")


gender_bingo %>% ggplot(aes(gender, fill = bingo_frequency)) +
  geom_bar()

gender_bingo %>% ggplot(aes(gender, fill = bingo_frequency)) +
  geom_bar(position = position_fill())


gender_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(gender, fill = bingo_frequency)) +
  geom_bar()

gender_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(gender, fill = bingo_frequency)) +
  geom_bar(position = position_fill())

#Age group and bingo frequency
age_bingo <- childfree %>% filter(age_group != "" & bingo_frequency != "")


age_bingo %>% ggplot(aes(age_group, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

age_bingo %>% ggplot(aes(age_group, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))


age_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(age_group, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

age_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(age_group, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Relationship status and bingo frequency
relationship_bingo <- childfree %>% filter(relationship_status != "" & bingo_frequency != "")


relationship_bingo %>% ggplot(aes(relationship_status, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

relationship_bingo %>% ggplot(aes(relationship_status, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))


relationship_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(relationship_status, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

relationship_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(relationship_status, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#SES and bingo frequency
ses_bingo <- childfree %>% filter(!(is.na(SES)) & bingo_frequency != "")


ses_bingo %>% ggplot(aes(SES, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ses_bingo %>% ggplot(aes(SES, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))


ses_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(SES, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ses_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(SES, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Ethnicity and bingo frequency, higher for hispanics
ethnicity_bingo <- childfree %>% filter(ethnicity_conden != "" & bingo_frequency != "")


ethnicity_bingo %>% ggplot(aes(ethnicity_conden, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ethnicity_bingo %>% ggplot(aes(ethnicity_conden, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))


ethnicity_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(racial_minorities, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

ethnicity_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(racial_minorities, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
#Sexual orientation and bingo frequency
sexuality_bingo <- childfree %>% filter(sexual_orientation != "" & bingo_frequency != "")

sexuality_bingo %>% ggplot(aes(sexual_orientation, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

sexuality_bingo %>% ggplot(aes(sexual_orientation, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))


sexuality_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(sexual_orientation, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

sexuality_bingo %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(sexual_orientation, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Intersectionality and bingo frequency

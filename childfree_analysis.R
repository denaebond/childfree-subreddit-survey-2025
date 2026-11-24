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
gender_diverse <- c("Agender", "Non-binary", "Other")
childfree <- childfree %>% mutate(US = case_when(.$current_location == "United States" ~ "US", .$current_location != "United States" ~ "Non US")) %>% 
  mutate(sterilization_desire = 
           case_when(.$sterilization_status == "Yes" ~ "Sterilized", .$sterilization_status %in% c("No, but desired and looking for a doctor", "No, but desired and not looking for a doctor yet", "No, but waiting for the procedure", "No, but desired and not legal") ~ "Desired", .$sterilization_status %in% c("No, and not needed", "No, and not desired") ~ "Not Desired", .$sterilization_status %in% c("Sterile due to a medical procedure", "Naturally sterile") ~ "Already Sterile")) %>%
  mutate(relation_conden = case_when(.$relationship_status %in% c("Single and not looking", "Single and open to non-serious options", "Single, looking for something serious") ~ "Single", .$relationship_status == "Short term relationship" ~ "Short term relationship", .$relationship_status %in% c("Long term relationship, not living with together", "Long term relationship, living together", "Engaged", "Married") ~ "Long-Term, Serious", .$relationship_status %in% c("Separated", "Divorced", "Widowed") ~ "Previously Serious", .$relationship_status == "Other" ~ "Other")) %>%
  mutate(ethnicity_conden = case_when(.$ethnicity == "" ~ "", .$ethnicity == "African Descent/Black" ~ "African Descent/Black", .$ethnicity == "Arabic/Middle Eastern/Near Eastern" ~ "Arabic/Middle Eastern/Near Eastern", .$ethnicity == "Caucasian/White" ~ "Caucasian/White", .$ethnicity == "East Asian" ~ "East Asian", .$ethnicity == "Hispanic" ~ "Hispanic", .$ethnicity == "Indian/South Asian" ~ "Indian/South Asian", .$ethnicity == "Jewish (the ethnicity, not religion)" ~ "Jewish (the ethnicity, not religion)", .$ethnicity == "North American Indigenous or Alaskan Native" ~ "North American Indigenous or Alaskan Native", .$ethnicity == "Other" ~ "Other", .$ethnicity == "Pacific Islander/Melanesian" ~ "Pacific Islander/Melanesian", .$ethnicity == "South-East Asian" ~ "South-East Asian", !(.$ethnicity %in% mixed_ethnicity) ~ "Mixed Ethincity")) %>%
  mutate(doctor_refusals_num = case_when(.$doctor_refusals == "" ~ NA, .$doctor_refusals == "0" ~ 0, .$doctor_refusals == "1" ~ 1, .$doctor_refusals == "2" ~ 2,.$doctor_refusals == "3" ~ 3,.$doctor_refusals == "4" ~ 4, .$doctor_refusals == "5" ~ 5, .$doctor_refusals == "6" ~ 6, .$doctor_refusals == "8" ~ 8, .$doctor_refusals == "I haven't found a doctor willing to sterilize me yet" ~ NA, .$doctor_refusals == "10+" ~ 10)) %>%
  mutate(racial_minorities = case_when(.$ethnicity == "" ~ "", .$ethnicity == "Caucasian/White" ~ "Caucasian/White", !(.$ethnicity %in% mixed_ethnicity) ~ "Mixed Ethincity", .$ethnicity %in% minority_ethnicity ~ "Ethinc Minority")) %>%
  mutate(gender_conden = case_when(gender %in% gender_diverse ~ "Gender Diverse", gender == "Man" ~ "Man", gender == "Woman" ~ "Woman"))

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
  mutate(racial_minorities = factor(racial_minorities, ordered = FALSE)) %>%
  mutate(gender_conden = factor(gender_conden, ordered = FALSE))

complete_childfree <- childfree %>% filter(age_group != "" 
                                           & childfree_age != "" 
                                           & gender != "" 
                                           & sexual_orientation != "" 
                                           & education != "" 
                                           & job_importance != "" 
                                           & retired != "" 
                                           & !is.na(SES) 
                                           & old_age_plan != "" 
                                           & nationality != "" 
                                           & current_location != "" 
                                           & ethnicity != "" 
                                           & childhood_faith != "" 
                                           & current_faith != "" 
                                           & current_faith != "Jedi Knights"
                                           & religious_practice != "" 
                                           & relationship_status != "" 
                                           & partner_childfree != "" 
                                           &  !is.na(happy_childhood) 
                                           & sterilization_status != "" 
                                           & single_parent_option != "" 
                                           & childfree_main_reason != "" 
                                           & !is.na(child_feelings) 
                                           & child_focused_job != "" 
                                           & childfree_support != "" 
                                           & openly_childfree_loved_ones != "" 
                                           & !is.na(childfree_location.) 
                                           & bingo_frequency != "" 
                                           & pro.choice != "" 
                                           & financial_abortion != "" 
                                           & pet_ownership != "" 
                                           & introversion_level != "" 
                                           & reddit_activity != ""
                                           & major !=""
                                           & current_industry != ""
                                           & work_hours != ""
                                           & industry_position != "")
#skipped doctor shopping age, sterilization age, sterilization time, doctor refusals, doctor search strat


sterilized <- childfree %>% filter(sterilization_status == "Yes" 
                                   & doctor_refusals != "" 
                                   & doctor_refusals != "I haven't found a doctor willing to sterilize me yet")

complete_sterilized <- complete_childfree %>% filter(sterilization_status == "Yes" 
                                                     & doctor_refusals != "" 
                                                     & doctor_refusals != "I haven't found a doctor willing to sterilize me yet"
                                                     & doctor_shopping_age != ""
                                                     & sterilization_age != ""
                                                     & sterilization_time != ""
                                                     & doctor_search_strat != "")


#Those who realized they're childfree at a younger age are more likely to be sterilized
#partially true, more likely to be AND want to be sterilized, REVERSE IS TRUE until late 30s
complete_childfree %>% ggplot(aes(childfree_age, fill = sterilization_desire)) + 
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(angle= 25, hjust = 1))


#Those currently sterilized are mostly 30 or older at sterilization age 
#False, mostly 25 to 29
complete_sterilized %>% ggplot(aes(sterilization_age)) + 
  geom_bar()

#Those currently sterilized under 30 had to talk to more doctors 
#False; ages late 40s & late 20s had less initial rejections; 10+ in 20s & late 30s; early 40s most likely for multiple rejections
complete_sterilized %>% ggplot(aes(sterilization_age, fill = doctor_refusals)) +
  geom_bar(position = position_fill())

complete_sterilized %>% filter(doctor_refusals != "0") %>%
  ggplot(aes(sterilization_age, fill = doctor_refusals)) +
  geom_bar(position = position_fill())

#Women had to talk to more doctors than men  
#True; but gender diverse more likely to experience rejection generally & at higher levels
complete_sterilized %>% ggplot(aes(gender_conden, fill = doctor_refusals)) +
  geom_bar(position = position_fill())

#Women were more likely to be or want to be sterilized than men
#False; slightly more men 
complete_childfree %>% ggplot(aes(gender_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill())

complete_childfree %>% filter(sterilization_desire %in% c("Desired", "Sterilized")) %>% 
  ggplot(aes(gender_conden, fill = sterilization_desire)) + 
  geom_bar()

C_man <- complete_childfree %>% filter(.$gender == "Man")
C_woman <- complete_childfree %>% filter(.$gender == "Woman")
mean(complete_childfree %>% .$sterilization_desire %in% c("Sterilized", "Desired"))
mean(C_man %>% .$sterilization_desire %in% c("Sterilized", "Desired"))
mean(C_woman %>% .$sterilization_desire %in% c("Sterilized", "Desired"))

#Women in the US were more likely to be or want to be sterilized than in other countries = 
#True; 0.71 for US Women & 0.57 for Non-US Women
complete_childfree %>%
  ggplot(aes(gender_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_grid(.~US) + 
  theme(axis.text.x=element_text(angle= 25, hjust = 1))

c_curr_loc_woman <- complete_childfree %>% filter(current_location != "") %>% filter(gender == "Woman")
C_US_woman <- c_curr_loc_woman %>% filter(.$US == "US")
C_Non_US_woman <- c_curr_loc_woman %>% filter(.$US == "Non US")
mean(c_curr_loc_woman %>% .$sterilization_desire %in% c("Sterilized", "Desired"))
mean(C_US_woman %>% .$sterilization_desire %in% c("Sterilized", "Desired"))
mean(C_Non_US_woman %>% .$sterilization_desire %in% c("Sterilized", "Desired"))

#People in relationships were more likely to desire sterilization than single people
#True; STR most likely to desire, Previously serious & other most likely to be sterilized 
complete_childfree %>% ggplot(aes(relation_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 8, angle= 10, hjust = 1))


#Where are the people located where sterilization is illegal?
c_illegal <- childfree %>% filter(sterilization_status == "No, but desired and not legal")

c_illegal %>% ggplot(aes(current_location)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

c_illegal %>% ggplot(aes(current_location, fill = relation_conden)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

#People under 21 rated it not legal, less based on location = FALSE
c_illegal %>% ggplot(aes(current_location, fill = age_group)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

#Correlation between Reddit activity level and introversion
#highest median & lowest range for posters, lowest median & IQR for participants, widest range for participants & commenters
complete_childfree %>% ggplot(aes(introversion_level, reddit_activity)) +
  geom_boxplot()

#Correlation between Reddit activity level and age
#trend of becoming more active with age, mostly to commenting; % peaks at early 40s and late 50s
complete_childfree %>% filter(reddit_activity != "I don't know / I'm not sure") %>%
  ggplot(aes(age_group, fill = reddit_activity)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

complete_childfree %>% filter(reddit_activity != "I don't know / I'm not sure") %>%
  ggplot(aes(age_group, fill = reddit_activity)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

#Correlation between Reddit activity level and realization age
#trend of becoming more active with higher childfree age to late 30s, mostly to commenting; late 30s highest % of posters & participants
complete_childfree %>% filter(reddit_activity != "I don't know / I'm not sure") %>%
  ggplot(aes(childfree_age, fill = reddit_activity)) +
  geom_bar() + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

complete_childfree %>% filter(reddit_activity != "I don't know / I'm not sure") %>%
  ggplot(aes(childfree_age, fill = reddit_activity)) +
  geom_bar(position = position_fill()) + 
  theme(axis.text.x=element_text(size = 8, angle= 20, hjust = 1))

#Ethnicity and sterilization status 
#no big differences between white and minorites; 
#of minorities, Hispanics most likely to be sterilized 
#jewish people least likely, 
#middle easterners most likely to be already sterile  
complete_childfree %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>% 
  ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>%
  ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_childfree %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Ethnicity and doctor refusals, Minorities have an easier time 
#False, mixed people most likely to experience doctor rejection
#minorities most likely to have 10+ rejections
#middle eastern > hispanic > others > mixed > black > white > east asian
complete_sterilized %>% ggplot(aes(ethnicity_conden, fill = doctor_refusals)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_sterilized %>% ggplot(aes(ethnicity_conden, fill = doctor_refusals)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_sterilized %>% ggplot(aes(racial_minorities, fill = doctor_refusals)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_sterilized %>% ggplot(aes(racial_minorities, fill = doctor_refusals)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_sterilized %>% filter(racial_minorities == "Ethinc Minority") %>%
  ggplot(aes(ethnicity_conden, fill = doctor_refusals)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_sterilized %>% filter(racial_minorities == "Ethinc Minority") %>%
  ggplot(aes(ethnicity_conden, fill = doctor_refusals)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#SES and sterilization status
#biggest range for desired, shortest for already sterile
#largest IQR for already sterile, shortest for not desired
complete_childfree %>% ggplot(aes(SES, sterilization_desire)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Childhood ratings and realization age 
#lower median for 18 and early 40s, higher for late 20s to late 30s
complete_childfree %>% ggplot(aes(happy_childhood, childfree_age)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Those with tokophobia want to be or are sterilized more than those with other main reasons = TRUE
complete_childfree %>% ggplot(aes(childfree_main_reason, fill = sterilization_desire)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_childfree %>% ggplot(aes(childfree_main_reason, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Job importance and child focused job 
complete_childfree %>% ggplot(aes(job_importance, child_focused_job)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Intersectionality with sterilization status

#Gender and ethnicity 
#highest already sterile in gender diverse ethnic minorities
#mixed gender diverse most likely to be sterilized and want to be sterilized
#mixed women least likely to be sterilized 
complete_childfree %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~gender_conden) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~gender_conden) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>%
  ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~gender_conden) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>%
  ggplot(aes(ethnicity_conden, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~gender_conden) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~gender_conden) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_childfree %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~gender_conden) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Gender and SES
#women have descreasing desire for sterilization as SES increases
#steep increased rates of sterilization as SES increases for men and gender diverse
complete_childfree %>% ggplot(aes(SES, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~gender_conden) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_childfree %>% ggplot(aes(SES, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~gender_conden) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
#SES and ethnicity 
#increased rates of sterilization as SES increases for whites and mixed
#decreased desire for sterilization for minorities
#skiwed parabola of not desired for mixed

complete_childfree %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~SES) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(racial_minorities, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~SES) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(SES, fill = sterilization_desire)) +
  geom_bar() +
  facet_wrap(.~racial_minorities) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#!
complete_childfree %>% ggplot(aes(SES, fill = sterilization_desire)) +
  geom_bar(position = position_fill()) +
  facet_wrap(.~racial_minorities) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Childhood ratings and child focused job
#those in child-focused job self-report a higher median childhood happiness
complete_childfree %>% ggplot(aes(happy_childhood, child_focused_job)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#Childhood faith and bingo frequency
#raised atheist and catholic only report getting bingoed once a day or more
#raised pagan most unsure about bingoing
#(small sample)raised pagan and wiccan mostly to be bingod once a week 
#raised buddhist, orthodox, & agnostic top to be bingod once a week 
#(small sample) Jehovah witness, siknism, and taoist never reported being bingoed
#(small sample) followed by spiritualism, hinduism, then anglicanism to least likiely to report being bingoed
#hinduism to least likily to report being bingoed
complete_childfree %>% ggplot(aes(childhood_faith, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
  
complete_childfree %>% ggplot(aes(childhood_faith, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(childhood_faith, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(childhood_faith, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
                           
#Current faith and bingo frequency
#currently catholics and none only report getting bingoed once a day or more
#hindus, occustists, and protestants most unsure about bingoing
#jews, wiccans, then pagans mostly to be bingod once a week 
#(small sample) Jainism, sikhism, and taoism never reported being bingoed
#(small sample) followed by islam, catholics, spiritalism to least likily to report being bingoed
complete_childfree %>% ggplot(aes(current_faith, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

top_current_faith <- c("Agnosticism", "Atheism", "None")

complete_childfree %>% filter(!current_faith %in% top_current_faith) %>%
  ggplot(aes(current_faith, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(current_faith, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
#!
complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(current_faith, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
#!
complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  filter(!current_faith %in% top_current_faith) %>%
  ggplot(aes(current_faith, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(current_faith, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#current location and childfree location
#without sample size filter, 
#Thailand, South Korea, Bahrin, Bahamas top median at 10
#Russia at bottom at 1
#with sample size filter,
#Argentina and Finland top median at 9
#Romania at bottom at 3
#US and India have widest range 
#Spain and Austria have narrowest range 
complete_childfree %>% ggplot(aes(childfree_location., current_location)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

complete_childfree %>% 
  mutate(current_location = reorder(current_location, childfree_location., FUN = median)) %>%
  ggplot(aes(childfree_location., current_location)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

complete_childfree %>%
  group_by(current_location) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(current_location = reorder(current_location, childfree_location., FUN = median)) %>%
  ggplot(aes(childfree_location., current_location)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

complete_childfree %>% filter(current_location != "United States") %>%
  ggplot(aes(current_location)) + geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

sort(complete_childfree$current_location)

complete_childfree %>% arrange(.$current_location) %>%
  filter(current_location != "United States" & current_location != "United Kingdom" & current_location != "Canada") %>%
  ggplot(aes(current_location)) + geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% group_by(current_location)

complete_childfree %>% table(.$current_location)

complete_childfree %>% ggplot(aes(current_location)) + geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#childfree location and bingo frequency
#Inverse relationship; Decreasing median childfree location rating with increasing bingo frequency
#
#those unsure have similar boxplot to those bingod once every three & six months
complete_childfree %>% ggplot(aes(childfree_location., bingo_frequency)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#childfree location and Sterilization_time
#partially postive relationship of medians; 3> months to 30 months around 8 then decreases for >30 months or longer
#highest median for 12-18 months; lowest median for 7+ years
#widest IQR for 3> months & 7+ years; narrowest IQR for 6-9 months
#widest range for 3> months & 3-6 months; narrowest range for 12-18 months
complete_sterilized %>% ggplot(aes(childfree_location., sterilization_time)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#childfree location and Doctor_refusals
#not a consistant pattern between groups as refusals increases
#widest range for 0-2 refusals
#widest IQR for 2 refusals
#narrowest IQR for 10+
#highest median between 0, 2 & 5 refusals
complete_sterilized %>% ggplot(aes(childfree_location., doctor_refusals)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

complete_sterilized %>% ggplot(aes(childfree_location., doctor_refusals_num)) +
  geom_jitter(alpha = 0.2)

#Doctor_search_strat and Doctor_refusals
#those that used doctor referals, insurance/gov/chairity recs, or other online lists experienceed 10+ refusals
#those that used r/childfree list, other soucres, or other online lists experienceed 5 refusals
#those that used doctor referals followed by search engines were least likely to experince doctor refusals
#those that used facebook groups were most likely to experince doctor refusals
complete_sterilized %>% ggplot(aes(doctor_refusals_num, doctor_search_strat)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

complete_sterilized %>% ggplot(aes(doctor_search_strat, fill = doctor_refusals)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_sterilized %>% ggplot(aes(doctor_search_strat, fill = doctor_refusals)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_sterilized %>% ggplot(aes(doctor_refusals, fill = doctor_search_strat)) +
  geom_bar(position = position_fill())

#Gender and bingo frequency
#only men report getting bingoed once a day or more
#men highest % to never report being bingoed
#gender diverse most likely to report being bingoed at all
#gender diverse most likely to report being bingoed once a week & once a month
#women most certain if they get bingoed or not
complete_childfree %>% ggplot(aes(gender, fill = bingo_frequency)) +
  geom_bar()

complete_childfree %>% ggplot(aes(gender, fill = bingo_frequency)) +
  geom_bar(position = position_fill())

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(gender, fill = bingo_frequency)) +
  geom_bar()

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(gender, fill = bingo_frequency)) +
  geom_bar(position = position_fill())

complete_childfree %>% ggplot(aes(gender_conden, fill = bingo_frequency)) +
  geom_bar()
#!
complete_childfree %>% ggplot(aes(gender_conden, fill = bingo_frequency)) +
  geom_bar(position = position_fill())


complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(gender_conden, fill = bingo_frequency)) +
  geom_bar()
#!
complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(gender_conden, fill = bingo_frequency)) +
  geom_bar(position = position_fill())

#Age group and bingo frequency
#more sure of bingoing as age increases, then jumps late 60s and later
#only late 20s & early 30s report bingod once a day or more
#increase in reports of bingoed once a week or more up to late 50s
#decrease in reports of never being bingoed up to early 50s
#increase in reports of bingoed once a year or less up to early 60s
complete_childfree %>% ggplot(aes(age_group, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(age_group, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(age_group, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(age_group, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Relationship status and bingo frequency
#single people most likely to report never being bingoed
#all previously serious report being bingoed
#only long-term serious report being bingoed once a day or more
#STR most likely to report being bingoed once a week or more
#STR most likely to report being bingoed once a month or more
#STR most likely to report being bingoed once every 3 months or more
complete_childfree %>% ggplot(aes(relationship_status, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(relationship_status, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(relation_conden, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(relation_conden, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(relationship_status, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(relationship_status, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(relation_conden, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(relation_conden, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#SES and bingo frequency
#inverse relationship of SES & uncertainity of bingoing
#inverse relationship of SES & never being bingoed 
complete_childfree %>% ggplot(aes(SES, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(SES, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(SES, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(SES, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Ethnicity and bingo frequency 
#minorities least likely to bingoed & most likely to unsure able being bingoed
#whites most likely to be bingoed & only group to report being bingoed for once a day or more
#jewish & NA Native only minorites always to be certain if being bingoed or not
#indian/south asian & middle eastern minorites most likely to be never be bingoed
#jewish most likely to report being bingoed once a week or more 
complete_childfree %>% ggplot(aes(racial_minorities, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(racial_minorities, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(racial_minorities, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(racial_minorities, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>% 
  ggplot(aes(ethnicity_conden, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>% 
  ggplot(aes(ethnicity_conden, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>% 
  ggplot(aes(ethnicity_conden, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>% 
  filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(ethnicity_conden, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(racial_minorities == "Ethinc Minority") %>% 
  filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(ethnicity_conden, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

#Sexual orientation and bingo frequency
#only ace and bi people reported being bingoed once a day or more
#other sexualities most unsure about being bingoed
#Ace and gay people most likely to be report never being bingoed 
#gay and others most likely to being bingoed once a week or more
complete_childfree %>% ggplot(aes(sexual_orientation, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% ggplot(aes(sexual_orientation, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>% 
  ggplot(aes(sexual_orientation, fill = bingo_frequency)) +
  geom_bar() +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))

complete_childfree %>% filter(bingo_frequency != "I'm not sure whether or not I get bingod.") %>%
  ggplot(aes(sexual_orientation, fill = bingo_frequency)) +
  geom_bar(position = position_fill()) +
  theme(axis.text.x=element_text(size = 5, angle= 20, hjust = 1))
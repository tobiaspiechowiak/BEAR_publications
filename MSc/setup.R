##
#libname <- "immavarablestoptryingtofindmeaslibrarryyoudingdong"
requireLibrary <- function(libname) {
    if (!require(libname, character.only = TRUE)) {
        install.packages(libname)
        library(libname, character.only = TRUE)
    }
}

requireLibrary("ggpmisc")
requireLibrary("caret")
requireLibrary("reshape2")
requireLibrary("gridExtra")
requireLibrary("plotly")
requireLibrary("ggmosaic")
requireLibrary("corrgram")
requireLibrary("party")
requireLibrary("tidyverse")
requireLibrary("RColorBrewer")
requireLibrary("scales")

<<<<<<< HEAD
if(!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
}
datafolder <- "../../Data/data/MsC"


=======
datafolder <- "E:/Documents/Bear data"
>>>>>>> 548a9c3853f899a04753e1dbd68eb0933a4ce97a
files <- c("15D-baseline.csv",
           "15D-followUP.csv",
           "Audiogram_left_classified.csv",
           "Audiogram_right_classified.csv",
           "Demographics.csv",
           "Fitting_rationale.csv",
           "HA_experience_use-time.csv",
           "SSQ12_IOI-HA_baseline.csv",
           "SSQ12_IOI-HA_visit_2.csv",
           "Low-benefit3rd.csv",
           "Motivation.csv",
           "Person-characteristics.csv",
           "QoL-baseline+follow-up.csv",
           "SSQ-baseline.csv",
           "SSQ-follow-up.csv",
           "SSQ_age_gender.csv",
           "THI-baseline.csv",
           "THI-follow-up.csv",
           "Tid-mellem.csv",
           "Tinnitus.csv")

#names(files) <- c("15d-1","15d-2","audiogram-l","audiogram-r","demo","fitting","ha-exp","ssq+ioi-1","ssq+ioi-2","low-benefit","motivation","person-stats","qol-1+2","")

fa_sex <- c("Male", "Female")
fa_fin <- c("Incomplete", "Unverified", "Complete")
fa_brands <- c("Oticon","Bernafon","Widex","GN ReSound", "Rexton", "Siemens/Sivantos", "Phonak","Unitron","Danacom","Other")


#15d
df_15d <- rbind(read.csv(paste(datafolder,"15D-baseline.csv",sep="/")),
                read.csv(paste(datafolder,"15D-followup.csv",sep="/")))
df_15d$d_complete <- factor(df_15d$d_complete, levels = 0:2, labels = fa_fin)
df_15d[,3:17] <- lapply(df_15d[,3:17], factor, 
                             levels=c(1,2,3,4,5), 
                             ordered = TRUE)

df_15d_base <- filter(df_15d, redcap_event_name == "baseline_arm_1")
df_15d_follow <- filter(df_15d, redcap_event_name == "besoeg2_arm_1")

#motivation
df_motivation <- read.csv(paste(datafolder,"Motivation.csv",sep="/"), colClasses = c(rep('factor', 3),rep('integer', 3),'factor'))#2/19
df_motivation$sex <- factor(df_motivation$sex, levels = 1:2, labels=fa_sex)
df_motivation$own_ha <- factor(df_motivation$own_ha, levels=0:1, labels=c("No", "Yes"))


#audiogram
df_audiogram_left <- read.csv(paste(datafolder, "Audiogram_left_classified.csv", sep = "/")) #14/19
df_audiogram_left$Gender <- factor(df_audiogram_left$Gender, levels = c(1,0), labels = fa_sex)
colnames(df_audiogram_left)[colnames(df_audiogram_left)=="ID"] <- "record_id"
df_audiogram_left$X <- NULL
df_audiogram_left$Side <- "left"

df_audiogram_right <- read.csv(paste(datafolder, "Audiogram_right_classified.csv", sep = "/")) #15/19
colnames(df_audiogram_right)[colnames(df_audiogram_right)=="AGE"] <- "Age"
df_audiogram_right$Gender <- factor(df_audiogram_right$Gender, levels = c(1,0), labels = fa_sex)
colnames(df_audiogram_right)[colnames(df_audiogram_right)=="ID"] <- "record_id"
df_audiogram_right$X <- NULL
df_audiogram_right$Side <- "right"

df_audiogram <- rbind(df_audiogram_left, df_audiogram_right)
df_audiogram$Side <- factor(df_audiogram$Side)

#redundant data?
#df_person_characteristics <- read.csv(paste(datafolder,files[11],sep="/"))#18/19 #identisk med df_motivation
#df_demographics <- read.csv(paste(datafolder,files[5],sep="/"))#19/19

#fitting rationale
df_fitting_rationale <- read.csv(paste(datafolder, "Fitting_rationale.csv", sep = "/")) #12/19
#Noget stemmer ikke overnes med hensyn til manufaktør, (måske?)
df_fitting_rationale$fittingrationale <- 
  factor(df_fitting_rationale$fittingrationale,
         levels = c(1:6),
         labels = c("Firmarationale",
                    "NAL-NL2",
                    "NAL-NL1",
                    "DSL v.5 til voksen",
                    "DSL v.5 til børn",
                    "Andet"))
df_fitting_rationale$ha_manufactor <- 
  factor(df_fitting_rationale$ha_manufactor, 
         levels = c(1:10), 
         labels = fa_brands)
df_fitting_rationale$ha_model2 <-
  factor(df_fitting_rationale$ha_model2,
         levels = c(1:10),
         labels = c("D220","D330","D440","C220","C330","C440","M220","M330","M440","Other"))
df_fitting_rationale$ha_model3 <-
  factor(df_fitting_rationale$ha_model3,
         levels = c(1:7),
         labels = c("Agil","Alta","Alta Pro","Alta Pro2","Chili","Sensei Pro","Other"))
df_fitting_rationale$ha_model4 <-
  factor(df_fitting_rationale$ha_model4,
         levels = c(1:4))#, labels = ?)
df_fitting_rationale$ha_model5 <-
  factor(df_fitting_rationale$ha_model5,
         levels = c(1:7))#, labels = ?)
df_fitting_rationale$ha_model6 <-
  factor(df_fitting_rationale$ha_model6)
#levels = c(1:7), labels = ?)
df_fitting_rationale$ha_model7 <-
  factor(df_fitting_rationale$ha_model7,
         levels = c(1:10))#, labels = ?)
df_fitting_rationale$ha_type <- 
  factor(df_fitting_rationale$ha_type, 
         levels = c(1:6), 
         labels = c("CIC","ITC/ITE","BTE (RITE)","BTE (tyndslange)","BTE (m. øreprop)","BTE (power)"))
df_fitting_rationale$ha_type_mm_complete <-
  factor(df_fitting_rationale$ha_type_mm_complete,
         levels = c(1:3),
         labels = fa_fin)

#ha experince use time
df_ha_use <- read.csv(paste(datafolder, "HA_experience_use-time.csv", sep = "/" )) #13/19
df_ha_use$sex <- 
  factor(df_ha_use$sex, 
         levels=c(1,2),
         labels = fa_sex)
df_ha_use$own_ha <- 
  factor(df_ha_use$own_ha, 
         levels=c(0,1), 
         labels = c("No", "Yes"))
df_ha_use$ha_number_of_ha <- 
  factor(df_ha_use$ha_number_of_ha, 
         levels=c(1:3), 
         labels = c("Both ears","Right ear","Left ear"))
df_ha_use$ha_dispencer <- 
  factor(df_ha_use$ha_dispencer, 
         levels=c(1:4), 
         labels = c("Høreklinikken, OUH","Audiologisk afdeling, Ålborg","Anden offentlig høreklinik", "Privat leverandør"))

#ssq + ioi
df_ssq_ioi <- rbind(read.csv(paste(datafolder,"SSQ12_IOI-HA_visit_2.csv",sep="/")))
df_ssq_ioi[,16:22] <- lapply(df_ssq_ioi[,16:22], factor, 
                       levels=c(0,1,2,3,4), 
                       ordered = TRUE)
df_ssq_ioi$ssq12_dk_complete <- factor(df_ssq_ioi$ssq12_dk_complete, levels = c(0,1,2), labels = fa_fin)
df_ssq_ioi$ioiha_dk_complete <- factor(df_ssq_ioi$ioiha_dk_complete, levels = c(0,1,2), labels = fa_fin)

colnames(df_ssq_ioi)[colnames(df_ssq_ioi)=="ssq_sppech_q1"] <- "ssq_speech_q1"

df_ssq.wider <- rbind(read.csv(paste(datafolder,"SSQ-baseline.csv",sep="/")), read.csv(paste(datafolder,"SSQ-follow-up.csv",sep="/")))
df_ssq <- select(df_ssq_ioi, record_id, redcap_event_name, ssq_speech_q1, ssq_speech_q4, ssq_speech_q10, ssq_speech_q11, ssq_speech_q12, ssq_space_q6, ssq_space_q9, ssq_space_q13, ssq_sound_q2, ssq_sound_q7, ssq_sound_q9, ssq_sound_q14, ssq12_dk_complete)

df_ssq_base <- filter(df_ssq, redcap_event_name == "baseline_arm_1")
df_ssq_follow <- filter(df_ssq, redcap_event_name == "besoeg2_arm_1")

ssq_diff <- df_ssq %>% 
  filter(ssq12_dk_complete == "Complete") %>% 
  filter(record_id %in% df_ssq_follow$record_id) %>% group_by(record_id) %>% 
  summarise_each(funs(if(is.numeric(.)) last(.)-first(.) else first(.)))

df_use_group <- df_ha_use %>% filter(!is.na(ha_usetime_hours_per_day)) %>% mutate(use_group = cut(x=ha_usetime_hours_per_day, breaks=c(0,1,4,8,24), labels = c("Very low(<1)","Low(1-4)", "Medium(4-8)", "High(>8)"), include.lowest = TRUE))

df_ssq_diff_group <- merge(ssq_diff, df_use_group[,c(1, 11)], by='record_id')
df_ssq_diff_group <- filter(df_ssq_diff_group, !is.na(use_group))
df_ioi <- select(df_ssq_ioi, record_id, redcap_event_name, ioi_ha_1, ioi_ha_2, ioi_ha_3, ioi_ha_4, ioi_ha_5, ioi_ha_6, ioi_ha_7, ioiha_dk_complete)

df_ssq_diff_group <- merge(df_ssq_diff_group, df_ioi, by=c('record_id', 'redcap_event_name'))


df_ssq$ssq_speech_mean <- rowMeans(df_ssq[,grepl("ssq_speech", colnames(df_ssq))], na.rm = TRUE)
df_ssq$ssq_space_mean <- rowMeans(df_ssq[,grepl("ssq_space", colnames(df_ssq))], na.rm = TRUE)
df_ssq$ssq_sound_mean <- rowMeans(df_ssq[,grepl("ssq_sound", colnames(df_ssq))], na.rm = TRUE)


ssq_diff$ssq_speech_mean <- rowMeans(ssq_diff[,grepl("ssq_speech", colnames(ssq_diff))], na.rm = TRUE)
ssq_diff$ssq_space_mean <- rowMeans(ssq_diff[,grepl("ssq_space", colnames(ssq_diff))], na.rm = TRUE)
ssq_diff$ssq_sound_mean <- rowMeans(ssq_diff[,grepl("ssq_sound", colnames(ssq_diff))], na.rm = TRUE)

#ioi
#df_benefit_ioi <- read.csv(paste(datafolder, "Low-benefit3rd.csv", sep = "/"), colClasses = c("factor", "factor", "numeric",  rep('factor', 9), rep("numeric", 4))) #5/19
df_ioi <- select(df_ssq_ioi, record_id, redcap_event_name, ioi_ha_1, ioi_ha_2, ioi_ha_3, ioi_ha_4, ioi_ha_5, ioi_ha_6, ioi_ha_7, ioiha_dk_complete)
df_ioi_base <- filter(df_ioi, redcap_event_name == "baseline_arm_1")
df_ioi_follow <- filter(df_ioi, redcap_event_name == "besoeg2_arm_1")

#df_ioi[,3:9] <- lapply(df_ioi[,3:9], factor, 
#                       levels=c(0,1,2,3,4), 
#                       ordered = TRUE)

#time
df_time_between <- read.csv(paste(datafolder,"Tid-mellem.csv", sep="/"))#1/19
df_time_between$visit_date_ha_delivery <- as.Date(df_time_between$visit_date_ha_delivery, format = "%Y-%m-%d")
df_time_between$visit_date_2 <- as.Date(df_time_between$visit_date_2, format = "%Y-%m-%d")

df_time_between <- mutate(df_time_between, interval = visit_date_2 - visit_date_ha_delivery)

#Tinnitus (THI) (ikke testet)
df_thi <- rbind(read.csv(paste(datafolder,"THI-baseline.csv",sep="/")), 
                read.csv(paste(datafolder,"THI-follow-up.csv",sep="/")))

## The column for whether or not people have tinnitus is all na in the followup.
## If we assume that nobody gained or lost tinnitus between visits, we can just copy that over.
## I checked, and everybody that has a followup has a baseline, and when we filter out the record_ids 
## from the baseline that are not in the followup, the order is the same. Therefore the below lines should work.
df_thi[df_thi$record_id%in%df_thi[df_thi$redcap_event_name=="besoeg2_arm_1",]$record_id&df_thi$redcap_event_name=="besoeg2_arm_1",]$tinnitus <-
  df_thi[df_thi$redcap_event_name=="baseline_arm_1",][df_thi[df_thi$redcap_event_name=="baseline_arm_1",]$record_id%in%df_thi[df_thi$redcap_event_name=="besoeg2_arm_1",]$record_id,c("record_id","tinnitus")]$tinnitus


df_thi$tinnitus <- factor(df_thi$tinnitus,
                                   levels = c(0,1,999), 
                                   labels = c("No", "Yes", "Don't know")) # Ødelægger det her tinnitus (ja,nej,måske) spørgsmålet for followup? Den er na for alle sammen :/

                        
df_thi[,4:28] <- lapply(df_thi[,4:28], factor, 
                                 levels=c(0,1,2), 
                                 labels = c("No", "Yes", "Occasionally"))

# Erstat alle na med det samme? no no, later
#df_thi <- df_thi %>% mutate_at(vars(starts_with("tinnitus_"), -ends_with("complete")), function(x)replace_na(x,"No"))

# udregn Score
 df_thi <- df_thi[,1:29] %>% melt(id.vars=names(df_thi[,c(1:3, 29)])) %>% 
   group_by(record_id, redcap_event_name, value) %>% 
   summarise(nes=n()) %>% spread(value, nes, fill=0) %>% 
   group_by(redcap_event_name, record_id) %>% 
   mutate(Score = No*0+Occasionally*2+Yes*4) %>%
   summarise(Score = sum(Score)) %>%
   merge(df_thi, by=c("redcap_event_name", "record_id"))

df_thi$tinnitus_complete <- factor(df_thi$tinnitus_complete,
                                            levels = c(0,1,2), 
                                            labels = fa_fin)

#df_thi <- mutate(df_thi, THI_grade=cut(Score, 
#                                      breaks=c(0,16,36,56,76,100),
#                                      labels=c("Very mild", "Mild", "Moderate", "Severe", "Catastrofic"),
#                                      include.lowest = TRUE))

df_thi_baseline <- filter(df_thi, redcap_event_name == "baseline_arm_1")
df_thi_followup <- filter(df_thi, redcap_event_name == "besoeg2_arm_1")


#tinnitus noise
df_tinnitus <- read.csv(paste(datafolder, "Tinnitus.csv", sep="/")) #7/19
df_tinnitus$sex <- factor(df_tinnitus$sex,
                          levels = c(1,2), 
                          labels = fa_sex)

df_tinnitus[,5:8] <- lapply(df_tinnitus[,5:8], factor, 
                            levels = c(0,1,999), 
                            labels = c("No", "Yes", "Don't know"))
df_tinnitus$noise_industry_1 <- factor(df_tinnitus$noise_industry_1,
                                       levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13), 
                                       labels = c("sund/plej/omso", "unde/fors/pæda", "rest/køkk/reng", "admi/kont", "hånd/kons/anlæg", "mech/mont/oper", "tran/lage/reno", "hand/serv/deta", "mili", "poli/redn", "føde/indu", "land/fisk/skov/dyr", "andet"))

df_tinnitus$noise_type_1 <- factor(df_tinnitus$noise_type_1,
                                   levels = c(1,2,3,4), 
                                   labels = c("Impulse", "Continous", "Tonal", "Jævnt variende i intensitet"))

df_tinnitus$noise_work_intensity_1 <- factor(df_tinnitus$noise_work_intensity_1,
                                             levels = c(1,2,3,4), 
                                             labels = c("Extremely loud", "Very loud", "Loud", "Annoying"),
                                             ordered = TRUE)

df_tinnitus$noise_work_duration_1 <- factor(df_tinnitus$noise_work_duration_1,
                                            levels = c(1,2,3), 
                                            labels = c("All day", "Approx. half day", "Less than half day"),
                                            ordered = TRUE)

df_tinnitus$alcohol_consumption <- factor(df_tinnitus$alcohol_consumption,
                                          levels = c(1,2,3,4,5,6), 
                                          labels = c("None", "Rarely", "1-2/week", "3-7/week", "8-12/week", "More"),
                                          ordered = TRUE)

df_tinnitus$smoking_number_of_cigarets <- factor(df_tinnitus$smoking_number_of_cigarets,
                                                 levels = c(1,2,3,4,5,6), 
                                                 labels = c("None", "Rarely", "1-2/day", "2-10/day", "10-20/day", "More"),
                                                 ordered = TRUE)

# drawer user

sort_class_severity <- df_audiogram[,4:10] %>% melt() %>% group_by(Class, variable) %>% dplyr::summarise(value = mean(value)) %>% group_by(Class) %>% summarise(value=mean(value*value))

sorted_classes <- sort_class_severity[order(sort_class_severity$value),]$Class

df_audiogram$Class <- factor(df_audiogram$Class, levels = sorted_classes, ordered = TRUE)

df_user_worstear <- df_audiogram %>% group_by(record_id) %>% summarise(class=max(Class))

ids.t1 <- df_user_worstear %>% merge(df_audiogram, by = "record_id") %>% merge(df_ha_use, by=("record_id")) %>% filter(ha_usetime_hours_per_day <= 1)
ids.t3 <- df_user_worstear %>% merge(df_audiogram, by = "record_id") %>% merge(df_ha_use, by=("record_id")) %>% filter(class > "S1" & ha_usetime_hours_per_day < 3)

t3.data.ssq <- df_ssq %>% mutate(IsT1DrawerUser=factor(record_id %in% ids.t1$record_id),
                                 IsT3DrawerUser=factor(record_id %in% ids.t3$record_id),
                                 IsDrawerUser=factor(ifelse(record_id %in% ids.t3$record_id, 3, ifelse(record_id %in% ids.t1$record_id, 2, 1)), 
                                                     levels = 1:3, 
                                                     labels = c("No", "Yes", "Yes/t3"), ordered = TRUE))
t3.data.thi <- df_thi %>% mutate(IsT1DrawerUser=factor(record_id %in% ids.t1$record_id),
                                 IsT3DrawerUser=factor(record_id %in% ids.t3$record_id),
                                 IsDrawerUser=factor(ifelse(record_id %in% ids.t3$record_id, 3, ifelse(record_id %in% ids.t1$record_id, 2, 1)), 
                                                     levels = 1:3, 
                                                     labels = c("No", "Yes", "Yes/t3"), ordered = TRUE))
t3.data.15d <- df_15d %>% mutate(IsT1DrawerUser=factor(record_id %in% ids.t1$record_id),
                                 IsT3DrawerUser=factor(record_id %in% ids.t3$record_id),
                                 IsDrawerUser=factor(ifelse(record_id %in% ids.t3$record_id, 3, ifelse(record_id %in% ids.t1$record_id, 2, 1)), 
                                                     levels = 1:3, 
                                                     labels = c("No", "Yes", "Yes/t3"), ordered = TRUE))




<<<<<<< HEAD


=======
## Removing missing values
# SSQ
ssq_means_naomit <- t3.data.ssq %>% filter(!is.na(.$ssq_speech_mean) & !is.na(.$ssq_space_mean) & !is.na(.$ssq_sound_mean)) # filter out the worst na's (the ones where an entire category of questions where na)
a <- ssq_means_naomit[,16:18] %>% kmeans(40) # cluster them by the mean (less problems with na's)
ssq_means_naomit$Cluster <- a$cluster
ssq_means_naomit <- ssq_means_naomit %>% 
  group_by(Cluster) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x),mean(x, na.rm = T), x)) # replace all na values with the mean of the column in the cluster
t3.data.ssq.no.na <- ssq_means_naomit[, names(ssq_means_naomit) != "Cluster"]

# THI
#replace all na's with "No", since there aren't that many, that aren't just people without tinnitus(where it would be no anyway)
t3.data.thi.no.na <- t3.data.thi %>%
  mutate(tinnitus, tinnitus=replace_na(tinnitus,"No")) %>% 
  mutate_at(vars(starts_with("tinnitus_"), -ends_with("complete")), function(x)replace_na(x,"No"))

t3.data.thi.no.na$Score <- NULL
t3.data.thi.no.na$THI_grade <- NULL

t3.data.thi.no.na <- t3.data.thi.no.na %>%
  select(record_id, redcap_event_name, starts_with("tinnitus_"),-ends_with("complete")) %>%
  melt(id.vars=c("record_id", "redcap_event_name")) %>% 
  group_by(record_id, redcap_event_name, value) %>% summarise(nes=n()) %>%
  spread(value, nes, fill=0) %>% group_by(redcap_event_name, record_id) %>%
  mutate(Score = No*0+Occasionally*2+Yes*4) %>%
  summarise(Score = sum(Score)) %>%
  mutate(THI_grade=cut(Score, 
                       breaks=c(0,16,36,56,76,100),
                       labels=c("Very mild", "Mild", "Moderate", "Severe", "Catastrofic"),
                       include.lowest = TRUE)) %>% 
  merge(t3.data.thi.no.na, by=c("redcap_event_name", "record_id"))

# 15D

# Tinnitus/noise

# Ha use

# 
>>>>>>> 548a9c3853f899a04753e1dbd68eb0933a4ce97a

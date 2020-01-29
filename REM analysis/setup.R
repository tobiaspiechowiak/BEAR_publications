##

library(reshape2)
library(corrgram)
library(party)
library(ggmosaic)
library(gridExtra)
library(plotly)


if(!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
}
datafolder <- "../../Data/data/MsC"  #needs to be updated
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
fa_brands <- c("Oticon","GN ReSound","Widex","Bernafon", "Rexton", "Siemens/Sivantos", "Phonak","Unitron","Danacom","Other")


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

df_ssq_base <- filter(df_ssq.wider, redcap_event_name == "baseline_arm_1")
df_ssq_follow <- filter(df_ssq.wider, redcap_event_name == "besoeg2_arm_1")

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

#ioi
#df_benefit_ioi <- read.csv(paste(datafolder, "Low-benefit3rd.csv", sep = "/"), colClasses = c("factor", "factor", "numeric",  rep('factor', 9), rep("numeric", 4))) #5/19
df_ioi <- select(df_ssq_ioi, record_id, redcap_event_name, ioi_ha_1, ioi_ha_2, ioi_ha_3, ioi_ha_4, ioi_ha_5, ioi_ha_6, ioi_ha_7, ioiha_dk_complete)
df_ioi_base <- filter(df_ioi, redcap_event_name == "baseline_arm_1")
df_ioi_follow <- filter(df_ioi, redcap_event_name == "besoeg2_arm_1")

df_ioi_base <- rbind(read.csv(paste(datafolder,"SSQ12_IOI-HA_baseline.csv",sep="/")))
df_ioi_base[,16:22] <- lapply(df_ioi_base[,16:22], factor, 
                             levels=c(0,1,2,3,4), 
                             ordered = TRUE)
df_ioi_base <- select(df_ioi_base, record_id, redcap_event_name, ioi_ha_1, ioi_ha_2, ioi_ha_3, ioi_ha_4, ioi_ha_5, ioi_ha_6, ioi_ha_7, ioiha_dk_complete)

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
df_thi$tinnitus <- factor(df_thi$tinnitus,
                                   levels = c(0,1,999), 
                                   labels = c("No", "Yes", "Don't know"))

a.fun <- function(i){
  if(is.na(i)) return(NA)
  if(i==0) return(0)
  if(i==1) return(4)
  if(i==2) return(2)
  return(NA)
}

df_thi$Score <- NA
for(row in 1:nrow(df_thi)){
  if(df_thi$tinnitus_complete[row] == 0) {
    df_thi[row,30] <- NA
  } else {
    num <- 0
    for(col in 4:28){
      if(!is.na(a.fun(df_thi[row,col])))
      {
        num <- num + a.fun(df_thi[row,col])
      }
    }
    df_thi$Score[row] <- num
  }
}
                        
df_thi[,4:28] <- lapply(df_thi[,4:28], factor, 
                                 levels=c(0,1,2), 
                                 labels = c("No", "Yes", "Occasionally"))

df_thi$tinnitus_complete <- factor(df_thi$tinnitus_complete,
                                            levels = c(0,1,2), 
                                            labels = fa_fin)

df_thi <- mutate(df_thi, THI_grade=cut(Score, 
                                      breaks=c(0,16,36,56,76,100),
                                      labels=c("Very mild", "Mild", "Moderate", "Severe", "Catastrofic"),
                                      include.lowest = TRUE))

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

df_tinnitus$smoking_number_of_cigarets <- factor(df_tinnitus$alcohol_consumption,
                                                 levels = c(1,2,3,4,5,6), 
                                                 labels = c("None", "Rarely", "1-2/day", "2-10/day", "10-20/day", "More"),
                                                 ordered = TRUE)

# drawer user

sort_class_severity <- df_audiogram[,4:10] %>% melt() %>% group_by(Class, variable) %>% dplyr::summarise(value = mean(value)) %>% group_by(Class) %>% summarise(value=mean(value*value))

sorted_classes <- sort_class_severity[order(sort_class_severity$value),]$Class

df_audiogram$Class <- factor(df_audiogram$Class, levels = sorted_classes, ordered = TRUE)

df_user_worstear <- df_audiogram %>% group_by(record_id) %>% summarise(class=max(Class))

ids.t1 <- df_user_worstear %>% merge(df_audiogram, by = "record_id") %>% merge(df_ha_use, by=("record_id")) %>% filter(ha_usetime_hours_per_day < 2)
ids.t3 <- df_user_worstear %>% merge(df_audiogram, by = "record_id") %>% merge(df_ha_use, by=("record_id")) %>% filter(class > "S1" & ha_usetime_hours_per_day < 2)

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



#mean audiogram 

df_audiogram <- read.csv(paste(datafolder,'MeanAudiogramClass.csv', sep='/'))

df_audiogram <- df_audiogram[,-c(1)]

names(df_audiogram) <- c('record_id','AudClass')


#Carlson Comorbidity Index
df_CCI <- read.csv(paste(datafolder, 'CCI-skema.csv', sep='/'))

df_CCI <- df_CCI[, -c(2)]







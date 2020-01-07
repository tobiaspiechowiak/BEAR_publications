
# SSQ
ssq_means_naomit <- t3.data.ssq %>% filter(!is.na(.$ssq_speech_mean) & !is.na(.$ssq_space_mean) & !is.na(.$ssq_sound_mean)) # filter out the worst na's (the ones where an entire category of questions where na)
a <- ssq_means_naomit[,16:18] %>% kmeans(40) # cluster them by the mean (less problems with na's)
ssq_means_naomit$Cluster <- a$cluster
ssq_means_naomit <- ssq_means_naomit %>% 
  group_by(Cluster) %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x),mean(x, na.rm = T), x)) # replace all na values with the mean of the column in the cluster
t3.data.ssq.no.na <- ssq_means_naomit[, names(ssq_means_naomit) != "Cluster"]

## Diff
t3.diff.ssq.no.na <- t3.data.ssq.no.na %>% 
  group_by(record_id) %>% filter(n()>1) %>% #grab only the observations with both a baseline and followup
  summarise_all(funs(if(is.numeric(.)) last(.)-first(.) else first(.)))


# THI
t3.data.thi.no.na <- t3.data.thi %>%
  mutate(tinnitus, tinnitus=replace_na(tinnitus,"No")) %>% 
  mutate_at(vars(starts_with("tinnitus_"), -ends_with("complete")), function(x)replace_na(x,"No"))

t3.data.thi.no.na$Score <- NULL
t3.data.thi.no.na$THI_grade <- NULL

t3.data.thi.no.na <- 
  t3.data.thi.no.na %>%
  select(record_id, redcap_event_name, starts_with("tinnitus_"),-ends_with("complete")) %>%
  melt(id.vars=c("record_id", "redcap_event_name")) %>% 
  group_by(record_id, redcap_event_name, value) %>% summarise(nes=n()) %>%
  spread(value, nes, fill=0) %>% group_by(redcap_event_name, record_id) %>% 
  mutate(Score = No*0+Occasionally*2+Yes*4) %>%
  summarise(Score=sum(Score)) %>% 
  mutate(THI_grade=cut(Score, 
                       breaks=c(0,16,36,56,76,100),
                       labels=c("Very mild", "Mild", "Moderate", "Severe", "Catastrofic"),
                       include.lowest = TRUE)) %>% 
  merge(t3.data.thi.no.na, by=c("redcap_event_name", "record_id"))

# IOI-HA
iois <- c(round(mean(as.numeric(df_ioi$ioi_ha_1), na.rm=T)),
          round(mean(as.numeric(df_ioi$ioi_ha_2), na.rm=T)),
          round(mean(as.numeric(df_ioi$ioi_ha_3), na.rm=T)),
          round(mean(as.numeric(df_ioi$ioi_ha_4), na.rm=T)),
          round(mean(as.numeric(df_ioi$ioi_ha_5), na.rm=T)),
          round(mean(as.numeric(df_ioi$ioi_ha_6), na.rm=T)),
          round(mean(as.numeric(df_ioi$ioi_ha_7), na.rm=T)))
names(iois) <- names(df_ioi[3:9])
a <- kmeans(replace_na(df_ioi[df_ioi$ioiha_dk_complete!="Incomplete",][3:9], as.list(iois)), 40)

ioi_clustered <- df_ioi
ioi_clustered$cluster <- -1
ioi_clustered[ioi_clustered$ioiha_dk_complete!="Incomplete",]$cluster <- a$cluster

ioi_clustered <- ioi_clustered %>% 
  filter(cluster>-1) %>% 
  group_by(cluster) %>% 
  mutate_if(is.ordered, function(x) ifelse(is.na(x),as.integer(names(sort(-table(x)))[1]), x))

df_ioi.no.na <- ioi_clustered[,1:10]
df_ioi.no.na[,3:9] <- lapply(df_ioi.no.na[,3:9], factor, 
                             levels=c(1,2,3,4,5), 
                             ordered = TRUE)

# 15D
melted_15d <- t3.data.15d %>% select(record_id, redcap_event_name, starts_with("fifteen_d_"), d_complete) %>%
  melt(id.vars=c("record_id", "redcap_event_name","d_complete"))

nas_15d <- melted_15d %>% 
  group_by(record_id, redcap_event_name, d_complete) %>% 
  summarize(nas=sum(is.na(value)))

lost_causes <- nas_15d[nas_15d$nas>14,] # What should the limit of a lost cause be? right now it 15 nas
theds <- c(round(mean(as.numeric(t3.data.15d$fifteen_d_1_move), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_2_sight), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_3_hear), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_4_breathe), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_5_sleep), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_6_eat), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_7_speak), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_8_pee), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_9_activity), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_10_mental), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_11_unplesant), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_12_depression), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_13_stress), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_14_power), na.rm=T)),
           round(mean(as.numeric(t3.data.15d$fifteen_d_15_sex), na.rm=T)))
names(theds) <- names(t3.data.15d[3:17])
# a <- kmeans(replace_na(t3.data.15d[t3.data.15d$d_complete!="Incomplete",][3:17], as.list(theds)), 40)
a <- kmeans(replace_na(t3.data.15d[!t3.data.15d$record_id %in% lost_causes$record_id,][3:17], as.list(theds)), 40)

fifteen_clustered <- t3.data.15d
fifteen_clustered$cluster <- -1
#fifteen_clustered[fifteen_clustered$d_complete!="Incomplete",]$cluster <- a$cluster
fifteen_clustered[!fifteen_clustered$record_id %in% lost_causes$record_id,]$cluster <- a$cluster

fifteen_clustered[fifteen_clustered$cluster!=-1,] <- fifteen_clustered[fifteen_clustered$cluster!=-1,] %>% 
  group_by(cluster) %>% 
  #mutate_if(is.ordered, function(x) ifelse(is.na(x),as.integer(names(sort(-table(x)))[1]), x))
  mutate_at(vars(starts_with("fifteen_d_")), function(x) ifelse(is.na(x),as.integer(names(sort(-table(x)))[1]), x))

t3.data.15d.no.na <- na.omit(fifteen_clustered[,1:21])
t3.data.15d.no.na[,3:17] <- lapply(t3.data.15d.no.na[,3:17], factor, 
                                   levels=c(5,4,3,2,1), 
                                   ordered = TRUE)

# HA Use

df_ha_use.no.na <- df_ha_use
df_ha_use.no.na$ha_manufactor <- NULL
df_ha_use.no.na$ha_number_of_ha  <- df_ha_use.no.na$ha_number_of_ha %>% as.numeric
df_ha_use.no.na$ha_dispencer  <- df_ha_use.no.na$ha_dispencer %>% as.character
df_ha_use.no.na  <- df_ha_use.no.na %>% replace_na(list(ha_number_of_ha=0, ha_use_time=0, ha_dispencer="None",own_ha="No"))
df_ha_use.no.na$ha_number_of_ha <- 
  factor(df_ha_use.no.na$ha_number_of_ha, 
         levels=c(0:3), 
         labels = c("None","Both ears","Right ear","Left ear"))
df_ha_use.no.na$ha_dispencer <- 
  factor(df_ha_use.no.na$ha_dispencer)
summary(df_ha_use.no.na)
df_ha_use.no.na <- na.omit(df_ha_use.no.na) #The remaining nas are in ha_usetime_hours_per_day, not sure how to remove those without affecting our results, considering how central use time is, so just remove for now.

# Noise
df_tinnitus.no.na <- replace_na(df_tinnitus, list(
  noise_at_work="Don't know",
  tinnitus="Don't know"
))

df_tinnitus.no.na <- df_tinnitus.no.na %>% mutate(noise_industry_1=factor(ifelse(record_id=="364-662", 5, noise_industry_1), levels =1:13 , labels=levels(df_tinnitus$noise_industry_1))) # gotta refactor the variable, because R is being difficult, and converting the levels to numbers :/
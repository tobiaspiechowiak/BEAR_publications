suppressMessages(suppressWarnings(
  expr=source('setup.R', echo = F)
))
suppressMessages(suppressWarnings(
  expr=source('missingValues.R', echo = F)
))

ssq_diff <- t3.diff.ssq.no.na %>% select(record_id,redcap_event_name,starts_with("ssq_"))
colnames(ssq_diff) <- c(colnames(ssq_diff[,1:2]), paste0(colnames(ssq_diff[,3:17]), "_diff"))
df_everything <- t3.data.15d.no.na %>% select(record_id, redcap_event_name, starts_with("fifteen_d_")) %>% 
  merge(
    t3.data.ssq.no.na %>% select(record_id,redcap_event_name,starts_with("ssq_"), IsT1DrawerUser)
    , by=c("record_id", "redcap_event_name")) %>% merge(
      t3.data.thi.no.na %>% select(record_id,redcap_event_name, tinnitus, starts_with("tinnitus_"),-ends_with("complete"))
      , by=c("record_id", "redcap_event_name")) %>% merge(
        df_ioi.no.na %>% select(record_id, starts_with("ioi_ha_"))
        , by=c("record_id")) %>% merge(
          ssq_diff %>% select(record_id, starts_with("ssq_"))
          , by=c("record_id")) %>% merge(
            df_ha_use.no.na %>% select(record_id, own_ha, ha_number_of_ha, ha_use_time, ha_usetime_hours_per_day, sex, age)
            , by=c("record_id")) %>%  merge(
              df_audiogram %>% select(record_id, Class)
              , by=c("record_id"))

summary(df_everything)

d1 <- df_everything %>% 
  select(everything(), -starts_with("tinnitus"), -ioi_ha_1, -redcap_event_name, -record_id, -ha_usetime_hours_per_day, -IsT1DrawerUser) %>%
  mutate_if(is.factor, as.numeric)
pca.fit <- prcomp(d1, scale. = T)

d2 <- data.frame(pca.fit$rotation)
d2$col <- rownames(d2)

ggplot(d2[,c(paste0("PC",1:6),"col")] %>% melt(id.vars="col"), aes(x=col, y=value)) + geom_col()+ facet_wrap(~variable)+theme(axis.text.x = element_text(angle=90))

load <- data.frame(pca.fit$rotation)[,1:6]
load$name <- rownames(load)
data <- load %>% melt(id.vars="name") %>% mutate_if(is.numeric, function(x)ifelse(abs(x)<0.15, 0, x))
ggplot(data) + geom_col(aes(y=value, fill=variable, x=name), color=rgb(0,0,0,0.5))+
  theme_light() + theme(axis.text.x = element_text(angle=90, vjust = 0.5)) + labs(fill="Component", x="Variable", y="Loading") + ggtitle("Loadings of Principal Component Analysis") + scale_fill_brewer(palette = "RdYlBu")

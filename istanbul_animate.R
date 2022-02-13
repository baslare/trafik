require(tidyverse)
require(sf)
require(gganimate)
require(extrafont)

schedule_df_merged <- readRDS("schedule_df_merged.rds")
metrobus_merged <- readRDS("metrobus_merged.rds")
schedule_donus_df_merged("schedule_donus_df_merged.rds")

gidis_merged <- rbind(schedule_df_merged,metrobus_merged)



ist <- read_sf("shapefiles/ist_ilce.shp")
goller <- read_sf("shapefiles/goller.shp")
goller <- goller %>% filter(type %>% str_detect("multipolygon"))




merged_final <- rbind(schedule_donus_df_merged,gidis_merged)
merged_final <- merged_final %>% filter(!hat %>% str_detect("ist_34"))
merged_final$time_minutes <- merged_final$times_f/lubridate::seconds(60)


anim = ggplot(merged_final) + geom_sf(data=ist,fill="#312F3C",color="transparent") + 
  geom_sf(data=goller,color="#847577",fill="#847577") +
  geom_point(size=0.25,aes(x=lng,y=lat,group=hat,fill=gidis),alpha=0.35,shape=21,color="transparent") +
  labs(x="",y="",caption = "Efe Baslar - @baslare",title = '{ifelse(floor(frame_time/60/60) < 10, paste0("0",floor(frame_time/60/60)),floor(frame_time/60/60))}:{ifelse(floor((frame_time/60/60 -floor(frame_time/60/60))*60) < 10,paste0("0",floor((frame_time/60/60 -floor(frame_time/60/60))*60)),floor((frame_time/60/60 -floor(frame_time/60/60))*60))}') +
  scale_x_continuous(limits = c(28.55,29.25)) + 
  scale_y_continuous(limits = c(40.9,41.3)) +
  transition_time(times_f)+ exit_disappear() + 
  theme(panel.border = element_rect(color="#322F3D",fill="transparent"),
        plot.background = element_rect(fill="#847577"),
        panel.background = element_rect(fill="#847577"),
        panel.grid = element_blank(),
        text = element_text(family="Noto Sans",color="white"),
        plot.title = element_text(size=25,hjust=0.5,vjust=-100),
        plot.caption = element_text(size=9),
        title = element_text(size=25,hjust = 0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("#0048BA","#FF0000"))+
  shadow_null()+
  ease_aes('linear')

to_save <- animate(anim, nframes = 960, fps = 24,width=800,height=800)
#to_save2 <- animate(anim2, nframes = 1440, fps = 24,width=800,height=600)

anim_save("iett_merged_normal.gif",animation = to_save)
#anim_save("iett_merged2.gif",animation = to_save2)



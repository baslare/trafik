require(tidyverse)
require(xml2)
require(rvest)
require(jsonlite)
require(sf)
require(gganimate)
require(extrafont)

###get the line codes from IETT
listIETT<- read_html("https://www.iett.istanbul/tr/main/hatlar") %>% html_node(css="#StaionLiveData > ol") %>% html_children()
hatNo <- sapply(listIETT, function(x) x %>% html_attr("data-hat-code"))
hatNames <- sapply(listIETT, function(x) x %>% html_attr("data-hat-name"))

###### trafi ########

hatlarGidis <- hatNo
hatlarDonus <- hatNo

###### gidis ######

trafiList <- list()


i <- 1
while(i <= length(hatlarGidis)){
  Sys.sleep(0.2)
  try(trafiList[[hatlarGidis[i]]] <- fromJSON(URLencode(paste0("https://web.trafi.com/api/schedules/istanbul/schedule?scheduleId=ist_",hatlarGidis[i],"&transportType=bus"))))
  i <- i + 1
}

gidisDurakKod <- lapply(trafiList, function(x) x$tracks$stops[[1]]$stopId)
donusDurakKod <- lapply(trafiList, function(x){
  
  index <- which(x$tracks$id %>% str_detect("D_D0"))
  if(identical(index,integer())){
    
    return("")
  }else{
    return(x$tracks$stops[[index]]$stopId)
  }
  
})



trafiCheck1 <- sapply(donusDurakKod, length)

donusDurakKod2 <- donusDurakKod[trafiCheck1 > 1]

schedule <- list()
hatlar <- names(gidisDurakKod)
i  <- 1
while(i <= length(hatlar)){
  j <- 0
  hat <- hatlar[i]
  schedule[[hat]] <- list()
  
  while(j > -1){
    
    
    durakKod <- gidisDurakKod[[i]][1]
    
    
    sub_schedule <- try(fromJSON(URLencode(paste0("https://web.trafi.com/api/runs/istanbul?scheduleId=ist_",hat,"&trackId=",hat,"_G_D0&stopId=",durakKod,"&timeIndex=",j,"&daysOfWeek=62"))))
    
    if(is.list(sub_schedule)){
      schedule[[hat]][[paste(j)]] <- sub_schedule
      
      j <- j + 1
    }else{
      j <- - 1
      message(paste(hat,"cleared, next!"))
    }
    
  }
  
  i <- i + 1
}

saveRDS(schedule,"schedule.rds")


#####donus#####

trafiList_donus <- list()


i <- 1
while(i <= length(hatlarDonus)){
  
  try(trafiList_donus[[hatlarDonus[i]]] <- fromJSON(URLencode(paste0("https://web.trafi.com/api/schedules/istanbul/schedule?scheduleId=ist_",hatlarDonus[i],"&transportType=bus&trackId=1_D_D0"))))
  i <- i + 1
}

metrobusDonus <- hatNo[hatNo %>% str_detect("^34")]
trafiList_donus_metrobus <- list()
i <- 1
while(i <= length(metrobusDonus)){
  
  try(trafiList_donus_metrobus[[metrobusDonus[i]]] <- fromJSON(URLencode(paste0("https://web.trafi.com/api/schedules/istanbul/schedule?scheduleId=ist_",metrobusDonus[i],"&transportType=metrobus&trackId=1_D_D0"))))
  i <- i + 1
}

trafiList_donus <- append(trafiList_donus,trafiList_donus_metrobus)

donusDurakKod <- lapply(trafiList_donus, function(x){
  
  index <- which(x$tracks$id %>% str_detect("D_D0"))
  if(identical(index,integer())){
    
    return("")
  }else{
    return(x$tracks$stops[[index]]$stopId)
  }
  
})



trafiCheck1 <- sapply(donusDurakKod, length)

donusDurakKod2 <- donusDurakKod[trafiCheck1 > 1]

schedule_donus <- list()
hatlar <- names(donusDurakKod2)
i  <- 1
while(i <= length(hatlar)){
  j <- 0
  hat <- hatlar[i]
  schedule_donus[[hat]] <- list()
  
  while(j > -1){
    
    
    durakKod <- donusDurakKod2[[i]][1]
    
    
    sub_schedule <- try(fromJSON(URLencode(paste0("https://web.trafi.com/api/runs/istanbul?scheduleId=ist_",hat,"&trackId=",hat,"_D_D0&stopId=",durakKod,"&timeIndex=",j,"&daysOfWeek=62"))))
    
    if(is.list(sub_schedule)){
      schedule_donus[[hat]][[paste(j)]] <- sub_schedule
      
      j <- j + 1
    }else{
      j <- - 1
      message(paste(hat,"cleared, next!"))
    }
    
  }
  
  i <- i + 1
}

saveRDS(schedule,"schedule_donus.rds")

######metrobus gidis #######

metrobus_gidis_kod <- lapply(trafiList_donus[metrobusDonus], function(x){
  
  index <- which(x$tracks$id %>% str_detect("G_D0"))
  if(identical(index,integer())){
    
    return("")
  }else{
    return(x$tracks$stops[[index]]$stopId)
  }
  
})

ln <- sapply(metrobus_gidis_kod, length)
metrobus_gidis_check <- names(ln)[ln > 1]


metrobus_gidis_list <- trafiList_donus_metrobus[metrobus_gidis_check]

schedule_g_metrobus <- list()
hatlar <- metrobus_gidis_check
i  <- 1
while(i <= length(hatlar)){
  j <- 0
  hat <- hatlar[i]
  schedule_g_metrobus[[hat]] <- list()
  
  while(j > -1){
    
    
    durakKod <- metrobus_gidis_list[[i]]$tracks$stops[[1]]$stopId[1]
    
    
    sub_schedule <- try(fromJSON(URLencode(paste0("https://web.trafi.com/api/runs/istanbul?scheduleId=ist_",hat,"&trackId=",hat,"_G_D0&stopId=",durakKod,"&timeIndex=",j,"&daysOfWeek=62"))))
    
    if(is.list(sub_schedule)){
      schedule_g_metrobus[[hat]][[paste(j)]] <- sub_schedule
      
      j <- j + 1
    }else{
      j <- - 1
      message(paste(hat,"cleared, next!"))
    }
    
  }
  
  i <- i + 1
}

saveRDS(schedule,"schedule_g_metrobus.rds")
saveRDS(trafiList_donus,"trafiList_donus.rds")


##### cleaning #####

donus_check <- sapply(trafiList_donus, function(x) which(x$tracks$id %>% str_detect("D_D0")))
donus_check <- sapply(donus_check, function(x) length(x) > 0)
trafiList_donus_2 <- trafiList_donus[donus_check]

trafiList_donus_2 <- lapply(trafiList_donus_2, function(x){
  
  index <- which(x$tracks$id %>% str_detect("D_D0"))
  return(left_join(x$tracks$stops[[index]],x$stops %>% select(id,name,areaName,lat,lng), by=c("stopId"="id")))
})




trafiList_donus_df3 <- Map(function(x,y) x %>% mutate(hat=y),x=trafiList_donus_2,names(trafiList_donus_2))



metrobus_g <- lapply(schedule_g_metrobus, function(x) Map(function(a,b) b$stops %>% mutate(times_f=time %>% lubridate::hm(),hat=paste0(b$scheduleId,"-",a)),a=names(x),b=x))
metrobus_g_df <- Map(function(x,y) lapply(x, function(z) z %>% mutate(lng=as.numeric(y$lng),lat=as.numeric(y$lat))), x=metrobus_g,y=trafiList_donus_2[metrobus_gidis_check])
metrobus_merged <- lapply(metrobus_g_df, bind_rows)
metrobus_merged <- metrobus_merged %>% bind_rows()
metrobus_merged <- metrobus_merged %>% mutate(gidis=TRUE)


schedule_donus_df <- lapply(schedule_donus, function(x) Map(function(a,b) b$stops %>% mutate(times_f=time %>% lubridate::hm(),hat=paste0(b$scheduleId,"-",a)),a=names(x),b=x))
schedule_donus_df <- Map(function(x,y) lapply(x, function(z) z %>% mutate(lng=as.numeric(y$lng),lat=as.numeric(y$lat))), x=schedule_donus_df,y=trafiList_donus_df3)
schedule_donus_df_merged <- lapply(schedule_donus_df, bind_rows)
schedule_donus_df_merged <- schedule_donus_df_merged %>% bind_rows()

schedule_donus_df_merged <- schedule_donus_df_merged %>% mutate(gidis=FALSE)
schedule_donus_df_merged <- schedule_donus_df_merged %>% mutate(hat = paste0(hat,"-D"))

schedule_df <- lapply(schedule2, function(x) Map(function(a,b) b$stops %>% mutate(times_f=time %>% lubridate::hm(),hat=paste0(b$scheduleId,"-",a)),a=names(x),b=x))
schedule_df <- Map(function(x,y) lapply(x, function(z) z %>% mutate(lng=as.numeric(y$lng),lat=as.numeric(y$lat))), x=schedule_df,y=dfIETT_sub)
schedule_df_merged <- lapply(schedule_df, bind_rows)
schedule_df_merged <- schedule_df_merged %>% bind_rows()

schedule_df_merged <- schedule_df_merged %>% mutate(gidis=TRUE)


##### save the dataframes for future use #####

saveRDS(metrobus_merged,"metrobus_merged.rds")
saveRDS(schedule_df_merged,"schedule_df_merged.rds")
saveRDS(schedule_donus_df_merged,"schedule_donus_df_merged.rds")




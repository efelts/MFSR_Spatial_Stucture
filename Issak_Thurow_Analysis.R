##################################################
#Recreating analysis from Isaak and Thurow (2006)#
##################################################

library(tidyverse)
library(sf)
library(readxl)

#Read in data that encompasses basin-wide redd count survey information, 1995-2020; Something
#that differs here is that this data set includes multiple surveys of the
#same stream segment in some years. For example, in the early 2000s Marsh Creek
#was surveyed by both a multiple pass ground count (IDFG) and an aerial count
#at the end of spawning (USFS). This code relies on some of my judgments about which 
#survey to consider "best" in those scenarios. In general, multiple pass ground counts 
#were considered "best" if available, and if not the USFS aerial count at the end of 
#spawning was considered "best"

mfsr_surveys <- read_excel("data/mfsr_transects_survey_record.xlsx",
                           sheet="mfsr_transects_survey_record")

#get the number of unique surveys per year at each unit (idfg_id)

mfsr_survey_summaries <- mfsr_surveys %>% 
  group_by(SurveyYear,idfg_id) %>% 
  summarize(events=n(),
            first_survey_id=first(survey_id))

#compile a list of the surveys to select in order to remove duplicates; this first data frame
#is surveys where there are no redundancies so they are the only option

mfsr_no_conflicts_id <- mfsr_survey_summaries %>% 
  filter(events==1) %>% 
  rename(ef_select_redds=first_survey_id) %>% 
  mutate(ef_select_wpts=ef_select_redds) %>% 
  dplyr::select(SurveyYear,idfg_id,ef_select_redds,ef_select_wpts)

###################################################################
#These lines select surveys that give what is in my opinion       #
#the best representation of an accurate number of redds. There     # 
#will be some discrepancies when looking at a finer spatial       #
#scale because some of the surveys I consider to be more accurate,#
#mostly earlier IDFG ground surveys, did not always have waypoints#
###################################################################


ef_select <- read_csv("data/ef_selections.csv") %>% 
  dplyr::select(SurveyYear,idfg_id,ef_select_redds,ef_select_wpts)

winner_surveys <- bind_rows(mfsr_no_conflicts_id,ef_select)

#join with the full survey list so it's reduced to the selected surveys

mfsr_surveys_winner <- inner_join(mfsr_surveys,winner_surveys,by=c("SurveyYear"="SurveyYear",
                                                                   "idfg_id"="idfg_id",
                                                                   "survey_id"="ef_select_redds"))

#read in transect attributes so we can summarize by other scales such as basin

transect_attributes <-  read_excel("data/mfsr_transects_survey_record.xlsx",
                                   sheet="transect_attributes")

#join transect attributes to the surveys

mfsr_surveys_join <- left_join(mfsr_surveys_winner,transect_attributes,by=c("idfg_id"))

#annual summary overall

complete_annual <- mfsr_surveys_join %>% 
  group_by(SurveyYear) %>% 
  summarize(Redds=sum(redds))

#annual basin plot

basin_plot <- ggplot(data=complete_annual,aes(SurveyYear,Redds))+
  geom_point()+
  geom_line()+
  theme_bw()
basin_plot

###################################################
#Working with the best collection of waypoint data#
#to look at occupancy by 1 km river section; this #
#will have slightly different counts of redds than#
#the numbers used in the previous graphs          #
###################################################

#read in redd waypoints

redds <- read_excel("data/mfsr_transects_survey_record.xlsx",
                    sheet="waypoints")

redds_join <- left_join(redds,mfsr_surveys_join,by="survey_id")


#check "winner" surveys for which actually have waypoints

redds_winners <- inner_join(redds,winner_surveys,by=c("survey_id"="ef_select_wpts"))

redds_winners_summary <- redds_winners %>% 
  group_by(SurveyYear,idfg_id) %>% 
  summarize(waypoints_actual=sum(!is.na(lat)),
            waypoints_na=sum(is.na(lat)))


winner_surveys_summary <- mfsr_surveys_winner %>% 
  group_by(SurveyYear,idfg_id) %>% 
  summarize(redds=sum(redds))

wpt_join_summary <- left_join(winner_surveys_summary,redds_winners_summary,
                              by=c("SurveyYear","idfg_id")) %>% 
  mutate(wpt_differential=redds-waypoints_actual)

missing_wpts <- wpt_join_summary %>% 
  filter(wpt_differential>0)

#Read in IP layer (200-m segments) and join in some of the attributes I wanted to add fo to 
#split network into 1 km segments and those segments used in Isaak and Thurow 2006)

ip <- st_read(dsn="data",layer="ip")

ip_att <- read_csv("data/ip_attributes.csv",
                   col_types = "ccccnc")


segment_lengths <- ip %>% 
  inner_join(ip_att,by=c("OBJECTID")) %>% 
  st_drop_geometry() %>% 
  group_by(Isaak_segment) %>% 
  summarize(segment_length=sum(LENGTH),
            segment_length_km=segment_length/1000) %>% 
  dplyr::select(Isaak_segment,segment_length_km)


#use this code to dissolve the ip layer into the segments used in their paper
ip_isaak <-inner_join(ip,ip_att,by=c("OBJECTID")) %>% 
  dplyr::select(ef_stream,Isaak_segment,ip_km,LENGTH_WC,GRADIENT,
                PER_FOR,CHINLABEL,ELEV,LENGTH) %>% 
  group_by(ef_stream,Isaak_segment,CHINLABEL,ip_km) %>% 
  summarize(length=sum(LENGTH),
            weighed_length=sum(LENGTH_WC),
            gradient=mean(GRADIENT),
            percent_forest=mean(PER_FOR),
            elevation=mean(ELEV)) %>%
  rename(stream=ef_stream) %>% 
  mutate(Basin="Middle Fork Salmon",
         TRT_POP=ifelse(CHINLABEL=="MFBEA","Bear Valley Creek",
                        ifelse(CHINLABEL=="MFMAR","Marsh Creek",
                               ifelse(CHINLABEL=="MFSUL","Sulphur Creek",
                                      ifelse(CHINLABEL=="MFUMA","Middle Fork Salmon River above Indian Creek",
                                             ifelse(CHINLABEL=="MFLOO","Loon Creek",
                                                    ifelse(CHINLABEL=="MFCAM","Camas Creek",
                                                           ifelse(CHINLABEL=="MFBIG","Big Creek",
                                                                  "Middle Fork Salmon River below Indian Creek"))))))))
#filter out NA waypoints

redds_to_join <- redds_winners %>% 
  filter(!is.na(lat))

redds.sf <- st_as_sf(redds_to_join,coords=c("long","lat")) %>%
  st_set_crs(st_crs(ip_isaak))

redds_mfsr_isaak_join <- st_join(redds.sf,ip_isaak,join=st_nearest_feature)


isaak_annual_summary <- redds_mfsr_isaak_join %>% 
  st_drop_geometry() %>% 
  left_join(segment_lengths,by="Isaak_segment") %>% 
  group_by(SurveyYear,Isaak_segment) %>% 
  summarize(redds=n(),
            length_km=first(segment_length_km),
            redds_per_km=redds/length_km)

#calculate mean redd density and standard error by stream segment to reproduce
#figure 3b

isaak_densities <- isaak_annual_summary %>% 
  group_by(Isaak_segment) %>% 
  summarize(mean_density=mean(redds_per_km),
            density_sd=sd(redds_per_km),
            sample_size=n(),
            density_se=density_sd/sqrt(sample_size),
            max_density=max(redds_per_km)) 

isaak_3b.plot <- ggplot(data=isaak_densities,aes(Isaak_segment,mean_density))+
  geom_point()+
  geom_errorbar(aes(ymin=(mean_density-density_se),ymax=(mean_density+density_se)))+
  geom_point(aes(Isaak_segment,max_density))+
  labs(x="Stream segment",y="Density (redds per km")+
  theme_bw()
isaak_3b.plot




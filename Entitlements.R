# Script for calculating entitlement usage and TAE
#notes: 2009 first year of mandatory VMS and introduction in management plan

#MISSING:   
    # Need management plans to get value of each unit (e.g. in 2009: 1 unit= 270 m GN but now it is 27 m)
    # Get this from Maddie and any year when the unit value changed (update here: 'Unit.value')
    #  Also, the total number of units (column entitlement) is set in the management plan so cross check here:
    # 'Total.current.entitlements', this has the values from the latest plan, has this changed thru time? use
    #  this values rather than Entitlements$entitlement which varies a bit from yr to yr
    # Issue with vessels fishing with longline (e.g. Marrabundi), especially in recent year (2020-21) as currently only
    #    assuming gillnet, ignoring longline
    # when multiplying entitlement X value or exhausted X value


# Header ------------------------------------------------------------------
if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

require(readxl)
require(janitor)
require(tidyverse)
require(stringr)
require(ggplot2)

# Data section ------------------------------------------------------------------

file_list <- list.files(path=handl_OneDrive('Data/VMS/Shared SDGDL information'))
Data_list=vector('list',length(file_list))
names(Data_list)=file_list

for(f in 1:length(Data_list))
{
  if(grepl('WCDGDL',names(Data_list)[f]))
  {
    Data_list[[f]]=read_excel(handl_OneDrive(paste('Data/VMS/Shared SDGDL information',names(Data_list)[f],sep='/')),
                              sheet = "Permits and Zones")%>%
      clean_names()%>%
      rename(entitlement=units_available,
             remaining=units_remaining)%>%
      mutate(exhausted=entitlement-remaining,
             zone="WC",
             finyear=str_match(names(Data_list)[f], "WCDGDL \\s*(.*?)\\s*.xls")[,2])%>%
      filter(!is.na(permit))%>%
      filter(!is.na(entitlement))%>%
      dplyr::select(finyear,permit,vessel,lfb,zone,entitlement,exhausted,remaining)
  }else
  {
    Nm=gsub( " .*$", "", names(Data_list)[f])
    Data_list[[f]]=read_excel(handl_OneDrive(paste('Data/VMS/Shared SDGDL information',names(Data_list)[f],sep='/')),
                              sheet = "Permits and Zones",
                              skip = 1)%>%
      clean_names()%>%
      mutate(exhausted=ifelse(is.na(exhausted),0,exhausted),
             remaining=entitlement-exhausted,
             finyear=str_match(names(Data_list)[f], paste(Nm," \\s*(.*?)\\s*.xls",sep=''))[,2])%>%
      filter(!is.na(permit))%>%
      filter(!is.na(entitlement))%>%
      dplyr::select(finyear,permit,vessel,lfb,zone,entitlement,exhausted,remaining)
  }

}

Total.current.entitlements=data.frame(zone=c('WC','1','2','3'),
                                      finyear=c(rep('2020-21',4)),
                                      entitlements=c(8740,10090,14045,625))

# Data manipulation ------------------------------------------------------------------
Entitlements=do.call(rbind,Data_list)%>%
              mutate(Fishery=ifelse(zone%in%c('1','2','3'),'SCDGDL',
                             ifelse(zone%in%c('WC'),'WCDGDL',NA)))%>%
              group_by(finyear,Fishery,zone)%>%
              summarise(entitlement=sum(entitlement,na.rm=T),
                        exhausted=sum(exhausted,na.rm=T),
                        remaining=sum(remaining,na.rm=T))%>%
              ungroup()%>%
              mutate(year=as.numeric(substr(finyear,1,4)),
                     finyear=paste(year,substr(year+1,3,4),sep='-'))%>%
  filter(year>=2009)

Yrs=unique(Entitlements$year)
  Unit.value= data.frame(zone=rep(c('WC','1','2','3'),length(Yrs)),
                         year=rep(Yrs,each=4),
                         unit.value=rep(c(rep(270,6),rep(27,6)),each=4))        #dummy, update  MISSING

  
Entitlements=Entitlements%>%
    left_join(Unit.value,by=c("zone","year"))%>%
    mutate(entitlement_unit.val=entitlement*unit.value,
           exhausted_unit.val=exhausted*unit.value, 
           remaining_unit.val=remaining*unit.value)


Entitlements%>%
  filter(!is.na(Fishery))%>%
  group_by(year,Fishery)%>%
  summarise(entitlement_unit.val=sum(entitlement_unit.val),
            exhausted_unit.val=sum(exhausted_unit.val))%>%
  ggplot(aes(year,entitlement_unit.val,colour=Fishery))+
  geom_line(size=1.5)+
  geom_point(aes(year,exhausted_unit.val,colour=Fishery),size=2)+
  geom_line(aes(year,exhausted_unit.val,colour=Fishery),size=1,linetype = "dotted")+
  xlab('Financial year')+ylab('Total allowable effort / Effort use')
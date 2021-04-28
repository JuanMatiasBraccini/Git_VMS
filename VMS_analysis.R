# Script for analysis VMS data
# Two approaches considered: 1. HMM for all VMS data
#                            2. Machine learning for observed VMS data


rm(list=ls(all=TRUE))
library(data.table)
library(lubridate)
library(geosphere)
library(sp)
library(prevR)
library(rgeos)
library(PBSmapping)
data(worldLLhigh)
library(TrackReconstruction)
library(depmixS4)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# 1. Data section ---------------------------------------------------------

if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')

#Read in VMS and Vessels data
WD=handl_OneDrive('Analyses/VMS')
setwd(WD)
Vessel.names.code=read.csv("Vessel_code_name.csv",stringsAsFactors = F)
setwd(paste(WD,'\\Matias.data',sep=""))
files=list.files(pattern="*.csv")
VMS = as.data.frame(do.call(rbind, lapply(files, fread)))

#Read in observer data
Dat_obs=read.csv('C:\\Users\\myb\\Desktop\\dummy\\survey\\GN.data.csv',stringsAsFactors = F)


#Define ports                       MISSING, add all ports
Ports=data.frame(Port=c("Fremantle","Bunbury","Augusta",
                        'Windy.Harbour',"Albany","Esperance"),
                 Lon=c(115.742056,115.645922,115.171269,
                       116.025581,117.999446,121.920722),
                 Lat=c(-32.063547,-33.317978,-34.331350,
                       -34.839053,-35.063775,-33.850294))

#Define southwestern WA polygon
WA.poly=data.frame(Lon=c(
  113.6869, 114.2011, 114.2801, 114.6124, 114.7152,
  114.8730, 115.0238, 114.9622, 115.1193, 115.8122,
  115.6408, 115.6751, 115.4205, 115.0238, 115.0923, 
  115.6408, 116.0179, 116.7377, 117.2176, 117.5604, 
  118.3794, 118.9533, 119.2662, 119.5791, 119.9943,
  121.4340, 123.0451, 123.6964, 124.1763, 124.8134,
  126.0960, 127.3643, 128.6326, 129.1125, 129.1125),
  Lat=c(
    -26.70520, -27.58435, -28.14762, -28.55142, -28.93238,
    -29.12087, -29.48918, -30.03271, -30.58797, -31.89220,
    -32.62482, -33.18162, -33.66630, -33.59189, -34.26591,
    -34.41243, -34.82270, -35.02784, -34.96923, -35.11576,
    -34.84965, -34.43647, -34.49810, -34.10364, -33.91425,
    -33.83264, -33.94355, -33.81027, -33.00579, -32.87079,
    -32.24386, -32.27316, -31.86289, -31.59915, -26.70520))

setwd(WD)

# 2. Parameter section ---------------------------------------------------------
Max.speed.knot=15                         #in knots     #MISSING: check with Fisher!!!
Max.speed=Max.speed.knot*0.515            #in m/s 
Max.speed.haul.knot=2                     #in knots     #MISSING: check with fishER!!!
Max.speed.haul=Max.speed.haul.knot*0.515  #in m/s 
Max.time.btwin.pings=2*3600    # max time between consecutive pings same trip (in seconds)

Explr="NO"  #do exploratory analyses

 Shark.vessl=unique(tolower(c('TRACEY LEA','SOUTHWESTERN','quadrant',
                              "coralie g","ocean mistress")))   #memory issues
# Shark.vessl=unique(tolower(c('MARIAN','MANIKI II',     #REVIEW  MISSING!!!!!!
#                              'EXCALIBUR','BARONESS',
#                              'TRACEY LEA','FALCON II',
#                              'SOUTHWESTERN','quadrant',
#                              'fish tales a35b',
#                              Vessel.names.code$VESSEL_NAME)))


# 3. Procedure section ---------------------------------------------------------

#3.1 All VMS data ----

#Data manipulations
  #select TDGDLF vessels as defined by VMS people
VMS$VESSEL_TYPE=with(VMS,ifelse(NAME=="Just George",'WEST COAST SHARK',VESSEL_TYPE))
VMS = VMS %>% filter(VESSEL_TYPE%in%c('SOUTH COAST SHARK','WEST COAST SHARK'))

  #improve definition of TDGDLF vessels
Vessel.names.code=distinct(Vessel.names.code, VESSEL, .keep_all= TRUE)
Vessel.names.code$VESSEL_NAME=tolower(Vessel.names.code$VESSEL_NAME)
VMS$NAME=tolower(VMS$NAME)
Shark.vessl.vms=unique(VMS$NAME)
Shark.vessl.vms=Shark.vessl.vms[which(Shark.vessl.vms%in%Shark.vessl)]

  #select only TDGDLF vessels as defined by catch and effort ladies
VMS=VMS %>% filter(NAME%in%Shark.vessl.vms) 
  
  #transform variables
#sapply(VMS, class)
char.to.num=c("LONGITUDE","LATITUDE","AVERAGE_SPEED","AVERAGE_COG")
VMS = VMS %>% mutate_at(.funs = list(as.numeric), .vars = char.to.num)
char.to.posixct=c("SENT_DATE_UTC","STATE_DATE")
VMS = VMS %>% mutate_at(vars(char.to.posixct),
                funs(as.POSIXct(., format ="%d/%m/%Y %H:%M:%S", tz = "UTC")))
              
#fix latitude and longitude
VMS$LATITUDE=-abs(VMS$LATITUDE)
VMS$LONGITUDE=abs(VMS$LONGITUDE)
VMS=VMS%>% filter(LONGITUDE>=112 & LONGITUDE<=129 )  %>%
           filter(LATITUDE<=(-26) & LATITUDE>=(-38)) 

#remove duplicates and reduntant vars
VMS=VMS %>% distinct(NAME, SENT_DATE_UTC, .keep_all = TRUE) %>%
            select(-c(VESSEL_TYPE,AVERAGE_SPEED,AVERAGE_COG,STATE_DATE))

#standardise pinging frequency to 30 mins
  #note: prelim. analyses showd that frequency is not constant so interpolate
unik.ves=unique(VMS$NAME)
list.ves=vector('list',length(unik.ves))
function.interp=function(VSL)
{
  d=subset(VMS,NAME==VSL)
  Min=min(d$SENT_DATE_UTC)
  Max=max((d$SENT_DATE_UTC))
  d$imputed="NO"
  d = merge(data.frame(SENT_DATE_UTC=seq(Min,Max,by='30 mins')),d,all=TRUE) %>%
    mutate(NAME=VSL)
  d$LONGITUDE[is.na(d$LONGITUDE)] <- 
    with(d, approx(SENT_DATE_UTC, LONGITUDE, xout=SENT_DATE_UTC)$y)[is.na(d$LONGITUDE)]
  d$LATITUDE[is.na(d$LATITUDE)] <- 
    with(d, approx(SENT_DATE_UTC, LATITUDE, xout=SENT_DATE_UTC)$y)[is.na(d$LATITUDE)]
  d$imputed=with(d,ifelse(is.na(imputed),"YES",imputed))
  return(d)
}
for(l in 1:length(list.ves)) list.ves[[l]]=function.interp(VSL=unik.ves[l]) #takes 20 sec
VMS=do.call(rbind,list.ves)
rm(list.ves)

#Determine if VMS points on land 
VMS$on.land=point.in.polygon(VMS$LONGITUDE, VMS$LATITUDE,WA.poly$Lon,WA.poly$Lat)           

#Determine if VMS points overlap with port 
Port.spatial=SpatialPointsDataFrame(cbind(Ports$Lon,Ports$Lat),data.frame(Port=Ports$Port))
Port.spatial.buffer <- gBuffer(Port.spatial, width=.1, byid=TRUE) #width=.1 ~ 10 km radius
VMS=VMS%>%mutate(Port=point.in.SpatialPolygons(LONGITUDE, LATITUDE, Port.spatial.buffer))
rm(Port.spatial)

#remove useless imputed pings (land/port)
#VMS=subset(VMS,!(imputed=="YES" & on.land==1)| !(imputed=="YES" & Port=="TRUE"))

#Define useful pings (not on land and not in port)
VMS$useful=with(VMS,ifelse(!(on.land==1) & !(Port=="TRUE"),"YES","NO"))

#define same trip     
VMS=VMS %>% arrange(NAME, SENT_DATE_UTC) %>%
            mutate(Year.sent=year(SENT_DATE_UTC),
                   Name.prev=lag(NAME,1),
                   useful.prev=lag(useful,1),
                   Same.trip= ifelse(NAME==Name.prev & useful=="NO" & useful.prev=="NO","NO",
                              ifelse((NAME==Name.prev & useful=="YES" & useful.prev=="NO")|
                                      (NAME==Name.prev & useful=="NO" & useful.prev=="YES")|
                                      (NAME==Name.prev & useful=="YES" & useful.prev=="YES"),"YES",
                                NA)))

#calculate time (s), distance (in m) and speed (m/s) between consecutive pings
#   and keep records from same trip only
#note: ignore warning(), not an issue
VMS=VMS %>% 
  arrange(NAME, SENT_DATE_UTC) %>%
  mutate(SENT_DATE_UTC.prev=lag(SENT_DATE_UTC,1),
         Long.prev=lag(LONGITUDE,1),
         Lat.prev=lag(LATITUDE,1))%>%
  drop_na(c(LONGITUDE,LATITUDE,Long.prev)) %>%
  mutate(delta.time=ifelse(Same.trip=="YES",difftime(SENT_DATE_UTC,SENT_DATE_UTC.prev,units='secs'),NA),
         delta.time=ifelse(delta.time>Max.time.btwin.pings,NA,delta.time),
         delta.pos=ifelse(Same.trip=="YES",
                  distCosine(cbind(Long.prev, Lat.prev),cbind(LONGITUDE, LATITUDE)),NA),
         speed=ifelse(Same.trip=="YES",delta.pos/delta.time,NA),
         speed=ifelse(speed>Max.speed,NA,speed)) %>%
  filter(Same.trip=="YES" & !is.na(speed)) %>%
  mutate(zone=ifelse(LONGITUDE<116 & LATITUDE>(-33),'west',     #add zone
                      ifelse(LONGITUDE<116.5 & LATITUDE<=(-33),'zone1',
                      ifelse(LONGITUDE>=116.5,'zone2',NA))))

#select relevant variables
VMS=VMS %>% arrange(NAME, SENT_DATE_UTC) %>%
            select(c(NAME,Name.prev,SENT_DATE_UTC,SENT_DATE_UTC.prev,delta.time,
                     LONGITUDE,Long.prev,LATITUDE,Lat.prev,delta.pos,speed,
                     on.land,Port,useful,zone,Same.trip,Year.sent,imputed)) %>%
            mutate(Date.sent=date(SENT_DATE_UTC))


#Exploratory stuff
if(Explr=="YES")
{
  colfunc <- colorRampPalette(c("lightcyan","dodgerblue4"))
  WA.lat=c(-36,-26)
  WA.long=c(113,129)
  
  fn.map=function(NM,This.dat)
  {
    if(!is.null(This.dat))a=subset(VMS,NAME%in%NM &  Date.sent%in%This.dat)
    if(is.null(This.dat))a=subset(VMS,NAME%in%NM)
    par(mfcol=c(1,1),mgp=c(2,.6,0))
    XLM=c(min(a$LONGITUDE)*.999,max(a$LONGITUDE)*1.001)
    YLM=c(min(a$LATITUDE)*1.001,max(a$LATITUDE)*.999)
    plotMap(worldLLhigh, xlim=XLM,ylim=YLM,plt = c(.1, .99, 0.075, .95),
            col="dark grey",tck = 0.025, tckMinor = 0.0125)
    mtext(paste(NM,min(a$Date.sent),"to",max(a$Date.sent)),3)
    a$col=colfunc(nrow(a))
    a$pch=ifelse(a$useful=="YES",19,1)
    a$col=ifelse(a$useful=="NO","red",a$col)
    points(a$LONGITUDE,a$LATITUDE,pch=a$pch,col=a$col,cex=1.25)
  }
  fn.hist=function(NM,This.dat)
  {
    if(!is.null(This.dat))a=subset(VMS,NAME%in%NM &  Date.sent%in%This.dat &!is.na(delta.pos))
    if(is.null(This.dat))a=subset(VMS,NAME%in%NM &!is.na(delta.pos))
    
    par(mfrow=c(2,2),mar=c(3,3,1,1),mgp=c(1.5,.6,0))
    plot(1:nrow(a),a$delta.pos,type='l',ylab="step length",xlab="time")
    plot(1:nrow(a),a$speed,type='l',ylab="speed",xlab="time")
    
    hist(a$delta.pos,xlab="step length",main=NA,col=2)
    hist(a$speed,xlab="speed",main=NA,col=2)
  }
  This.ves=unique(VMS$NAME)
  pdf('Exploratory/main.vessels.pdf')
  for(l in 1:length(This.ves))
    for(v in 1:length(This.ves[[l]]))
    {
      fn.map(NM=This.ves[[l]][v],This.dat=NULL)
      fn.hist(NM=This.ves[[l]][v],This.dat=NULL)
    }
  dev.off()
  # fn.map(NM='Tracey Lea',
  #        This.dat=seq(as.Date('2018-04-01'),as.Date('2018-04-05'),by='day'))
  fn.plt.ping=function(d)
  {
    plot(1:nrow(d),d$delta.time/60,ylab="pinging frequency (mins)",
         main=unique(d$NAME))
    abline(h=60,col=2,lwd=2)
    text(0,120,"60 mins",pos=4,col=2,srt=45)
  }
  pdf('Exploratory/pinging.frequency.pdf')
  for(v in 1:length(Shark.vessl.vms)) fn.plt.ping(d=VMS%>%filter(Same.trip=="YES" & NAME == Shark.vessl.vms[v]))
  dev.off()
  
}

#Fit Hidden Markov Model
fn.HMM=function(d,rv,trm,N.states)
{
  mod <- depmix(formula(paste(rv,trm,sep="~")), data=d,nstates=N.states,
                family=gaussian(log),transition= formula(paste("",trm,sep="~")))
  fm <- fit(mod)
  Pred.state=posterior(fm)
  names(Pred.state)[-1]=paste("Prob",names(Pred.state)[-1],sep=".")
  d=cbind(d,Pred.state)
  
  return(list(mod.fit=fm,dat=d))
}

zns=sort(unique(VMS$zone))
StateS=2:3
Formla=c("1")
#Formla=c("1","NAME")  #missing: issues in West with few observations of some vessels
dummy.z=vector('list',length(zns))
names(dummy.z)=zns
AIC.tab=best.mod=dummy.z
for(z in 1:length(zns))
{
  dummy.s=vector('list',length(StateS))
  names(dummy.s)=StateS
  for(s in 1:length(StateS))
  {
    dummy.f=vector('list',length(Formla))
    names(dummy.f)=Formla
    for(f in 1:length(Formla))
    {
      dummy.f[[f]]=fn.HMM(d=VMS%>%filter(zone==zns[z]),
                rv='speed',trm=Formla[f],N.states=StateS[s])
      dummy.f[[f]]$aic=AIC(dummy.f[[f]]$mod.fit)
    }
    dummy.s[[s]]=dummy.f
  }
  dummy.z[[z]]=dummy.s
}

#select best model based on AIC
for(z in 1:length(zns))
{
  dummy.s=vector('list',length(StateS))
  for(s in 1:length(StateS))
  {
    dummy.f=vector('list',length(Formla))
    for(f in 1:length(Formla)) dummy.f[[f]]=dummy.z[[z]][[s]][[f]]$aic
    dummy.s[[s]]=data.frame(Term=names(dummy.z[[z]][[s]]),AIC=do.call(rbind,dummy.f))
  }
  AIC.tab[[z]]=data.frame(State=names(dummy.z[[z]]),do.call(rbind,dummy.s))
  best= AIC.tab[[z]][which.min(AIC.tab[[z]]$AIC),]
  id.s=match(as.character(best$State),names(dummy.z[[z]]))
  id.f=match(as.character(best$Term),names(dummy.z[[z]][[id.s]]))
  best.mod[[z]]=dummy.z[[z]][[id.s]][[id.f]]
}

#Define if vessel of fishing or not based on predicted state   MISSING!!!
Fishing.state=2:3
for(z in 1:length(zns))
{
  best.mod[[z]]$dat$Fishing.pred=with(best.mod[[z]]$dat,
                  ifelse(state%in%Fishing.state,1,0))
}


#3.2 Observer data approach ----

  #Select observer records that occur's in VMS
Dat_obs = Dat_obs %>% mutate(date=as.POSIXct(DATE, format ="%d/%m/%Y", tz = "UTC"),
                             Year=year(date),Month=month(date))  %>%
                      distinct(SHEET_NO, .keep_all= TRUE) %>%      #remove duplicates
                      filter(Year%in%VMS$Year.sent) %>%            #select records occurrying in MVS
                      select(SHEET_NO,BOAT,date,Year,Month,        #select relevant variables
                             Method,AVE.SET.TIME,AVE.HAUL.TIME,
                             SOAK.TIME,gillnet.effort) 

#MISSING: dummy hour. REMOVE ##33########################33
#had to manually set and haul hours, 
# Real observer data only has Date but if haul time is the following date, this
# will cause travel so fix !!!
Dat_obs=Dat_obs%>% filter(BOAT=="E67") %>%
  mutate(SENT_DATE_UTC.start=
          ymd_hms(c('2012-06-30 00:56:00','2012-06-30 09:00:00',
                  '2012-06-30 20:06:00','2012-07-01 07:42:00',
                  '2012-07-01 14:14:00','2012-07-02 01:50:00',
                  '2012-07-02 13:26:00','2012-07-03 01:32:00',
                  '2012-07-03 12:06:00','2012-07-03 22:42:00',
                  '2012-07-04 10:16:00','2012-07-04 22:22:00')),
         SENT_DATE_UTC.end=
          ymd_hms(c('2012-06-30 07:30:00','2012-06-30 18:34:00',
                   '2012-07-01 05:10:00','2012-07-01 12:44:00',
                   '2012-07-02 00:50:00','2012-07-02 11:24:00',
                   '2012-07-03 00:00:00','2012-07-03 10:06:00',
                   '2012-07-03 21:40:00','2012-07-04 07:46:00',
                   '2012-07-04 19:20:00','2012-07-05 06:26:00')),
         date=as.Date(date)) 
##33#######remove to here ##########33

#Turn on when real data is used
# Dat_obs=Dat_obs%>%mutate(SENT_DATE_UTC.start=ymd_hms(paste(date,AVE.SET.TIME,sep="")),
#                          SENT_DATE_UTC.end=ymd_hms(paste(date,AVE.HAUL.TIME,sep="")),
#                          date=as.Date(date))              

  #add vessel name to observer data and fix some inconsistencies
Dat_obs=merge(Dat_obs,Vessel.names.code,by.x="BOAT",by.y='VESSEL',all.x=T)
Dat_obs=Dat_obs %>% mutate(VESSEL_NAME=ifelse(Year==2012 &
                            VESSEL_NAME=='coralie g',"quadrant",VESSEL_NAME)) %>%
                    filter(!is.na(VESSEL_NAME))

Not.in.Vessel.names.code=c("A69","B132","B22","E009","E062","E10","F244",
                           "F505","F756","F768","T756")   #MISSING; CHECK WHY NOT, are then shark boats??

  #Extract 'observed' VMS data
VMS.obsrvd=VMS %>% filter(SENT_DATE_UTC >= min(Dat_obs$SENT_DATE_UTC.start) &
                          SENT_DATE_UTC <= max(Dat_obs$SENT_DATE_UTC.end)   & 
                            NAME %in% unique(Dat_obs$VESSEL_NAME)) %>%
                   select(-c(Year.sent))

  #Define if VMS record is fishing/not fishing based on observed time set-haul
#note: fishing is defined as the interval between start-set and end-haul times.
#      this includes speeds of 0 m/s (on anchor) or slow speeds that
#      correspond to hauling or setting manuevre). The steaming
#      part is not fishing

Dat_obs=Dat_obs %>% filter(VESSEL_NAME%in%VMS.obsrvd$NAME) %>%
                    select(SHEET_NO,BOAT,VESSEL_NAME,SHEET_NO,date,
                           SENT_DATE_UTC.start,SENT_DATE_UTC.end) %>% 
                    mutate(Interval=as.interval(SENT_DATE_UTC.start,
                                                SENT_DATE_UTC.end))
VMS.obsrvd$Fishing=NA
unik.ves=unique(VMS.obsrvd$NAME)
for(u in 1:length(unik.ves))
{
  dummy=subset(Dat_obs,VESSEL_NAME==unik.ves[u])
  id=which(VMS.obsrvd$NAME==unik.ves[u])
  VMS.obsrvd$Fishing[id]=rowSums(sapply(dummy$Interval,
          function(x) { mapply(`%within%`, VMS.obsrvd$SENT_DATE_UTC[id], x)}))
  VMS.obsrvd$Fishing[id]=ifelse(VMS.obsrvd$Fishing[id]>1,1,VMS.obsrvd$Fishing[id])
}

  #remove duplicated vessel date time if any
VMS.obsrvd=VMS.obsrvd %>% distinct(NAME,SENT_DATE_UTC, .keep_all= TRUE)

  #exploratory stuff
if(Explr=="YES")
{
  colfunc <- colorRampPalette(c("red","blue"))
  fn=function(DT1,DT)
  {
    b=subset(VMS.obsrvd,SENT_DATE_UTC<=ymd_hms(DT) & SENT_DATE_UTC>ymd_hms(DT1))
    Lat=b$LATITUDE
    Lat.prev=b$Lat.prev
    Lon.prev=b$Long.prev
    Lon=b$LONGITUDE
    b$col=colfunc(nrow(b))
    
    par(mfcol=c(2,1),mar=c(2.5,2.2,.1,.1),oma=c(1,.5,1,.1),mgp=c(1.25,.5,0))
    plot(1,1,ylim=c(min(c(Lat,Lat.prev))-.001,max(c(Lat,Lat.prev))+.001),xlab="",ylab="",
         xlim=c(min(c(Lon,Lon.prev))-.001,max(c(Lon,Lon.prev))+.001))
    points(Lon,Lat,pch=19,col=b$col,cex=1.15)
    text(Lon,Lat,paste("t=",1:nrow(b)," speed=",round(b$speed,2),sep=""),
         pos=1,offset=0.3,col=b$col,cex=.95)
    mtext(paste(unique(b$NAME),DT1,"to",DT),3,cex=.9)
    hist(b$speed,breaks=50,main="",xlab="Speed (m/s)",col=3)
    b$time.step=1:nrow(b)
    return(subset(b,select=c(time.step,SENT_DATE_UTC,speed)))
  }
  
  SEQ=seq(min(VMS.obsrvd$SENT_DATE_UTC),max(VMS.obsrvd$SENT_DATE_UTC),by='day')
  Stor=vector('list',(length(SEQ)-1))
  pdf('Exploratory/VMS_observed_just_plot.pdf')
  for(i in 1:(length(SEQ)-1))Stor[[i]]=fn(DT1=SEQ[i],DT=SEQ[i+1])
  dev.off()
  
  fn1=function(DT1,DT)
  {
    b=subset(VMS.obsrvd,SENT_DATE_UTC<=ymd_hms(DT) & SENT_DATE_UTC>ymd_hms(DT1))
    Lat=b$LATITUDE
    Lat.prev=b$Lat.prev
    Lon.prev=b$Long.prev
    Lon=b$LONGITUDE
    b$col=colfunc(nrow(b))
    
    par(mfcol=c(3,1),mar=c(2.5,2.2,.1,.1),oma=c(1,.5,1,.1),mgp=c(1.25,.5,0))
    plot(1,1,ylim=c(min(c(Lat,Lat.prev))-.001,max(c(Lat,Lat.prev))+.001),xlab="",ylab="",
         xlim=c(min(c(Lon,Lon.prev))-.001,max(c(Lon,Lon.prev))+.001))
    points(Lon,Lat,pch=19,col=b$col,cex=1.15)
    text(Lon,Lat,paste("t=",1:nrow(b)," speed=",round(b$speed,2),sep=""),
         pos=1,offset=0.3,col=b$col,cex=.95)
    mtext(paste(unique(b$NAME),DT1,"to",DT),3,cex=.8)
    
    dummy=subset(VMS.obsrvd,SENT_DATE_UTC<=ymd_hms(DT) & SENT_DATE_UTC>ymd_hms(DT1))
    Lat=dummy$LATITUDE
    Lat.prev=dummy$Lat.prev
    Lon.prev=dummy$Long.prev
    Lon=dummy$LONGITUDE
    dummy$col=with(dummy,ifelse(Fishing==1,"black","forestgreen"))
    
    plot(1,1,ylim=c(min(c(Lat,Lat.prev))-.001,max(c(Lat,Lat.prev))+.001),xlab="",ylab="",
         xlim=c(min(c(Lon,Lon.prev))-.001,max(c(Lon,Lon.prev))+.001))
    points(Lon,Lat,pch=19,col=dummy$col,cex=1.15)
    text(Lon,Lat,paste("t=",1:nrow(dummy)," speed=",round(dummy$speed,2),sep=""),
         pos=1,offset=0.3,col=dummy$col,cex=.95)
    
    plot(dummy$speed,dummy$Fishing,col=dummy$col,pch=19,cex=2,
         ylab="Fishing",xlab="Speed")
  }
  pdf('Exploratory/VMS_observed_check_classification.pdf')
  for(i in 1:(length(SEQ)-1))fn1(DT1=SEQ[i],DT=SEQ[i+1])
  dev.off()
}


#ACA
  #Fit machine learning


# 4. Validate HMM predictions with observer data ---------------------------------------------------------
#caret package classification error, specificity (confusion matrix)
CONF.MAT=vector('list',length(zns))
names(CONF.MAT)=zns
Val.VMS=CONF.MAT
for(z in 1:length(zns))
{
  Validate.VMS=best.mod[[z]]$dat %>% filter(SENT_DATE_UTC >= min(VMS.obsrvd$SENT_DATE_UTC) &
                              SENT_DATE_UTC <= max(VMS.obsrvd$SENT_DATE_UTC)   & 
                              NAME %in% unique(VMS.obsrvd$NAME)) 
  if(nrow(Validate.VMS)>0)
  {
    Validate.VMS=inner_join(Validate.VMS,subset(VMS.obsrvd,select=c(NAME,SENT_DATE_UTC,Fishing)))
    prediction=factor(Validate.VMS$Fishing.pred,levels=c(0,1))
    truth=factor(Validate.VMS$Fishing,levels=c(0,1))
    CONF.MAT[[z]]=confusionMatrix(prediction, truth)
    Val.VMS[[z]]=Validate.VMS
  }
}


#plot(best.mod[[z]]$dat$speed, best.mod[[z]]$dat$state)   

  
# 5. Report section ---------------------------------------------------------

Res.wd=paste(getwd(),'Results',sep='/')

#export AIC table
write.csv(data.frame(zone=names(AIC.tab),do.call(rbind,AIC.tab)),
    paste(Res.wd,"AIC_table.csv",sep='/'),row.names=F)

#Validation of HMM predictions
z=3;plot(CONF.MAT[[z]]$table)
qplot(Fishing, Fishing.pred, data=Val.VMS[[z]], colour= Fishing, 
      geom = c("boxplot", "jitter"), xlab = "Observed", ylab = "Predicted")

################################################################################################3
###YEAR 2020
###
###SCript for intercatch format: Create ages intercatch file from length intercatch file. By laboratory
###
###by:Gersom Costas
###
################################################################################################3
###
###
###  1.   "HI": header information
###  2.   "SI": Species information
###  3.   "SD": Species data
###

#####################################################################################
#####################################################################################
#install.packages("plyr")
#install.packages("tidyr")
#install.packages("dplyr")
#browseVignettes(package="dplyr")# muestra ejemplos del uso del package dplyr



library (tidyverse)
#####################################################################################
#####################################################################################

### 00 INTRODUCTION


#Importar archivo formato intercatch



IC_HOM_W <- read.table("2020/data/2020 DC WGWIDE  hom.27.2a4a5b6a7a-ce-k8 ES landigs IEO.csv",header=F,sep=",",na.strings ="")##catch data for 2019
      

head(IC_HOM_W)
str(IC_HOM_W)



######################################################################
######################################################################
######################################################################
###01.     "HI": header information

IC_HOM_W_HI <- filter(IC_HOM_W, IC_HOM_W[,1]=="HI")%>% 
  select_if(function(x) !(all(is.na(x)) | all(x==""))) %>% #Remove empty columns in dataframe 
  na_if (-9)%>%  # replace -9 to NA
  droplevels()%>%mutate_if( is.character, as.factor)##add

colnames(IC_HOM_W_HI)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","UnitEffort","Effort","AreaQualifier")


str(IC_HOM_W_HI)
summary(IC_HOM_W_HI)
head(IC_HOM_W_HI)
##error en fichero original  a?adir fila
#HI	ES	2018	Quarter	3	OTB_DEF_70-99_0_0	Div	27.7.b	NA	kWd	-9	NA
# dim(IC_HOM_W_HI)
# IC_HOM_W_HI<-add_row(IC_HOM_W_HI, RecordType=  "HI", Country= "ES", Year= 2018, SeasonType= "Quarter", Season=3 , Fleet= "OTB_DEF_70-99_0_0", AreaType= "Div", FishingArea= "27.7.b", DepthRange = "NA" , UnitEffort=  "kWd", Effort=  "-9", AreaQualifier= "NA" )
dim(IC_HOM_W_HI)
tail(IC_HOM_W_HI)											

######################################################################
######################################################################
######################################################################
### 02.    "SI": Species information####LANDINGS


IC_HOM_W_SI <- filter(IC_HOM_W, IC_HOM_W[,1]=="SI")%>%  
  select_if(function(x) !(all(is.na(x)) | all(x==""))) %>% #Remove empty columns in dataframe 
  na_if (-9) %>% # replace -9 to NA
  droplevels()%>%mutate_if( is.character, as.factor)

colnames(IC_HOM_W_SI)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","DataToFrom","Usage","SamplesOrigin","QualityFlag","UnitCATON","CATON","OffLandings","varCATON","InfoFleet","InfoStockCoordinator","InfoGeneral")


str(IC_HOM_W_SI)
summary(IC_HOM_W_SI)
head(IC_HOM_W_SI)

##error NA en sp cuando debe ser HOM. adhoc
summary(IC_HOM_W_SI$Species)
IC_HOM_W_SI<-IC_HOM_W_SI%>%mutate(Species=as.factor("HOM"))
summary(IC_HOM_W_SI$Species)

#CatchCategory:


# L =  Landings. Landings above minimum size.
# B =  BMS Landings. Landings below minimum size, BMS.
# D = Discards. part of the catch thrown overboard into the sea and not registered in  logbook.
# C = Catch , no separation in the information of landings or discards.
# R = Logbook Registered Discard. Relevant for stocks under landing obligation.  are under the exemption rules (e.g. minimis).


######################################################################
######################################################################
######################################################################
###03.     "SD": Species data    ##CATAGE



#Hacemos subset para crear archivo con distribuciones de talla por division y season
IC_HOM_W_SD <- filter(IC_HOM_W, IC_HOM_W[,1]=="SD")%>%
  select_if(function(x) !(all(is.na(x)) | all(x==""))) %>% #Remove empty columns in dataframe 
  na_if (-9) %>% # replace -9 to NA
    droplevels()%>%mutate_if( is.character, as.factor)

#ponemos nombres a las variable del archivo intercatch
colnames(IC_HOM_W_SD)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")


str(IC_HOM_W_SD)
IC_HOM_W_SD$AgeLength<- as.numeric(as.character(IC_HOM_W_SD$AgeLength))# pasamos la variable talla a foprmato numerico. . Al importarlo automaticament le habia asignado formato factor

summary(IC_HOM_W_SD)
head(IC_HOM_W_SD,2)

### 03.1  Length Distribution   LD

#Obtenemos un archivo de distribucion de tallas por metier, division y quarter.

LD<-IC_HOM_W_SD %>%
select(Stock,FishingArea,Fleet,Season, CatchCategory,AgeLength, NumberCaught,UnitAgeOrLength) %>%
group_by(Stock,FishingArea,Fleet,Season,CatchCategory,AgeLength,UnitAgeOrLength) %>%
summarise(canum = sum(NumberCaught, na.rm=T)) %>%
arrange(Stock,FishingArea,CatchCategory, Season,Fleet,AgeLength)

head(LD,3)


### 03.2  ALK
#Importamos las claves .
#Las claves deben de estar por division y semester En principio las importamos con formato: X, a0, a1, ......aXplus
##importante  el formato de las claves  igual (5-50 cm  y vacio nombre de columna length)

alkHOM_8cw_1S <- read.csv("2020/alks/hmack_alk_8cw_s1_19.csv")
alkHOM_8cw_2S <- read.csv("2020/alks/hmack_alk_8cw_s2_19.csv")
alkHOM_8ce_1S <- read.csv("2020/alks/hmack_alk_8ce_s1_19.csv")
alkHOM_8ce_2S <- read.csv("2020/alks/hmack_alk_8ce_s2_19.csv")
alkHOM_8a_1S <- read.csv("2020/alks/hmack_alk_8a_s1_19.csv")
alkHOM_8a_2S <- read.csv("2020/alks/hmack_alk_8a_s2_19.csv")
alkHOM_8b_1S <- read.csv("2020/alks/hmack_alk_8b_s1_19.csv")
alkHOM_8b_2S <- read.csv("2020/alks/hmack_alk_8b_s2_19.csv")
alkHOM_8d_1S <- read.csv("2020/alks/hmack_alk_8d_s1_19.csv")
alkHOM_8d_2S <- read.csv("2020/alks/hmack_alk_8d_s2_19.csv")


#8cw

alkHOM_8cw_1S<- bind_rows(alkHOM_8cw_1S,alkHOM_8cw_1S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.c.w"),Season=rep(1:2, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8cw_1S)

alkHOM_8cw_2S<- bind_rows(alkHOM_8cw_2S,alkHOM_8cw_2S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.c.w"),Season=rep(3:4, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8cw_2S)


#8ce

alkHOM_8ce_1S<- bind_rows(alkHOM_8ce_1S,alkHOM_8ce_1S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.c.e"),Season=rep(1:2, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8ce_1S)

alkHOM_8ce_2S<- bind_rows(alkHOM_8ce_2S,alkHOM_8ce_2S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.c.e"),Season=rep(3:4, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8ce_2S)
 

#8a

alkHOM_8a_1S<- bind_rows(alkHOM_8a_1S,alkHOM_8a_1S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.a"),Season=rep(1:2, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8a_1S)

alkHOM_8a_2S<- bind_rows(alkHOM_8a_2S,alkHOM_8a_2S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.a"),Season=rep(3:4, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8a_2S)


#8b

alkHOM_8b_1S<- bind_rows(alkHOM_8b_1S,alkHOM_8b_1S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.b"),Season=rep(1:2, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8b_1S)

alkHOM_8b_2S<- bind_rows(alkHOM_8b_2S,alkHOM_8b_2S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.b"),Season=rep(3:4, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8b_2S)


#8d

alkHOM_8d_1S<- bind_rows(alkHOM_8d_1S,alkHOM_8d_1S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.d"),Season=rep(1:2, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8d_1S)

alkHOM_8d_2S<- bind_rows(alkHOM_8d_2S,alkHOM_8d_2S)%>%#replicate matrix
  replace(is.na(.),0)%>% rename( AgeLength=X)%>%
  mutate(FishingArea=factor("27.8.d"),Season=rep(3:4, each=51))# length alk 5-50 cm #anadimos columna Season: trimestre
head(alkHOM_8d_2S)


#una vez importados# ALK by quarter area ##*percentage

alk<-bind_rows(alkHOM_8cw_1S,alkHOM_8ce_1S,alkHOM_8a_1S,alkHOM_8b_1S,alkHOM_8d_1S,
               alkHOM_8cw_2S,alkHOM_8ce_2S,alkHOM_8a_2S,alkHOM_8b_2S,alkHOM_8d_2S)%>%
   mutate(FishingArea=as.factor(as.character(FishingArea)))%>%
    group_by(FishingArea,Season,AgeLength)%>%
  summarise(total=sum(a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14, a15plus, na.rm=T))%>%
  full_join(bind_rows(alkHOM_8cw_1S,alkHOM_8ce_1S,alkHOM_8a_1S,alkHOM_8b_1S,alkHOM_8d_1S,
                      alkHOM_8cw_2S,alkHOM_8ce_2S,alkHOM_8a_2S,alkHOM_8b_2S,alkHOM_8d_2S)%>%
              mutate(FishingArea=as.factor(as.character(FishingArea))))%>%
  gather(age, num, -FishingArea, -Season, -AgeLength, -total,na.rm = T)%>%
  mutate(perc=num/total)%>%
  select(-num)%>%
  spread(age,perc)%>%
  select(FishingArea,total,Season,AgeLength,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15plus)%>%
  arrange(FishingArea,Season,  AgeLength)%>%ungroup()


head(alk)


###03.3  Abundance by age (catage) .. thousands
 ##Exploratory stuff

summary(IC_HOM_W_SD$FishingArea)
table(IC_HOM_W_SD$Fleet);
table(IC_HOM_W_SI$Fleet)


table(IC_HOM_W_SD$FishingArea,IC_HOM_W_SD$Fleet )
table(IC_HOM_W_SI$FishingArea,IC_HOM_W_SI$Fleet )

#by area, metier age.thousands#######


catage<-left_join(LD,alk,by=c("FishingArea" ,"Season" ,"AgeLength"   ))%>%
  mutate_at(vars(a0:a15plus),list(~.*canum*1))%>%  #% esta en thousands k
  filter(!is.na(total))%>%#delete areas-metier-quarter without sampling
  select(-AgeLength, -UnitAgeOrLength,-total)%>%
  group_by(Fleet,FishingArea,Season, CatchCategory)%>%
  summarise_at(vars(a0:a15plus),list(~sum(. ,na.rm=T)))%>%
  mutate(Flota= recode_factor( Fleet,"GNS_DEF_60-79_0_0"="artisanal", "LLS_DWS_0_0_0"="artisanal","GNS_DEF_80-99_0_0"="artisanal", "GTR_DEF_60-79_0_0"="artisanal", "LHM_DEF_0_0_0" ="artisanal", "LHM_DWS_0_0_0" ="artisanal","LHM_SPF_0_0_0"="artisanal", "LLS_DEF_0_0_0"="artisanal", "MIS_MIS_0_0_0_HC"="artisanal","OTB_DEF_>=55_0_0"="arrastre",  "OTB_MCD_>=55_0_0"="arrastre",  "OTB_MPD_>=55_0_0" ="arrastre","OTB_DEF_>=70_0_0"="arrastre" ,"PS_SPF_0_0_0"="cerco","PTB_MPD_>=55_0_0"="arrastre", "PTB_DEF_>=70_0_0"="arrastre",  "OTB_MCF_>=70_0_0"="arrastre",  "GNS_DEF_>=100_0_0"="artisanal", "OTB_SPF_>=55_0_0"="arrastre" ))%>% 
  select( Flota,Fleet,FishingArea:a15plus)%>% ##add AZTI metiers
  data.frame()%>%
  arrange(Fleet, FishingArea,Season)

head(catage)


catage_long<-catage%>%group_by(Flota,Fleet, FishingArea,Season, CatchCategory)%>% 
  gather(AgeLength, NumberCaught, a0:a15plus)%>% 
  mutate(AgeLength=factor(as.character(AgeLength), levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9",
                                                              "a10","a11","a12","a13","a14","a15plus")))%>%
 mutate(AgeLength= recode_factor(AgeLength, "a0"="0","a1"="1","a2"="2","a3"="3","a4"="4","a5"="5","a6"="6","a7"="7",
                                 "a8"="8","a9"="9","a10"="10","a11"="11","a12"="12","a13"="13","a14"="14","a15plus"="15"))%>%
  arrange(Fleet, FishingArea, CatchCategory,Season, AgeLength)%>%
  data.frame()

head(catage_long)


##mean length by area, metier age (  L(cm)  )######9an

lmeanage<-left_join(LD ,alk,by=c("FishingArea" ,"Season" ,"AgeLength"   ))%>%
  group_by(Fleet,FishingArea,Season, CatchCategory)%>%
  mutate_at(vars(a0:a15plus),list(~.*canum*1))%>%  #% esta en thousands k
  filter(!is.na(total))%>%#delete areas-metier-quarter without sampling
  mutate(meanlength=AgeLength+0.5)%>%
  group_by(Fleet,FishingArea,Season, CatchCategory)%>%
  mutate_at(vars(a0:a15plus),list(~.*meanlength))%>%
  select(-Stock, -AgeLength,- UnitAgeOrLength,-canum,-total,-meanlength )%>%
  group_by(Fleet,FishingArea,Season, CatchCategory)%>%  
  summarise_all(list(~sum(.,na.rm=T )))%>%
  group_by(Fleet,FishingArea,Season, CatchCategory)%>% 
  gather(AgeLength, sum_cm, a0:a15plus)%>%
  left_join(  gather(catage,AgeLength, n, a0:a15plus),by=c("Fleet", "FishingArea" ,"Season" ,"AgeLength", "CatchCategory"   ))%>%
  mutate(AgeLength=factor(as.character(AgeLength), levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9",
                                                              "a10","a11","a12","a13","a14","a15plus")))%>%
  mutate(AgeLength= recode_factor(AgeLength, "a0"="0","a1"="1","a2"="2","a3"="3","a4"="4","a5"="5","a6"="6","a7"="7",
                                  "a8"="8","a9"="9","a10"="10","a11"="11","a12"="12","a13"="13","a14"="14","a15plus"="15"))%>%
  mutate(MeanLength=sum_cm/n)%>% 
  select(-sum_cm,-n)%>%
  arrange(FishingArea, Fleet,CatchCategory,  Season)%>%
  data.frame()

head(lmeanage)


##mean weight by area, metier age  weight######

# #CHECKING--Parametos de conversion talla peso
# a<-0.01291              
# b<- 2.8545
##estimate for 2019: Modelo_hom8west.html
a<-0.008213              
b<- 3.010182

wmeanage<-left_join(LD ,alk,by=c("FishingArea" ,"Season" ,"AgeLength"   ))%>%
  group_by(Fleet,FishingArea,Season, CatchCategory)%>%
  mutate_at(vars(a0:a15plus),list(~.*canum*1))%>%  #% esta en thousands k
  filter(!is.na(total))%>%#delete areas-metier-quarter without sampling
  mutate(meanlength=AgeLength+0.5)%>%
  group_by(Fleet,FishingArea,Season, CatchCategory)%>%
  mutate(meanweight=a*meanlength^b) %>% #%  weight by length cm
  mutate_at(vars(a0:a15plus),list(~.*meanweight))%>%
  select(-Stock, -AgeLength,- UnitAgeOrLength,-canum,-total,-meanlength,-meanweight )%>%
  summarise_all(list(~sum(.,na.rm=T )))%>%
  group_by(Fleet,FishingArea,Season, CatchCategory)%>%  
  gather(AgeLength, sum_wgt, a0:a15plus)%>%
  left_join(  gather(catage,AgeLength, n, a0:a15plus),by=c("Fleet", "FishingArea" ,"Season" ,"AgeLength", "CatchCategory"   ))%>% 
  mutate(AgeLength=factor(as.character(AgeLength), levels = c("a0","a1","a2","a3","a4","a5","a6","a7","a8","a9",
                                                              "a10","a11","a12","a13","a14","a15plus")))%>%
  mutate(AgeLength= recode_factor(AgeLength, "a0"="0","a1"="1","a2"="2","a3"="3","a4"="4","a5"="5","a6"="6","a7"="7",
                                  "a8"="8","a9"="9","a10"="10","a11"="11","a12"="12","a13"="13","a14"="14","a15plus"="15"))%>%
  mutate(MeanWeight=sum_wgt/n, MeanWeight=MeanWeight/1000)%>% 
  select(-sum_wgt,-n)%>%
  arrange(FishingArea, Fleet, Season, CatchCategory)%>%
  data.frame()

  head(wmeanage)




###Catch (estimated) by metier, div, Season
 

  catches<-select(IC_HOM_W_SI,Year,Season, Fleet, FishingArea, CatchCategory, ReportingCategory,CATON )

##new SD data but by ages 
   
  IC_HOM_W_SD_new<-unique(select(IC_HOM_W_SD,-AgeLength ,-NumberCaught,-MeanWeight,-MeanLength))%>%
  mutate(CANUMtype=recode_factor(CANUMtype, "lngt"="age"),PlusGroup=as.factor("15"),
         UnitAgeOrLength=recode_factor(UnitAgeOrLength, "cm"="year"),NumSamplesAge= -9,NumAgeMeas= -9)%>%
  right_join( catage_long,by=c("Fleet", "FishingArea" ,"Season","CatchCategory"    ))%>%
  right_join( lmeanage,by=c("Fleet", "FishingArea" ,"Season","AgeLength" ,"CatchCategory"    ))%>%
  right_join( wmeanage,by=c("Fleet", "FishingArea" ,"Season", "AgeLength" ,"CatchCategory"   ))%>%
  select("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species",
         "Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch",
         "NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength",
         "UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")%>%
  left_join(catches)%>%
  mutate(SampledCatch =CATON)%>%
  select(-CATON)
  
  ##exploratory: sampled metiers by subdiv and quarter
  
  IC_HOM_W_SD_new%>%filter(NumberCaught >0)%>%select(Fleet , CatchCategory, FishingArea, Season )%>%unique()%>% arrange(Season, FishingArea, CatchCategory, Fleet)%>%arrange(Fleet,FishingArea,Season)
 
 ###03.4  Adding sampling information: Otolites, samples
#Introduce  ad hoc  
  ##OTOLITES_2019
  ##                Quarter     	1	  2 	  3   	4
  ## Otolitos le?dos  8cw     	207	282	  327	  1   #real   207	282	  328	  0
  ##Otolitos le?dos  	8cE	      331  482  198  444  
  ##N? muestreos    	8cW	       2    5    16    1  # real   2    5    17    0 
  ##N? muestreos    	8cE	       8   12     2   45  

  ##elegimos un metier que tenga muestreo todas las "subdiv con muestreo" y presencia en todos los trimestres 
  IC_HOM_W_SD_new<-IC_HOM_W_SD_new%>%mutate(NumAgeMeas=as.numeric(as.character(NumAgeMeas)),
             NumAgeMeas=replace(NumAgeMeas,Fleet=="PS_SPF_0_0_0"& Season==1& FishingArea=="27.8.c.w"& Year==2019& CatchCategory=="L",207),
             NumAgeMeas=replace(NumAgeMeas,Fleet=="PS_SPF_0_0_0"& Season==2& FishingArea=="27.8.c.w"& Year==2019& CatchCategory=="L",282),
             NumAgeMeas=replace(NumAgeMeas,Fleet=="PS_SPF_0_0_0"& Season==3& FishingArea=="27.8.c.w"& Year==2019& CatchCategory=="L",327),
             NumAgeMeas=replace(NumAgeMeas,Fleet=="PS_SPF_0_0_0"& Season==4& FishingArea=="27.8.c.w"& Year==2019& CatchCategory=="L",1),
             NumAgeMeas=replace(NumAgeMeas,Fleet=="PS_SPF_0_0_0"& Season==1& FishingArea=="27.8.c.e"& Year==2019& CatchCategory=="L",331),
             NumAgeMeas=replace(NumAgeMeas,Fleet=="PS_SPF_0_0_0"& Season==2& FishingArea=="27.8.c.e"& Year==2019& CatchCategory=="L",482),
             NumAgeMeas=replace(NumAgeMeas,Fleet=="PS_SPF_0_0_0"& Season==3& FishingArea=="27.8.c.e"& Year==2019& CatchCategory=="L",198),
             NumAgeMeas=replace(NumAgeMeas,Fleet=="PS_SPF_0_0_0"& Season==4& FishingArea=="27.8.c.e"& Year==2019& CatchCategory=="L",444),
             
             NumSamplesAge=replace(NumSamplesAge,Fleet=="PS_SPF_0_0_0"& Season==1& FishingArea=="27.8.c.w"& Year==2019& CatchCategory=="L",2),
             NumSamplesAge=replace(NumSamplesAge,Fleet=="PS_SPF_0_0_0"& Season==2& FishingArea=="27.8.c.w"& Year==2019& CatchCategory=="L",5),
             NumSamplesAge=replace(NumSamplesAge,Fleet=="PS_SPF_0_0_0"& Season==3& FishingArea=="27.8.c.w"& Year==2019& CatchCategory=="L",16),
             NumSamplesAge=replace(NumSamplesAge,Fleet=="PS_SPF_0_0_0"& Season==4& FishingArea=="27.8.c.w"& Year==2019& CatchCategory=="L",1),
             NumSamplesAge=replace(NumSamplesAge,Fleet=="PS_SPF_0_0_0"& Season==1& FishingArea=="27.8.c.e"& Year==2019& CatchCategory=="L",8),
             NumSamplesAge=replace(NumSamplesAge,Fleet=="PS_SPF_0_0_0"& Season==2& FishingArea=="27.8.c.e"& Year==2019& CatchCategory=="L",12),
             NumSamplesAge=replace(NumSamplesAge,Fleet=="PS_SPF_0_0_0"& Season==3& FishingArea=="27.8.c.e"& Year==2019& CatchCategory=="L",2),
             NumSamplesAge=replace(NumSamplesAge,Fleet=="PS_SPF_0_0_0"& Season==4& FishingArea=="27.8.c.e"& Year==2019& CatchCategory=="L",45),
             varNumLanded =replace_na(varNumLanded ,-9),
             varWgtLanded=replace_na(varWgtLanded,-9),
             varLgtLanded=replace_na(varLgtLanded,-9))
  
##Comparative  
  head(IC_HOM_W_SD_new,3)
head(filter(IC_HOM_W, IC_HOM_W[,1]=="SD"),3)
######################################################################
######################################################################
######################################################################
######################################################################



###04  Export Intercatch file

#IC_HOM_W <- read.table("IC 2019 DC WGHANSA hom.27.9a ES IEO landings IEO.csv",header=F,sep=",",na.strings ="")
colnames(IC_HOM_W_SD_new)
#"RecordType"        "Country"           "Year"              "SeasonType"        "Season"            "Fleet"            
#[7] "AreaType"          "FishingArea"       "DepthRange"        "Species"           "Stock"             "CatchCategory"    
#[13] "ReportingCategory" "Sex"               "CANUMtype"         "AgeLength"         "PlusGroup"         "SampledCatch"     
#[19] "NumSamplesLngt"    "NumLngtMeas"       "NumSamplesAge"     "NumAgeMeas"        "unitMeanWeight"    "unitCANUM"        
#[25] "UnitAgeOrLength"   "UnitMeanLength"    "Maturity"          "NumberCaught"      "MeanWeight"        "MeanLength"       
#[31] "varNumLanded"      "varWgtLanded"      "varLgtLanded"     
colnames(IC_HOM_W)
#[1] "V1"  "V2"  "V3"  "V4"  "V5"  "V6"  "V7"  "V8"  "V9"  "V10" "V11" "V12" "V13" "V14" "V15" "V16" "V17" "V18" "V19" "V20" "V21" "V22" "V23" "V24" "V25" "V26" "V27" "V28" "V29" "V30" "V31" "V32" "V33"


##exportamos en formato INTERCATCH para combinarlo con otros  todas edades  todas fuentes (azti, ieo, ieo_D)

write.table(bind_rows(IC_HOM_W_HI%>%
                        rename( V1=RecordType, V2=  Country , V3=Year, V4=SeasonType, V5=Season,V6=Fleet,
                                V7=AreaType,V8=FishingArea, V9=DepthRange, V10=UnitEffort, V11=Effort,V12=AreaQualifier)%>%
                        mutate_if(is.factor,list(~as.character(.)))%>%
                        mutate_if(is.numeric,list(~as.character(.)))%>%
                        mutate_if(is.integer,list(~as.character(.))),
                      IC_HOM_W_SI%>%
                        mutate(varCATON=replace(varCATON,is.na(varCATON),-9))%>%
                        rename(V1=RecordType, V2=  Country , V3=Year, V4=SeasonType, V5=Season,V6=Fleet,
                               V7=AreaType,V8=FishingArea, V9=DepthRange, V10=Species, V11=Stock,V12=CatchCategory, 
                               V13=ReportingCategory, V14=DataToFrom, V15=Usage, V16=SamplesOrigin, V17=QualityFlag,
                               V18=UnitCATON, V19=CATON, V20=OffLandings, V21=varCATON, V22=InfoFleet,
                               V23=InfoStockCoordinator, V24=InfoGeneral )%>%
                        mutate_if(is.factor,list(~as.character(.)))%>%
                        mutate_if(is.numeric,list(~as.character(.)))%>%
                        mutate_if(is.integer,list(~as.character(.))),
                              IC_HOM_W_SD_new%>%
                         mutate(MeanWeight=replace(MeanWeight,is.nan(MeanWeight),-9),##change
                               MeanLength=replace(MeanLength,is.nan(MeanLength),-9))%>%
                         rename( V1=RecordType, V2=  Country , V3=Year, V4=SeasonType, V5=Season,V6=Fleet,
                             V7=AreaType,V8=FishingArea, V9=DepthRange, V10=Species, V11=Stock,V12=CatchCategory, 
                             V13=ReportingCategory, V14=   Sex, V15=CANUMtype, V16=AgeLength, V17=PlusGroup,
                             V18=SampledCatch, V19=NumSamplesLngt, V20=NumLngtMeas, V21=NumSamplesAge, V22=NumAgeMeas,
                             V23=unitMeanWeight, V24=unitCANUM, V25=UnitAgeOrLength, V26=UnitMeanLength, V27=Maturity,
                             V28=NumberCaught, V29=MeanWeight, V30=MeanLength, V31=varNumLanded, V32=varWgtLanded,
                             V33=varLgtLanded  )%>%
                        mutate_if(is.factor,list(~as.character(.)))%>%
                        mutate_if(is.numeric,list(~as.character(.)))%>%
                        mutate_if(is.integer,list(~as.character(.)))),
            file="2020/output/IC_HOM_W_ieo_2019.txt",
            quote = F,
            sep = ",",
            na = "",
            dec = ".",
            row.names = F,
            col.names = F )

 
######################################################################
######################################################################
##removing when catage =0 because meanweight and meanlength are mandatorys (no with -9)-> to check in intercatch data handling


write.table(bind_rows(IC_HOM_W_HI%>%
                        rename( V1=RecordType, V2=  Country , V3=Year, V4=SeasonType, V5=Season,V6=Fleet,
                                V7=AreaType,V8=FishingArea, V9=DepthRange, V10=UnitEffort, V11=Effort,V12=AreaQualifier)%>%
                        mutate_if(is.factor,list(~as.character(.)))%>%
                        mutate_if(is.numeric,list(~as.character(.)))%>%
                        mutate_if(is.integer,list(~as.character(.))),
                      IC_HOM_W_SI%>%
                        mutate(varCATON=replace(varCATON,is.na(varCATON),-9))%>%
                        rename(V1=RecordType, V2=  Country , V3=Year, V4=SeasonType, V5=Season,V6=Fleet,
                               V7=AreaType,V8=FishingArea, V9=DepthRange, V10=Species, V11=Stock,V12=CatchCategory, 
                               V13=ReportingCategory, V14=DataToFrom, V15=Usage, V16=SamplesOrigin, V17=QualityFlag,
                               V18=UnitCATON, V19=CATON, V20=OffLandings, V21=varCATON, V22=InfoFleet,
                               V23=InfoStockCoordinator, V24=InfoGeneral )%>%
                        mutate_if(is.factor,list(~as.character(.)))%>%
                        mutate_if(is.numeric,list(~as.character(.)))%>%
                        mutate_if(is.integer,list(~as.character(.))),
                      IC_HOM_W_SD_new%>%
                        filter(!is.na(MeanWeight))%>%
                        rename( V1=RecordType, V2=  Country , V3=Year, V4=SeasonType, V5=Season,V6=Fleet,
                                V7=AreaType,V8=FishingArea, V9=DepthRange, V10=Species, V11=Stock,V12=CatchCategory, 
                                V13=ReportingCategory, V14=   Sex, V15=CANUMtype, V16=AgeLength, V17=PlusGroup,
                                V18=SampledCatch, V19=NumSamplesLngt, V20=NumLngtMeas, V21=NumSamplesAge, V22=NumAgeMeas,
                                V23=unitMeanWeight, V24=unitCANUM, V25=UnitAgeOrLength, V26=UnitMeanLength, V27=Maturity,
                                V28=NumberCaught, V29=MeanWeight, V30=MeanLength, V31=varNumLanded, V32=varWgtLanded,
                                V33=varLgtLanded  )%>%
                        mutate_if(is.factor,list(~as.character(.)))%>%
                        mutate_if(is.numeric,list(~as.character(.)))%>%
                        mutate_if(is.integer,list(~as.character(.)))),
            file="2020/output/IC_HOM_W_ieo_2019_intercatch.txt",
            quote = F,
            sep = ",",
            na = "",
            dec = ".",
            row.names = F,
            col.names = F )

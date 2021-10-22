# compile indators

library(hurricaneexposure)
library(hurricaneexposuredata)

load("../data/acs/acs_covariates.Rdata")
# makes 'acs_df" which is couty level 

# remove NAs
acs_df <- acs_df[,-which(names(acs_df)%in%c("variable","moe"))]



# loads the county inequality variables for each of the acs variables
load('../data/acs_covariates_inequality.Rdata')
#names(df_county_ineq)[-1] <- paste0(names(df_county_ineq),"_I")[-1]
merged <- merge(acs_df,df_county_ineq,by.x="GEOID",by.y="GEOID",suffixes = c("","_I"))

# load climate opinions ----------------------------------------------------------------------------
load("../data/state_county_climate_transformations.Rdata")
names(df_county_pca)[3] <- "climate_base"
names(df_county_trans_pca)[3] <- "climate_trans"

climate <- merge(df_county_pca,df_county_trans_pca,by="GeoName")
climate <- climate[,-2]
climate <- climate[,c(3,1,2,4)]

merged <- merge(merged,climate,by="GEOID")
# result is have everything saved, cleaned, compiled approproately with the response correctly named
# all the NAs revmoed etc

# add resilience by county to this
load("../data/outages/county_res_WITH_FIPS.Rdata")
merged <- merge(merged,county_res,by.x="GEOID",by.y="fips")

# now remove columns I don't want which might have a bunch of infs or NAs or uselss infor
# for(j in 1:ncol(merged)){
#   
#  if(sum(is.na(merged[,j]))>0){
#    print(j)
#  }
# }

# All done ad hoc
inf_cols <- c(49,59,81,86,87,88)
merged <- merged[,-inf_cols]
merged <- merged[,-c(89,92)]

# load exposure data 

ts_events <- county_events(counties=merged$GEOID,start_year = 2018,end_year = 2018,event_type=c("tropical_storm"))
ts_events <- ts_events[which(ts_events$storm_id=="Michael-2018"),]

ts_distance <- county_distance(counties=merged$GEOID,start_year = 2018,end_year = 2018,dist_limit = 10000)
ts_distance <- ts_distance[which(ts_distance$storm_id=="Michael-2018"),]

# combine exposure data with DF_all
merged$storm_exposure <- FALSE
merged$storm_exposure[which(merged$GEOID%in%ts_events$fips)] <- TRUE

#df_all <- merge(x=df_all,y=ts_distance[,c(2,4)],by.x="fips",by.y="fips")
merged <- merge(merged,ts_distance[,c(2,4)],by.x="GEOID",by.y="fips")
# bin the response by 'low' 'med' 'high'

# add our trinary resilience measure
merged$res1Binned <- 1
merged$res1Binned[which(merged$res1==1)] <- 3
merged$res1Binned[which(merged$res1<1&merged$res1>=0.8)] <- 2





save(merged,file = "../data/all_data_clean.Rdata")

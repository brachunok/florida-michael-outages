# compute all transformations

# (1) use the SOM from the previous as the original
.pardefault <- par()
library(kohonen)
library(RColorBrewer)
library("ggplot2")
theme_set(theme_bw())
library("sf")
library('maps')
library(hurricaneexposure)
library(hurricaneexposuredata)
library("rnaturalearth")
library('viridis')
library("rnaturalearthdata")
library('naturalhearthhires')
library('stringr')
library("ggspatial")
library('dplyr')
data("hurr_tracks")
library('ggrepel')

#source("./som_viz/somComponentPlanePlottingFunction.R")
#source("Map_COUNTY_BMU.R")
#source("./som_viz/plotUMatrix.R")

load("../data/reduced_data_clean_FINAL.Rdata")
df_mc <- df
rm(df)
load("../data/reduced_data_clean_VSURF.Rdata")

df <- cbind(df,df_mc$mdc1yr_65k_to_75k)

names(df)[24] <- "res1"
x <- df[,-c(22:26)]

# from the tuning file, i see that the parameters are generally 
# toroidal = T, 
# and i'll pick a grid size based on Tien 2014 which says i want 
# total_neurns = 5*sqrt(observations) or in this case 40 total
set.seed(5)
#som3 <- som(scale(x),somgrid(5,8,"hexagonal",toroidal = T),rlen=10000)
#save(som3,file = "../code/som_model.Rdata")
load("../code/som_model.Rdata")
summary(som3)
df$original_location <- som3$unit.classif

# set up some plotting objects
world <- ne_countries(scale = "large", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

#counties$area <- as.numeric(st_area(counties))
# bounding box is:-82.2,33.86,-64.04,46.57
florida_box <- c(-88.0473,24.1571,-79.5664,31.1796)

data("county.fips")
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida",counties$ID))

counties$ID <- as.character(counties$ID)

counties$FIPS <- NA
for(c in c(1:nrow(counties))){
  if (c!=46){
    counties$FIPS[c] <- county.fips$fips[which(county.fips$polyname==counties$ID[c])]
  }
}
counties$FIPS[46] <- 12091  
michael_tracks <- hurr_tracks[which(hurr_tracks$storm_id=="Michael-2018"),]
pts <- as.matrix(michael_tracks[,c(4,5)])
# (2) for each row and column, perturb the variable until we see a distance change -----------------
transformation_map <- data.frame(df$NAME,df$GEOID)
transformation_map <- cbind(transformation_map,x)
transformation_map[,c(3:ncol(transformation_map))] <- NA

interested_columns <- c(2,3,4,7,9,14,15,18,21,22)
grid_points <- som3$grid$pts
x_perturb <- x
for(j in interested_columns){
  # for each county
  print(names(x)[j])
  for(i in c(1:nrow(x))){ # THIS IS THE ORIGINAL ONE
  #for(i in 3){
    print(df$NAME[i])
    current_node <- df$original_location[i]
   
    # for each step from 0 to 1 (for most of these) in thousandths increments, 
    # see what node the SOM would be in and evaluate
    increments <- seq(from=0,to=1,length.out = 1000)
    distances <- data.frame(increments)
    distances$distance <- NA
    distances$perturb <- distances$increments-x[i,j]
    for(s in 1:length(increments)){
      x_perturb[i,j] <-increments[s]    
     
      new_som <- kohonen::map(som3, scale(x_perturb))
      new_node<-  new_som$unit.classif[i]
      
      orig_coords <- grid_points[current_node,]
      new_coords  <- grid_points[new_node,]
      
      dist <- sqrt((new_coords[1]-orig_coords[1])^2+(new_coords[2]-orig_coords[2])^2)
      
      distances$distance[s] <- dist
    }  
    x_perturb[i,j] <- x[i,j]
    #make a plot of what we want to show
    par(.pardefault)
    
    if(i==3){
      # if we are in bay county...
      #save(distances,file = paste0("../data/bay_county_transformation/bay_county_transformation_",names(x)[j],".Rdata"))
      
    }
    # jpeg(file=paste0("../plots/transformation_maps/county_variable_changes/",names(x)[j],"movement_for",df$NAME[i],".jpeg"))
    # plot(distance~increments,data=distances,main=paste0("distance vs value of ",names(x)[j], "for county ",df$NAME[i]  ) )
    # points(x=x[i,j],y=0,cpch = 24, cex=2, col="blue", bg="red", lwd=2)
    # dev.off()
    # now get the deviation value required (up or down) to get to a transformation
    
    changes <- distances[which(distances$distance>0),]
    if(nrow(changes)>0){
      # if we actually see changes find the minimum and put it in the dataframe
      transformation_map[i,j+2] <- changes$perturb[which.min(abs(changes$perturb))]
    }
  }
  
  # merge the transformation map data into COUNTIES
  counties <-  merge(counties,transformation_map[,c(2,j+2)],by.x="FIPS",by.y="df.GEOID")
  names(counties)[3] <- "FILL"
  
  # change the date format to plotting for the labels
  michael_tracks$dateEZ <- as.character(as.POSIXct(michael_tracks$date,format="%Y%m%d%H%M"))
  
  # remove michael trcks points which fall outside the plotted boundary. otherwise the 
  # labels crowd the area
  michael_tracks <- michael_tracks[which(michael_tracks$date>="201810091200"&michael_tracks$date<="201810110000"),]
  michael_tracks_label <- michael_tracks[c(3,8),]
  michael_tracks_label$label <- as.character(as.Date(michael_tracks_label$dateEZ))
  # now make a 'cool' plot of the transformation and save it with an appropriate filename 
  # res_plot <- ggplot(data = world) +
  #   geom_sf() +
  #   geom_sf(fill = "antiquewhite1") +
  #   geom_sf(data = states, fill = NA) + 
  #   geom_sf(data = counties,  color = gray(.5),aes(fill=FILL*100)) +
  #   geom_point(data=michael_tracks,aes(x=longitude,y=latitude))+
  #   geom_path(data=michael_tracks,aes(y=latitude,x=longitude))+
  #   scale_fill_viridis_c( alpha = .8,direction = -1) +
  #   coord_sf(xlim = florida_box[c(1,3)], ylim = florida_box[c(2,4)], expand = FALSE) +
  #   annotation_scale(location = "bl", width_hint = 0.4) +
  #   annotation_north_arrow(location = "bl", which_north = "true", 
  #                          pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
  #                          style = north_arrow_fancy_orienteering) +
  #   geom_label_repel(data=michael_tracks_label,aes(x=longitude,y=latitude,label= label))+
  #   xlab("Longitude") + ylab("Latitude") + labs(fill = "% Change")+
  #   ggtitle(paste0("Value of ",names(x)[j], " required to elicit transformation"), subtitle = "During Hurricane Michael") +
  #   theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
  #                                         size = 0.5), panel.background = element_rect(fill = "aliceblue"))
  #res_plot
  
  # counties <- counties[,which(names(counties)!="FILL")]
 #ggsave(filename=paste0("../plots/transformation_maps/",names(x)[j],".pdf"),res_plot,width = 8,height=8,units = "in")
}
save(transformation_map,file = "../data/county_transformations.Rdata")

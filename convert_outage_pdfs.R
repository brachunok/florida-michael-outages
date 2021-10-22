# parse PDFs from here: https://maps.floridadisaster.org/outage_reports/michael/
library('pdftools')
library('tidyverse')

# get all the files with regexes
search_string <- "PRINT"

file_list <- list.files(path ="../data/outages/florida/michael/maps.floridadisaster.org/outage_reports/michael/",pattern = search_string,all.files = T,include.dirs = T,
                        full.names = T)
# create an output document
output <- data.frame(date=character(0),
                     time=character(0),
                     county_name=character(0),
                     county_outages=numeric(0),
                     county_total=numeric(0),
                     state_outages=numeric(0),
                     state_perc=numeric(0),stringsAsFactors = F)

# count # of files
fc <- 1
# for each file, 
for(file in file_list){
  # read file into text
  
  print(file)
  text <- pdf_text(file)%>%readr::read_lines()
  
  header <- text[1:8]
  body   <- text[9:length(text)]
  if(length(grep("MichaelPrintReport",file))>0){
    # slightly different formatting for the earlier files
    header <- text[1:21]
    body   <- text[22:length(text)]
  }
  
  # split into columns
  header <- header %>%str_replace_all(',', '')%>%strsplit(split=" ")
  body <- body %>%str_replace_all(',', '')%>%strsplit(split=" ")
  
  # read line 1 as the datetime and add as column
  header <- header[[2]]
  header <- header[which(header!="")]
  datetime <- header[c(2:3)]
  total_out <- as.numeric(header[which(grepl("Power:",header))+1])
  perc_total_out <- header[length(header)-2]
  perc_total_out <- as.numeric(gsub("%","",perc_total_out)) # This is in 0-100 
  
  # these constants are the offsets for getting the values. They are counted in from the right side
  # in order to get the rightmost column
  OUT_IDX <- 3
  TOTAL_IDX <- 2
  
  #remove all other non-complete columns
  for(county in 1:(length(body)-1)){
    
    this_county <- body[[county]]
    this_county <- this_county[which(this_county!="")]
    
    tcl <- length(this_county) # "This County Length"
    
    name <- this_county[1]
    outs <- this_county[tcl-OUT_IDX]
    total <- this_county[tcl-TOTAL_IDX]
  
    return_row <- c(datetime,name,outs,total,total_out,perc_total_out)
    output[fc,] <-return_row
    fc <- fc+1
  }

  
}

# save

save(output,file = "../data/outages/pdf_scraped_data.Rdata")
write.csv(output,file="../data/outages/pdf_scraped_data.csv")

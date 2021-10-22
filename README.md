# florida-michael-outages
Data and code for the paper "Quantifying Community Transformation to Resilience Traps" by Rachunok &amp; Nateghi

Hurricane Michael outages originaly scraped from [this url](https://maps.floridadisaster.org/outage_reports/michael/). The source was originally used by (Mitsova, 2019)[https://doi.org/10.3390/su11020516] 

If utilizing my processed version of the data, please cite the original source [floridadisaster.org](floridadisaster.org) **and** this paper 

## Contents

### PDF 
The pdf directory contains copies of the original outage reports. These reports were (unfortunately) published in PDF, but are kept here for posterity. 

### Raw Data
The data directory contains the scraped values from the PDFs in CSV and Rdata formats. It is a lightly processed and machine-readible of the PDF versions above. This file is called 'pdf_scraped_data.csv'. It contains hourly power outage counts for each county in florida during hurricane michael. Not to brag, but this was hard to find and there are a million questions to ask about this. If you want to collaborate on projects related to this dataset in any way email me. Another note, this data **is all public** it's just tough to get to. 

### DATA TO REPLICATE
File is named all_data_clean.csv. Names are described in the SI of the mansucript, anything with an I after it is a kolms-inequality measure taken for all tracts in a county of that attribute. 

### Resilience Values
For each county, we calculate an integrated resilience value (scaled area under a performance curve). These are listed in a CSV here. For descriptions of the calculations, please see the supplemental material of the original paper. It also contains plots of the failure and recovery of two sample counties. 

### Code
The PDF scraping code is called convert_outage_pdfs.R . It is a combination of R and bash scripts to open, process, and extract the PDF data into easily readible formats. If you utilize this code, please cite the associated manuscript as well. It should also be noted that this is 'research grade' code and comes with no warrenty or guarentees. This isn't an intellectual contribution of the 

### Engineering resilience model  
files compileIndicators.R, buildBestModel.R, and categoryImprovment.R are the scripts to run (in that order) to create the necessary files to populate the data to train the CCN. They should only require the replication datafile above but may need to be tweaked. 

### SOM training
The file which actually computes transformation trajectory is SOM_changes.R 



If you have general questions please feel free to reach out. If you have questions about using the data, I can point you in general directions and contribute minimally. If you're interested in using a CCN-related idea to do something cool (particularly with respect to resilience) I'd love love love love to collaborate and am happy to contribute more broadly! 

My life is in constant chaos, try these emails: brachunok@gmail.com, rachunok@stanford.edu, brachuno@purdue.edu, barachun@ncsu.edu or personal website (http://brachunok.com) for better contact info. Twitter also works @rachunok.



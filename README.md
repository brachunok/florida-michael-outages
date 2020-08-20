# florida-michael-outages
Data for the paper "Quantifying Community Transformation to Resilience Traps" by Rachunok &amp; Nateghi

Hurricane Michael outages originaly scraped from [this url](https://maps.floridadisaster.org/outage_reports/michael/). The source was originally used by (Mitsova, 2019)[https://doi.org/10.3390/su11020516] 

If utilizing my processed version of the data, please cite the original source [floridadisaster.org](floridadisaster.org) **and** this paper: 

## Contents

### PDF 
The pdf directory contains copies of the original outage reports. These reports were (unfortunately) published in PDF, but are kept here for posterity. 

### Raw Data
The data directory contains the scraped values from the PDFs in CSV and Rdata formats. It is a lightly processed and machine-readible of the PDF versions above. 

### Resilience Values
For each county, we calculate an integrated resilience value (scaled area under a performance curve). These are listed in a CSV here. For descriptions of the calculations, please see the supplemental material of the original paper. 

### Code
The PDF scraping code is in this directory. It is a combination of R and bash scripts to open, process, and extract the PDF data into easily readible formats. If you utilize this code, please cite the associated manuscript as well. It should also be noted that this is 'research grade' code and comes with no warrenty or guarentees. 

If you have general questions please feel free to reach out, brachuno@purdue.edu, or Twitter: @rachunok.




#https://www.bls.gov/jlt/jlt_statedata.htm
#https://www.bls.gov/jlt/jlt_statedata_methodology.htm
#https://www.bls.gov/news.release/pdf/jolts.pdf


# Job Openings           - JO
# Quits                  - Q
# Hires                  - H
# Layoffs and Discharges - LD
# Total Separations      - TS turnover

#https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3

#https://www.bls.gov/opub/hom/pdf/jlt-20130314.pdf

#The job openings rate is computed by
#dividing the number of job openings by the sum 
#of the number of people employed and the number of job openings and
#multiplying the resulting quotient by 100

#The hires rate is computed by dividing the number of people hired by
#the number of people employed and multiplying the resulting
#quotient by 100.

#The quits, layoffs and discharges, and "other separations" rates are
#computed similarly, by dividing the number of workers who,
#respectively, quit their jobs, were laid off or discharged, and
#were otherwise separated, by the number of people employed
#and multiplying the resulting quotient by 100.


library(tidyverse)
library(readr)

my.file <- "X:/JOLTS/jlt_statedata_2018.xlsx"


jolts_state <- readxl::read_xlsx( path = my.file,
                   sheet =  "Table A",
                   skip=2) %>% rename_all(tolower)


jolts_state %>% arrange(st,period)




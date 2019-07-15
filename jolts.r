

# Definition of variables in Excel file
#
# Job Openings           - JO
# Quits                  - Q
# Hires                  - H
# Layoffs and Discharges - LD
# Total Separations      - TS turnover


# Resources
#
#https://www.bls.gov/jlt/jlt_statedata.htm
#https://www.bls.gov/jlt/jlt_statedata_methodology.htm
#https://www.bls.gov/news.release/pdf/jolts.pdf
#https://www.bls.gov/opub/hom/pdf/jlt-20130314.pdf


#Libraries
library(tidyverse)
library(readxl)
library(seasonal)
library(furrr)
library(timetk)
library(scales)
library(plotly)
library(lubridate)


###############################################
# Read in Experimental JOLTS State Data
#

#location of file
my.file <- "E:/JOLTS/jlt_statedata_2018.xlsx" 
jolts_state <- readxl::read_xlsx( path = my.file,
                                  sheet = "Table A",
                                  skip = 2)

#Set all names to lowercase so its less of a pain
#to work with
names( jolts_state ) <- tolower( names( jolts_state ) )


########################################
# Add a date Variable 
# for easier processing later
jolts_state <- jolts_state %>% 
        mutate( date = as.Date( paste0( substr(period,1,4), "-", substr(period,5,6), "-01" ), 
                                format = "%Y-%m-%d" ))


######################################################
#
# Some of the states are highly seasonal and it makes 
# it difficult to see true economic shifts
#
# I seasonally adjust it with X-13-ARIMA-SEATS using the
# seasonal package and use the furrr package to paralellize
# the code to make it run faster

plan(multiprocess) #set up multiprocessor job
jolts_state_adj <- jolts_state %>% 
                   nest( -st ) %>% #Nest the dataset, convert series to ts objects, seasonally all series,
                                 #future_map is the workhorse for parallel, tk_tbl converts ts back to tibble
                   mutate( jor_seas = future_map(data, ~tk_tbl(final( seas  (ts ( .x$jor, frequency=12, start=c(2001,2)),transform.function = "log")))),
                           qr_seas =  future_map(data, ~tk_tbl(final( seas  (ts ( .x$qr,  frequency=12, start=c(2001,2)),transform.function = "log")))),
                           ldr_seas = future_map(data, ~tk_tbl(final( seas  (ts ( .x$ldr, frequency=12, start=c(2001,2)),transform.function = "log")))))


#unnest dataset, get rid of TS object indexes, rename the series back to their respective names (suffixed with s for seasonally adjusted)
jolts_state_adj <- jolts_state_adj %>%
                   unnest( ) %>% 
                   select( -index, -index1, -index2 ) %>%
                   rename( jors = value, qrs = value1, ldrs = value2 )



####
# Create ggplot of 
#

gg <- ggplot(jolts_state_adj, aes( x = jors,
                                   y = ldrs,
                                   text = paste('Date: ',paste0(year(date),"-",month(date)), #Create hover tool-tips
                                               '<br>State:', st, 
                                               '<br>Job Opening Rate:', sprintf("%1.3f%%", 100*jors),
                                               '<br>Layoff/Discharge Rate:',sprintf("%1.3f%%", 100*ldrs)))) +
  geom_text(aes(frame = paste0(year(date),"-",sprintf("%02d",month(date))), #Draw the points as state abbreviations
                label=st, 
                ids = st   ),size=2.5) +
  scale_y_continuous(labels=percent) + #make x and y percentages
  scale_x_continuous(labels=percent) +
  xlab("Job Openings Rate") + 
  ylab("Layoff/Discharges Rate") +
  ggtitle("Seasonally Adjusted Job Opening Rates <br> vs Layoff/Discharge Rates") + 
  geom_abline(slope=1,intercept=0) +  #y=x line, if points are above this, state is laying off more than opening
  theme(plot.title = element_text(hjust = 0.5)) #center title


#####
# Create plotly object and render it
#
ggplotly(gg,tooltip = "text") %>% 
  config(displayModeBar = F) %>%   #remove plotly menu bar
  layout(margin=list(t=100)) %>%   #make top margin large to fit title
  animation_slider(                #create prefix for animation (year, month)
    currentvalue = list(prefix = "YEAR/MONTH: ", font = list(color="blue"))
  )



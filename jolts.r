
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
library(magrittr)


my.file <- "E:/JOLTS/jlt_statedata_2018.xlsx"


jolts_state <- readxl::read_xlsx( path = my.file,
                   sheet = "Table A",
                    skip = 2)


names(jolts_state) <- tolower(names(jolts_state))

temp <- jolts_state %>% 
        mutate( date = as.Date( paste0( substr(period,1,4), "-", substr(period,5,6), "-01" ), 
                                format="%Y-%m-%d" ),
                year = as.numeric(substr(period,1,4)) )


st_avg <- temp %>% group_by(st,year) %>% summarise( jo = mean( jo ), 
                                                    q = mean(q), 
                                                    ld = mean(ld), 
                                                    jor = mean( jor ), 
                                                    qr = mean(qr), 
                                                    ldr = mean(ldr) )

st_avg %>% filter( year %in% c("2010","2018") ) %>% 
  ggplot( aes( x=jo, y=q, color=as.factor(year) ) ) + geom_point()


st_avg %>% ggplot( aes( x = year, y = q, col = st) ) + geom_line()


temp %>% filter( st == "NM" ) %>%  ggplot(aes(x=date,y=jo)) + geom_line() 




st_avg


p <- temp %>% ggplot(aes(x = jor, y=ldr)) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  geom_text( aes(label=st),hjust=0, vjust=0) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  #scale_x_log10() +
  #scale_y_log10() + 
  labs(x ="Job Openings", y = "Layoffs and Discharges")


p + transition_states(date, 
                      transition_length = 2,
                      state_length =1 )  + ease_aes('cubic-in-out') 

final <- p + transition_time(as.numeric(year)) +
  

anim_save( "lay_offs_v_job_openings.gif")


#############################################################################
#############################################################################

# Create example data
df <- data.frame(ordering = c(rep(1:3, 2), 3:1, rep(1:3, 2)),
                 year = factor(sort(rep(2001:2005, 3))),
                 value = round(runif(15, 0, 100)),
                 group = rep(letters[1:3], 5))

library("gganimate")
library("ggplot2")

# Create animated ggplot with coord_flip
ggp <- ggplot(df, aes(x = ordering, y = value)) +
  geom_bar(stat = "identity", aes(fill = group)) +
  transition_states(year, transition_length = 2, state_length = 0) +
  view_follow(fixed_x = TRUE) +
  coord_flip() +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        plot.margin = unit(c(1, 1, 8, 1), "cm"))



ggp

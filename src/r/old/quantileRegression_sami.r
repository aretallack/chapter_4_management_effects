# sample dataset
ss <- DF[which(cell %in% sample_cells), ]

# Calculate "climatology" or monthly means, and SD of PV per month across years
# Mean and standard deviation per location per month 
# Creates reference values from pre-2009
ss_refs <- ss[ddate < 2009][,.(pv_u = mean(pv,na.rm = T), 
                               pv_sd = sd(pv,na.rm = T),
                               npv_u = mean(npv, na.rm = T),
                               npv_sd = sd(pv, na.rm = T),
                               bare_u = mean(bare, na.rm = T),
                               bare_sd = sd(bare, na.rm = T)), by = .(x, y, month)]

# Add these summary stats to original dataset
ss <- merge(ss, ss_refs, by = c("x", "y", "month"))

# calculation of anomalies, and z-score transformed anomalies
# Differece of each pv value to the mean pv value for that cell and month (calculated before 2009)
ss[,`:=`(pv_anom = pv - pv_u)] # abs anomaly, by month
ss[,`:=`(pv_anom_sd = pv_anom/pv_sd)] # z-score transformed anomaly
ss[,`:=`(npv_anom = npv - npv_u)]
ss[,`:=`(npv_anom_sd = npv_anom/npv_sd)]
ss[,`:=`(bare_anom = bare - bare_u)]
ss[,`:=`(bare_anom_sd = bare_anom/bare_sd)]

# aggregration to year, for min/avg/max of pv_anom
# i.e. per cycle minimum, maximum and mean (annual cycles)
# per station and nvis group
ss2 <- ss[,.(pv_anom_min = min(pv_anom, na.rm = T),
           pv_anom_avg = mean(pv_anom, na.rm = T),
           pv_anom_max = max(pv_anom, na.rm = T),
           npv_anom_min = min(npv_anom, na.rm = T),
           npv_anom_avg = mean(npv_anom, na.rm = T),
           npv_anom_max = max(npv_anom, na.rm = T),
           bare_anom_min = min(bare_anom, na.rm = T),
           bare_anom_avg = mean(bare_anom, na.rm = T),
           bare_anom_max = max(bare_anom, na.rm = T)), 
        by = .(x, y, station, nvis_group, year)]


# visualize trends of anoms
# Plots increase in average, max and minimum anomolies


compare_stations <- function(stations, cover_type){
  data <- ss2 %>% 
    select(year,station,starts_with(cover_type)) %>% 
    pivot_longer(-c("year","station"))
  
    ggplot(data, aes(year,value, group = station)) +
    geom_smooth(method = 'lm', se = F, col = 'grey', lwd = 0.5) +
    geom_smooth(data = data %>% filter(station %in% stations), 
                method = 'lm', se = F,
                aes(colour = station),
                lwd = 2) + 
    facet_wrap(~name, dir = "h")
  }

compare_refs <- function(stations) {
  data <- ss %>% 
  select(year, station, pv_u) %>% 
  filter(year < 2009) %>%
  group_by(station) %>% 
  summarise(mean = mean(pv_u))

ggplot(data, aes(mean, station)) +
  geom_bar(stat = "identity", fill = "grey70") +
  geom_bar(data = data %>% filter(station %in% stations), stat = "identity", aes(fill = station))
}

compare_stations(c("Yellabinna RR", "Wilgena", "Mulgathing", "Mobella"), "pv")
compare_stations(c("Bon Bon", "Mount Eba", "Coondambo", "North Well", "Mount Vivian"), "npv")
compare_stations(c("Mount Eba", "Bon Bon"), "pv")

compare_refs(c("Yellabinna RR", "Wilgena", "Mulgathing", "Mobella"))
compare_refs(c("Bon Bon", "Mount Eba", "Coondambo", "North Well", "Mount Vivian"))
  

# 
# # 'simple' linear model of 
# jj1 <- gam(I(pv_anom_max + 10) ~ 
#              # te(x,y)+
#              # s(nvis_group, bs='re') + # random effect
#              year * station, 
#        data = ss2[year >= 2008], 
#        family = Gamma(link = 'log'))
# summary(jj1)

# library(visreg)
# visreg(jj1, xvar='year',by='station')

ss2 %>% select(x,y) %>% colMeans()

# simulate response to visualize the interaction of station with time

expand_grid(year = 2008:2022,
            station = ss$station %>% unique, 
            x=558589.7,
            y=6636685.1 ) %>% 
  mutate(pred = predict(jj1,newdata=., type='response')) %>% 
  ggplot(data=.,aes(year, pred,color=station))+
  geom_line(aes(group=station),col='black',lwd=1)+
  geom_line()+
  geom_line(data=. %>%
              filter(station=="Yellabinna RR"),
            col='red',
            lwd=2)+
  geom_line(data=. %>%
              filter(station=="Bon Bon"),
            col='blue',
            lwd=2)+
  scale_color_viridis_d(option='H')+
  labs(y = "Estimated annual-max-PV anomaly trend")




# reconstructing the predictions 
length(coef(jj1)) # coefficients estimated from the model
dim(model.matrix(jj1)) # model matrix, is all of the covariates for each row, in matrix form

vec_jnk1 <- model.matrix(jj1) %*% coef(jj1) # matrix multiplication of covariates by coefficients
vec_jnk1 <- as.array(vec_jnk1[,1]) # cast to array

predict(jj1, type = 'response') == as.array(vec_jnk1) # the in-built prediction is equal to our manual reconstruction


ss[year %in% 2001:2008][,.(val = mean(pv,na.rm = T)),by = station][order(station)]
ss[year %in% 2020:2022][,.(val = mean(pv,na.rm = T)),by = station][order(station)]



# do geom line, overlay bb in different col. 
ss[,.(val = mean(pv,na.rm = T)), by = .(year,station)] %>% 
  ggplot(data = .,aes(year, val,group = station)) +
  geom_smooth(method = 'lm', 
              se = F, lwd = 0.5, col = 'grey60') +
  geom_smooth(method = 'lm',se = F,
            data = . %>% filter(station == "Yellabinna RR"), 
            col = 'red',lwd = 2) + 
  geom_smooth(method = 'lm',se = F,
            data = . %>% filter(station == "Bon Bon"), 
            col = 'green',lwd = 2) + 
  geom_smooth(method = 'lm',se = F,
            data = . %>% filter(station == "Coondambo"), 
            col = 'black',lwd = 2) + 
  geom_smooth(method = 'lm',se = F,
            data = . %>% filter(station == "Mount Eba"), 
            col = 'blue',lwd = 2) + 
  geom_smooth(method = 'lm',se = F,
            data = . %>% filter(station == "North Well"), 
            col = 'orange',lwd = 2)


ss[,.(val = pv), by = .(year,station)] %>% 
  ggplot(data = .,aes(year, val,group = station)) +
  geom_smooth(stat = "smooth",
              method = "gam",
              se = F, lwd = 0.5, col = 'grey60') +
  geom_smooth(data = ss2 %>% 
                select(year,station,starts_with(c('pv'))) %>% 
                pivot_longer(-c("year","station")) %>% 
                filter(station == "Yellabinna RR" & name == "pv_anom_max"),
              aes(year, value),
              col = 'red',
              method = 'lm',
              se = F) + 
  geom_smooth(stat = 'smooth',
              method = 'gam',
              se = F,
              data = . %>% filter(station == "Yellabinna RR"),
              col = 'red',lwd = 2)
  # geom_smooth(stat = 'smooth',se = F,
  #             data = . %>% filter(station == "Yellabinna RR"),
  #             col = 'red',lwd = 2) +
  # geom_smooth(method = 'lm',se = F,
  #             data = . %>% filter(station == "Bon Bon"), 
  #             col = 'green',lwd = 2) + 
  # geom_smooth(method = 'lm',se = F,
  #             data = . %>% filter(station == "Coondambo"), 
  #             col = 'black',lwd = 2) + 
  # geom_smooth(method = 'lm',se = F,
  #             data = . %>% filter(station == "Mount Eba"), 
  #             col = 'blue',lwd = 2) + 
  # geom_smooth(method = 'lm',se = F,
  #             data = . %>% filter(station == "North Well"), 
  #             col = 'orange',lwd = 2)



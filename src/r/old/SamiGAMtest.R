tmp2 <- DFtrain[,.(nobs = .N), by=.(x,y)]
tmp2 %>% ggplot(data=.,aes(x,y,fill=nobs))+geom_tile()+coord_sf()


as.POSIXct(paste(year,month,1),"ymd")




m3 <- lme(pv ~ 1,
    random= ~ 1|cell,
    correlation = corAR1(), 
    data=tmp3 %>% mutate(cell = as.factor(cell)))
summary(m3)


lm(pv~1, data=tmp3)
tmp3$pv %>% mean



m4 <- bam(pv ~ 
      # continuous var, grouping factor, bs='factor smooth'
      s(ddate, fcell, bs='fs') + 
      s(month, bs='cc') +
      s(p_img_a,k=5) + 
      te(x, y, fx = T),
    # family = gaussian(),
    family = tw(),
    select = TRUE,
    # rho = 0.79,
    # AR.start = (tmp3$ddate == (tmp3$ddate %>% min)),
    method = "fREML",
    discrete = T,
    data = tmp3 %>% 
      mutate(fcell = as.factor(cell), 
             fl_sys = droplevels(l_sys)) %>% 
      as.data.table())
summary(m4)
plot(m4, pages=1)

# Including land_system as grouping variable on rainfall
m5 <- bam(pv ~ 
            # continuous var, grouping factor, bs='factor smooth'
            s(ddate, fcell, bs='fs') + 
            s(month, bs='cc') +
            s(p_img_a, l_sys, bs = "fs", k=5) + 
            te(x, y, fx = T),
          # family = gaussian(),
          family = tw(),
          select = TRUE,
          # rho = 0.79, # taken from m3 to deal with temporal autocorrelation
          # AR.start = (tmp3$ddate == (tmp3$ddate %>% min)),
          method = "fREML",
          discrete = T,
          data = tmp3 %>% 
            mutate(fcell = as.factor(cell), 
                   fl_sys = droplevels(l_sys)) %>% 
            as.data.table())
summary(m5)
plot(m5, pages=1)

# Grouping rainfall by station. 
m6 <- bam(pv ~ 
            # continuous var, grouping factor, bs='factor smooth'
            s(ddate, fcell, bs='fs') + 
            s(month, bs='cc') +
            s(p_img_a, sttn, bs = "fs", k=5) + 
            te(x, y, fx = T),
          # family = gaussian(),
          family = tw(),
          select = TRUE,
          # rho = 0.79, # taken from m3 to deal with temporal autocorrelation
          # AR.start = (tmp3$ddate == (tmp3$ddate %>% min)),
          method = "fREML",
          discrete = T,
          data = tmp3 %>% 
            mutate(fcell = as.factor(cell), 
                   fl_sys = droplevels(l_sys)) %>% 
            as.data.table())
summary(m6)
plot(m6, pages = 1)



f_fit_mods <- function(station_name, nsamps){
  out <- bam(pv ~ 
              # continuous var, grouping factor, bs='factor smooth'
              s(ddate, fcell, bs = 'fs') + 
              s(month, bs = 'cc') +
              s(p_img_a, k = 5) + 
              te(x, y, fx = T),
            family = gaussian(),
            select = TRUE,
            # rho = 0.79,
            # AR.start = (tmp3$ddate == (tmp3$ddate %>% min)),
            method = "fREML",
            discrete = T,
            data = DF[sttn == station_name][sample(.N, nsamps)] %>% 
              mutate(fcell = as.factor(cell)) %>% 
              as.data.table()
            )
 return(out)
}

l_mods <- as.vector(unique(DF$sttn)) %>% lapply(f_fit_mods, nsamps = 50)

pacman::p_load(data.table,lubridate,tidyverse,
               mgcv,mgcViz,gratia, 
               patchwork)

dat <- arrow::read_parquet("data/BonBonDF.parquet")
dat[,`:=`(tsd = if_else(is.na(tsd)==T, 0, tsd))]

# hypothetical broadscale model
m4 <- bam(pv ~ 
            te(x, y, fx = T, k = 10) +
            # s(en34, bs='ts', k=3)+
            s(ddate,by=station,
              bs=c('bs')) + # umbrella term for other climate stuff
            s(month, bs = "cc") +
            s(tmean_3, k = 5) +
            s(vpd_3, k = 5) +
            p_img_3*nvis_group + 
            p_img_12*nvis_group
          # p_img_36*nvis_group
          ,
          method = "fREML",
          discrete = T,
          data = dat)
summary(m4)

# finer scale model 
m3 <- bam(pv ~ +
       # te(x, y, fx = T, k = 20) +
       # stocked*ddate +
       tsd + 
       s(ddate,  bs='ts') + # umbrella term for other climate stuff
       # s(ddate, station, k = 20, bs = c("fs")) +
     s(month, bs = "cc") +
       s(tmean_3, k = 5) +
       s(vpd_3, k = 5) +
       p_img_3*nvis_group + 
       p_img_12*nvis_group
       # p_img_36*nvis_group
       ,
           method = "fREML",
           discrete = T,
           data = dat)
summary(m3)
plot(m3,pages=1)


# uses mgcViz
# fast way to quickly visualize all terms, including parametric
fn_viz <- function(mod){
  getViz(mod) %>% 
    plot(allTerms=T) %>% 
    print(pages=1)
}
fn_viz(m4)



dat[,.(val = sum(tsd!=0)),by=ddate] %>% 
  ggplot(data=.,aes(ddate,val))+
  geom_line()




mod <- m3
smooth_term <- "s(month)"


fn_viz_1 <- function(mod, smooth_term){
  # get xvariable name
  xvar <- str_remove(smooth_term, "s\\(") %>% 
    str_remove(., "\\)")
  
  p_out <- gratia::smooth_estimates(mod) %>% 
    filter(smooth==smooth_term) %>% 
    # pivot_longer(cols=c(est,se,month)) %>% 
    ggplot(aes( !!ensym(xvar), est))+
    geom_ribbon(aes(!!ensym(xvar), 
                    ymax=est+1.96*se, # assuming an "identity" link
                    ymin=est-1.96*se), 
                alpha=0.5, 
                lty=0)+
    geom_line()+
    labs(x=xvar,
         y="Partial Effect")+
    coord_cartesian(expand=F)+
    theme_linedraw()+
    theme(panel.grid = element_blank())
  
  return(p_out)
}

vec_sm <- gratia::smooths(m3)
l_p <- lapply(vec_sm, fn_viz_1, mod=m3)

patchwork::wrap_plots(l_p, ncol = 2)


fn_viz_1(m3,"s(ddate)")
fn_viz_1(m3,"s(tmean_3)")
fn_viz_1(m3,"s(vpd_3)")






str_detect(vec_sm, ",")

fn_viz_1(m3,"s(month)")






vec_sm2 <- smooths(m4)


gratia::smooth_estimates(m4) %>% 
  filter(smooth %in% vec_sm2[2:6]) %>% 
  ggplot(aes(ddate, est, color=station))+
  # geom_ribbon(aes(!!ensym(xvar), 
  #                 ymax=est+1.96*se, # assuming an "identity" link
  #                 ymin=est-1.96*se), 
  #             alpha=0.5, 
  #             lty=0)+
  geom_line(
    aes(group=station),
    lwd=1.5, 
    color='black'
  )+
  geom_line()+
  geom_line(
    data=. %>% 
      filter(station=='Bon Bon'), 
    lwd=2, 
    col='red'
  )+
  labs(x="Date",
       y="Partial Effect")+
  scale_color_viridis_d(option='H')+
  coord_cartesian(expand=F)+
  theme_linedraw()+
  theme(panel.grid = element_blank())

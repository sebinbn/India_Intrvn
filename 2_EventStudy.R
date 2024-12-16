# this file performs the event study on individual announcements


# Event study on slope ----------------------------------------------------

Merge_dat$s101_1d = c(NaN,diff(Merge_dat$s101))
Merge_dat$s101_2d = c(NaN,Merge_dat$s101[3:nrow(Merge_dat)] - 
                        Merge_dat$s101[1:(nrow(Merge_dat)-2)],NaN)  #calculating 2 day differences as tomorrow - yesterday

EvStudy_Ann = data.frame(Date = Twist_Dates$Announcement,
                         matrix(NaN,nrow(Twist_Dates),4))
colnames(EvStudy_Ann)[-1] = c("D_1d","D_2d","pval_1d","pval_2d")
EvStudy_Ann$D_1d = Merge_dat$s101_1d[Merge_dat$D_Ann == 1]
EvStudy_Ann$D_2d = Merge_dat$s101_2d[Merge_dat$D_Ann == 1]
sd_1d = sd(Merge_dat$s101_1d[Merge_dat$Date >= as.Date("2019-12-01") &
                       Merge_dat$Date <= as.Date("2021-05-31")], na.rm = T)
sd_2d = sd(Merge_dat$s101_2d[Merge_dat$Date >= as.Date("2019-12-01") &
                             Merge_dat$Date <= as.Date("2021-05-31")], na.rm = T)
EvStudy_Ann$pval_1d = (1-pnorm(abs(EvStudy_Ann$D_1d)/sd_1d))*2  
EvStudy_Ann$pval_2d = (1-pnorm(abs(EvStudy_Ann$D_2d)/sd_2d))*2
pnorm(10)
pnorm(-0.083/sd_2d)
mean(Merge_dat$s101_2d, na.rm = T)

Merge_dat
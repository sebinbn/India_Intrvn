# this file extracts values from the results of Intervention Analysis stored in 
# lists and arranges them into tables TF_tab, Int_tab & Int_Auc_tab



# Transfer function results filling in  -----------------------------------

TF_tab = matrix(NaN, 15, 8)
rownames(TF_tab) = c("Liq","Liq_5","WACR_3","WACR_12", "EFFR_1", "EFFR_12",
                     "DGS10","DGS10_1","DGS10_5", "ar1", "ar2", "Obs", "LogLik",
                     "sigma","AIC")
colnames(TF_tab) = c("s101", "Yr10","Yr1","OIS1yr","s101_se", "Yr10_se","Yr1_se","OIS1yr_se")

TF_tab[names(TF_101$coef),"s101"] = TF_101$coef
TF_tab[names(TF_101$coef),"s101_se"] = sqrt(diag(TF_101$var.coef))
TF_tab[names(TF_10$coef),"Yr10"] = TF_10$coef
TF_tab[names(TF_10$coef)[-1],"Yr10_se"] = sqrt(diag(TF_10$var.coef))
TF_tab[names(TF_1$coef),"Yr1"] = TF_1$coef
TF_tab[names(TF_1$coef),"Yr1_se"] = sqrt(diag(TF_1$var.coef))
TF_tab[rownames(TF_OIS$coefficients),"OIS1yr"] = TF_OIS$coefficients[,"Estimate"]
TF_tab[rownames(TF_OIS$coefficients),"OIS1yr_se"] = TF_OIS$coefficients[,"Std. Error"]
TF_tab["Obs",1:4] = c(TF_101$nobs,TF_10$nobs,TF_1$nobs, length(TF_OIS$residuals))
TF_tab["LogLik",1:4] = c(TF_101$loglik,TF_10$loglik,TF_1$loglik, TF_OIS$adj.r.squared)
TF_tab["AIC",1:4] = c(TF_101$aic,TF_10$aic,TF_1$aic, TF_OIS$fstatistic["value"])
TF_tab["sigma",1:4] = c(TF_101$sigma2,TF_10$sigma2,TF_1$sigma2, TF_OIS$sigma^2)

# Intervention Analysis results filling in -------------------------------------

# Announcement analysis ----------------------------------------------------

Int_tab = matrix(NaN, 16, 8)
rownames(Int_tab) = c("D_Ann", rownames(TF_tab))
colnames(Int_tab) = c("s101", "Yr10","Yr1","OIS1yr","s101_se", "Yr10_se","Yr1_se","OIS1yr_se")

Int_tab[names(Int_101$coef),"s101"] = Int_101$coef
Int_tab[names(Int_101$coef),"s101_se"] = sqrt(diag(Int_101$var.coef))
Int_tab[names(Int_10$coef),"Yr10"] = Int_10$coef
Int_tab[names(Int_10$coef)[-1],"Yr10_se"] = sqrt(diag(Int_10$var.coef))
Int_tab[names(Int_1$coef),"Yr1"] = Int_1$coef
Int_tab[names(Int_1$coef),"Yr1_se"] = sqrt(diag(Int_1$var.coef))
Int_tab[rownames(Int_OIS$coefficients),"OIS1yr"] = Int_OIS$coefficients[,"Estimate"]
Int_tab[rownames(Int_OIS$coefficients),"OIS1yr_se"] = Int_OIS$coefficients[,"Std. Error"]
Int_tab["Obs",1:4] = c(Int_101$nobs,Int_10$nobs,Int_1$nobs, length(Int_OIS$residuals))
Int_tab["LogLik",1:4] = c(Int_101$loglik,Int_10$loglik,Int_1$loglik, Int_OIS$adj.r.squared)
Int_tab["AIC",1:4] = c(Int_101$aic,Int_10$aic,Int_1$aic, Int_OIS$fstatistic["value"])
Int_tab["sigma",1:4] = c(Int_101$sigma2,Int_10$sigma2,Int_1$sigma2, Int_OIS$sigma^2)


## Auction Analysis --------------------------------------------------------

Int_Auc_tab = matrix(NaN, 16, 8)
rownames(Int_Auc_tab) = c("D_Auc", rownames(TF_tab))
colnames(Int_Auc_tab) = c("s101", "Yr10","Yr1","OIS1yr","s101_se", "Yr10_se","Yr1_se","OIS1yr_se")

Int_Auc_tab[names(Int_101_Auc$coef),"s101"] = Int_101_Auc$coef
Int_Auc_tab[names(Int_101_Auc$coef),"s101_se"] = sqrt(diag(Int_101_Auc$var.coef))
Int_Auc_tab[names(Int_10_Auc$coef),"Yr10"] = Int_10_Auc$coef
Int_Auc_tab[names(Int_10_Auc$coef)[-1],"Yr10_se"] = sqrt(diag(Int_10_Auc$var.coef))
Int_Auc_tab[names(Int_1_Auc$coef),"Yr1"] = Int_1_Auc$coef
Int_Auc_tab[names(Int_1_Auc$coef),"Yr1_se"] = sqrt(diag(Int_1_Auc$var.coef))
Int_Auc_tab[rownames(Int_OIS_Auc$coefficients),"OIS1yr"] = Int_OIS_Auc$coefficients[,"Estimate"]
Int_Auc_tab[rownames(Int_OIS_Auc$coefficients),"OIS1yr_se"] = Int_OIS_Auc$coefficients[,"Std. Error"]
Int_Auc_tab["Obs",1:4] = c(Int_101_Auc$nobs,Int_10_Auc$nobs,Int_1_Auc$nobs, length(Int_OIS_Auc$residuals))
Int_Auc_tab["LogLik",1:4] = c(Int_101_Auc$loglik,Int_10_Auc$loglik,Int_1_Auc$loglik, Int_OIS_Auc$adj.r.squared)
Int_Auc_tab["AIC",1:4] = c(Int_101_Auc$aic,Int_10_Auc$aic,Int_1_Auc$aic, Int_OIS_Auc$fstatistic["value"])
Int_Auc_tab["sigma",1:4] = c(Int_101_Auc$sigma2,Int_10_Auc$sigma2,Int_1_Auc$sigma2, Int_OIS_Auc$sigma^2)


# 24 dummy regression results ---------------------------------------------

# This fills in values of covariates and not the 24 dummies.
Int_cum_tab = matrix(NaN, 16, 8)
rownames(Int_cum_tab) = c(rownames(TF_tab))
colnames(Int_cum_tab) = c("s101", "Yr10","Yr1","OIS1yr","s101_se", "Yr10_se","Yr1_se","OIS1yr_se")

x101 = names(Int_101_Cum$coef)[!names(Int_101_Cum$coef) %in% paste("D_Ann_",1:24,sep = "")]
x10 = !names(Int_10_Cum$coef) %in% paste("D_Ann_",1:24,sep = "")
x1 = !names(Int_1_Cum$coef) %in% paste("D_Ann_",1:24,sep = "")
xOIS = !names(Int_OIS_Cum$coef) %in% paste("D_Ann_",1:24,sep = "")
Int_cum_tab[x101,"s101"] = Int_101_Cum$coef[x101]
Int_cum_tab[names(Int_101_Cum$coef[x101]),"s101_se"] = sqrt(diag(Int_101_Cum$var.coef)[x101])
Int_cum_tab[names(Int_10_Cum$coef[x10]),"Yr10"] = Int_10_Cum$coef[x10]
Int_cum_tab[names(Int_10_Cum$coef[x10])[-1],"Yr10_se"] = sqrt(diag(Int_10_Cum$var.coef)[x10])
Int_cum_tab[names(Int_1_Cum$coef[x1]),"Yr1"] = Int_1_Cum$coef[x1]
Int_cum_tab[names(Int_1_Cum$coef[x1]),"Yr1_se"] = sqrt(diag(Int_1_Cum$var.coef)[x1])
Int_cum_tab[rownames(Int_OIS$coefficients[xOIS]),"OIS1yr"] = Int_OIS$coefficients[xOIS,"Estimate"]
Int_cum_tab[rownames(Int_OIS$coefficients),"OIS1yr_se"] = Int_OIS$coefficients[,"Std. Error"]
Int_cum_tab["Obs",1:4] = c(Int_101_Cum$nobs,Int_10_Cum$nobs,Int_1_Cum$nobs, length(Int_OIS$residuals))
Int_cum_tab["LogLik",1:4] = c(Int_101_Cum$loglik,Int_10_Cum$loglik,Int_1_Cum$loglik, Int_OIS$adj.r.squared)
Int_cum_tab["AIC",1:4] = c(Int_101_Cum$aic,Int_10_Cum$aic,Int_1_Cum$aic, Int_OIS$fstatistic["value"])
Int_cum_tab["sigma",1:4] = c(Int_101_Cum$sigma2,Int_10_Cum$sigma2,Int_1_Cum$sigma2, Int_OIS$sigma^2)
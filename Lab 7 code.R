d_educ <- data.frame(model.matrix(~ dat_use1$EEDUC))
summary(d_educ)
levels(dat_use1$EEDUC)

d_marstat <- data.frame(model.matrix(~ dat_use1$MS))
d_race <- data.frame(model.matrix(~ dat_use1$RRACE))
d_hispanic <- data.frame(model.matrix(~ dat_use1$RHISPANIC))
d_gender <- data.frame(model.matrix(~ dat_use1$GENID_DESCRIBE))
d_region <- data.frame(model.matrix(~ dat_use1$REGION))

d_vaxx <- data.frame(model.matrix(~ dat_use1$vaxx))

dat_for_analysis_sub <- data.frame(
  d_vaxx[,2],

  d_educ[!is.na(dat_use1$vaxx),2:7],
  d_marstat[!is.na(dat_use1$vaxx),2:6],
  d_race[!is.na(dat_use1$vaxx),2:4],
  d_hispanic[!is.na(dat_use1$vaxx),2],
  d_gender[!is.na(dat_use1$vaxx),2:5],
  d_region[!is.na(dat_use1$vaxx),2:4]) 



names(dat_for_analysis_sub) <- sub("dat_use1.","",names(dat_for_analysis_sub))
names(dat_for_analysis_sub)[1] <- "vaxx"
names(dat_for_analysis_sub)[17] <- "Hispanic"

require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$vaxx)
restrict_1 <- (runif(NN) < 0.2) 
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)

sobj <- standardize(vaxx ~ EEDUCHS.diploma + EEDUCsome.coll + EEDUCassoc.deg + EEDUCbach.deg + EEDUCadv.deg + 
                      MSmarried + MSwidowed + MSdivorced + MSseparated + MSnever + RRACEBlack + RRACEAsian + RRACEOther +
                      hispanic + GENID_DESCRIBEmale + GENID_DESCRIBEfemale + GENID_DESCRIBEtransgender + GENID_DESCRIBEother +
                      REGIONSouth + REGIONMidwest + REGIONWest
                    , dat_train, family = binomial)

s_dat_test <- predict(sobj, dat_test)

model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > mean(pred_vals_lpm))
table(pred = pred_model_lpm1, true = dat_test$vaxx)
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$vaxx)


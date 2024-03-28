# data <- d
# head(data)
# write.csv(data, file = "data_eng_spain.csv")

rm(list = ls())
load("incubation_data_25022022.RData")
data <- d

head(data)

# exclude
# ind1 <- data_raw[, "covid_exp"] == "" # sum(!ind1)
# ind2 <- data_raw[, "test_result"] == "Positive" ## sum(!ind2)
# data <- data_raw[ind1 & ind2, ]
# head(data)

vaccine <- data[, "vaccine"]
vactype <- data[, "vaccine_1"]

sex <- data[, "sex"]
age <- data[, "age"]

d_symptoms <- as.Date(data$d_symptoms)
d_infection_max <- as.Date(data$d_infection_max)
d_infection_min <- as.Date(data$d_infection_min)

max_incperiod <- 15

# indicating Case 4
wo_ind <- !(is.na(d_infection_min) & is.na(d_infection_max))

# no min_exposure
note1 <- which(is.na(d_infection_min))
d_infection_min[note1] <- d_symptoms[note1] - max_incperiod # could be 14

# no max_exposure
note2 <- which(is.na(d_infection_max))
d_infection_max[note2] <- d_symptoms[note2]

# # same days for all
note3 <- which(d_infection_min == d_symptoms)
# d_infection_max[note3] <- d_infection_max[note3] - 0.5
d_infection_min[note3] <- d_infection_min[note3] - 0.5

## Data analysis 
# vaccine Yes (at least one) vs No
vaccine_x <- ifelse(vaccine == "Yes", 1, 0) 

# vaccine Complete vs Incomplete
vacstat_x <- ifelse(is.na(data[, "vaccine_1"]), 0, NA) # "Incomplete"
vacstat_x[!is.na(data[, "vaccine_1"]) & !is.na(data[, "vaccine_2"])] <- 1 # "Complete"
vacstat_x[!is.na(data[, "vaccine_1"]) & is.na(data[, "vaccine_2"])] <- 0 # "Incomplete"
vacstat_x[data[, "vaccine_1"] == "Janssen"] <- 1 # "Complete"

# cbind(vacstat_x, data[, c("vaccine_1", "vaccine_2")])
vactype_x <- matrix(0, length(vacstat_x), 2)
vactype_x[data[, "vaccine_1"] %in% c("Pfizzer", "Moderna"), 1] <- 1
vactype_x[data[, "vaccine_1"] %in% c("Janssen", "Astrazeneca"), 2] <- 1
colnames(vactype_x) <- c("mRNA", "Adeno")

vackind_x <- matrix(0, length(vacstat_x), 4)
vackind_x[data[,"vaccine_1"] == "Pfizzer", 1] <- 1
vackind_x[data[,"vaccine_1"] == "Moderna", 2] <- 1
vackind_x[data[,"vaccine_1"] == "Janssen", 3] <- 1
vackind_x[data[,"vaccine_1"] == "Astrazeneca", 4] <- 1
colnames(vackind_x) <- c("Pfizzer", "Moderna", "Janssen", "Astrazeneca")



# data.frame(data[, "vaccine_1"], vactype_x)
# data.frame(data[, c("vaccine_1", "vaccine_2")], vacstat_x)

sex_x <- ifelse(sex == "Man", 1, 0)
age_x <- scale(age)
age_x2 <- rep(0, nrow = length(age))
# age_x2[(age > 0) & (age <= 17), 1] <- 1
age_x2[(age >= 18) & (age <= 29)] <- 1
age_x2[(age >= 30) & (age <= 39)] <- 2
age_x2[(age >= 40) & (age <= 49)] <- 3
age_x2[(age >= 50) & (age <= 64)] <- 4
age_x2[(age >= 65) & (age <= 74)] <- 5
age_x2[(age >= 75) & (age <= 84)] <- 6
age_x2[(age >= 85)] <- 7
# colnames(age_x2) <- c("a1", "a2", "a3", "a4", "a5", "a6")
# cbind(age, age_x2)
# table(age_x2)
# age_x <- age_x2



# all
date.exposure <- data.frame(d_infection_min, d_infection_max)
date.onset <- d_symptoms
date.hosp <- rep(NA, length(d_symptoms))
X <- data.frame(age = age_x, sex = sex_x, vaccine = vactype_x)
X2 <- data.frame(age = age_x, sex = sex_x, vaccine = vacstat_x)
X3 <- data.frame(age = age_x, sex = sex_x, vacstat = vacstat_x, vactype = vactype_x)
X4 <- data.frame(age = age_x, sex = sex_x, vacstat = vacstat_x, vactype = vactype_x, inter = vacstat_x * vactype_x)

# without Case 4
date.exposure <- date.exposure[wo_ind, ]
date.onset <- date.onset[wo_ind]
date.hosp <- date.hosp[wo_ind]
X <- X[wo_ind,]
X2 <- X2[wo_ind,]
X3 <- X3[wo_ind,]
X4 <- X4[wo_ind,]
vackind_x <- vackind_x[wo_ind, ]
age_x2 <- age_x2[wo_ind]


# summary table
t(rbind(table(age_x2), paste(round(prop.table(table(age_x2)), 2) * 100, "%", sep = "")))
t(rbind(table(sex_x), paste(round(prop.table(table(sex_x)), 2) * 100, "%", sep = "")))
t(rbind(table(X3[,3]), paste(round(prop.table(table(X3[,3])), 2) * 100, "%", sep = ""))) #status
t(rbind(table(data[,"vaccine_1"], useNA =  "always"), paste(round(prop.table(table(data[,"vaccine_1"], useNA =  "always")), 2) * 100, "%", sep = "")))

### Univariate analysis
library(IncPeriod)
IncPeriod.default

set_ini <- expand.grid(c(0.01, 0.001, 0.0001, 0), c(0.01, 0.001, 0.0001, 0), c(-0.01, -0.001, -0.0001, 0))
set_ini <- set_ini[(set_ini[,1] >= set_ini[,3]) & (set_ini[,2] >= set_ini[,3]),]

#age
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  ini.val0 <- set_ini[i, ]
  res_age <- IncPeriod(date.exposure, date.onset, date.hosp, X$age, ini.val = c(ini.val0, rep(0, 1)))
  set_lik[i] <- res_age$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res_age <- IncPeriod(date.exposure, date.onset, date.hosp, X$age, ini.val = c(ini.valf, rep(0, 1)))
summary(res_age)


## all
#sex
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  ini.val0 <- set_ini[i, ]
  res_sex <- IncPeriod(date.exposure, date.onset, date.hosp, X$sex, ini.val = c(ini.val0, rep(0, 1)))
  set_lik[i] <- res_sex$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res_sex <- IncPeriod(date.exposure, date.onset, date.hosp, X$sex, ini.val = c(ini.valf, rep(0, 1)))
d_sex <- X$sex
summary(res_sex)

#vactype
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  ini.val0 <- set_ini[i, ]
  res_vactype <- IncPeriod(date.exposure, date.onset, date.hosp, X[,-c(1:2)], ini.val = c(ini.val0, rep(0, 2))) 
  set_lik[i] <- res_vactype$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res_vactype <- IncPeriod(date.exposure, date.onset, date.hosp, X[,-c(1:2)], ini.val = c(ini.valf, rep(0, 2)))
Xv <- X[,-c(1:2)]
d_vactype <- rep(NA, nrow(Xv))
d_vactype[Xv[, 1] == 1] <- "mRNA"
d_vactype[Xv[, 2] == 1] <- "Adeno"
d_vactype[is.na(d_vactype)] <- "No"
# cbind(d_vactype, Xv)

summary(res_vactype)

#vacstat
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  ini.val0 <- set_ini[i, ]
  res_vacstat <- IncPeriod(date.exposure, date.onset, date.hosp, X2$vaccine, ini.val = c(ini.val0, rep(0, 1)))
  set_lik[i] <- res_vacstat$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res_vacstat <- IncPeriod(date.exposure, date.onset, date.hosp, X2$vaccine, ini.val = c(ini.valf, rep(0, 1)))
summary(res_vacstat)

d_status <- X2$vaccine

### Multivariate analysis with type and status
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  b_initial <- c(res_age$est[4], res_sex$est[4], res_vacstat$est[4], res_vactype$est[4:5])
  ini.val0 <- set_ini[i, ]
  res <- IncPeriod(date.exposure, date.onset, date.hosp, X3, ini.val = c(ini.val0, b_initial))
  set_lik[i] <- res$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res <- IncPeriod(date.exposure, date.onset, date.hosp, X3, ini.val = c(ini.valf, b_initial))
summary(res)


### Multivariate analysis with type
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  b_initial <- c(res_age$est[4], res_sex$est[4], res_vactype$est[4:5])
  ini.val0 <- set_ini[i, ]
  res <- IncPeriod(date.exposure, date.onset, date.hosp, X, ini.val = c(ini.val0, b_initial))
  set_lik[i] <- res$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res <- IncPeriod(date.exposure, date.onset, date.hosp, X, ini.val = c(ini.valf, b_initial))
summary(res)


### Multivariate analysis with status
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  b_initial <- c(res_age$est[4], res_sex$est[4], res_vacstat$est[4])
  ini.val0 <- set_ini[i, ]
  res <- IncPeriod(date.exposure, date.onset, date.hosp, X2, ini.val = c(ini.val0, b_initial))
  set_lik[i] <- res$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res <- IncPeriod(date.exposure, date.onset, date.hosp, X2, ini.val = c(ini.valf, b_initial))
summary(res)

########### TABLE 1
### Multivariate analysis with type and status only
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  b_initial <- c(res_vacstat$est[4], res_vactype$est[4:5])  
  ini.val0 <- set_ini[i, ]
  res <- IncPeriod(date.exposure, date.onset, date.hosp, X3[, -c(1:2)], ini.val = c(ini.val0, b_initial))
  set_lik[i] <- res$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res <- IncPeriod(date.exposure, date.onset, date.hosp, X3[, -c(1:2)], ini.val = c(ini.valf, b_initial))
summary(res)

########### TABLE 2
library(numDeriv)
p <- c(0.025, 0.25, 0.5, 0.75, 0.975)

### quantiles for sex
fun_incubation <- function(par, x1, p) {
  
  lambda <- exp(par[1])
  phi <- exp(par[2])
  rho <- exp(par[3])
  b <- par[-c(1:3)]
  xb <- sum(par[-c(1:3)] * x1)
  
  ((p^(-rho) - 1)/(rho * exp(xb)))^(1/phi) * lambda
  
}

xall <- expand.grid(sex = c(1,0))
p <- c(0.025, 0.25, 0.5, 0.75, 0.975)
result <-matrix(NA, nrow = nrow(xall), ncol = length(p))
resultp_sex <- matrix(NA, nrow = nrow(xall), ncol = 3)

est_ours <- res_sex$est
hess_ours <- res_sex$hess

for (i in 1:nrow(xall)) {
  for (pp in 1:length(p)) {
    pi <- 1 - p[pp]
    ours_incubation <- fun_incubation(est_ours, x1 = xall[i, 1], p = pi)
    ours_incubationse <- sqrt(as.numeric(matrix(grad(func = fun_incubation, x = est_ours, x1 = xall[i, 1], p = pi) , 1)
                                         %*%solve(hess_ours)%*%matrix(grad(func = fun_incubation, x = est_ours, x1 = xall[i, 1], p = pi) , ncol = 1)))
    ours_incubationci <- ours_incubation + c(-1, 1) * qnorm(0.975) * ours_incubationse
    
    result[i, pp] <- paste(round(ours_incubation,1), " (", paste(round(ours_incubationci,1), sep = ", ", collapse = ", "), ")", sep = "")
    if(pi == 0.5) resultp_sex[i,] <- c(median = ours_incubation, lower = ours_incubationci[1], ours_incubationci[2])
  }
  
}
res_incubation <- cbind(xall, result)
colnames(res_incubation) <- c("sex", p)
res_incubation

### quantiles for status
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  b_initial <- c(res_vacstat$est[4])
  ini.val0 <- set_ini[i, ]
  res <- IncPeriod(date.exposure, date.onset, date.hosp, X2[, -c(1:2)], ini.val = c(ini.val0, b_initial))
  set_lik[i] <- res$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res <- IncPeriod(date.exposure, date.onset, date.hosp, X2[, -c(1:2)], ini.val = c(ini.valf, b_initial))
summary(res)

fun_incubation <- function(par, x1, p) {
  
  lambda <- exp(par[1])
  phi <- exp(par[2])
  rho <- exp(par[3])
  b <- par[-c(1:3)]
  xb <- sum(par[-c(1:3)] * x1)
  
  ((p^(-rho) - 1)/(rho * exp(xb)))^(1/phi) * lambda
  
}

xall <- expand.grid(status = c(1,0))
result <-matrix(NA, nrow = nrow(xall), ncol = length(p))
resultp_status <- matrix(NA, nrow = nrow(xall), ncol = 3)

est_ours <- res$est
hess_ours <- res$hess
for (i in 1:nrow(xall)) {
  for (pp in 1:length(p)) {
    pi <- 1 - p[pp]
    ours_incubation <- fun_incubation(est_ours, x1 = xall[i, 1], p = pi)
    ours_incubationse <- sqrt(as.numeric(matrix(grad(func = fun_incubation, x = est_ours, x1 = xall[i, 1], p = pi) , 1)
                                         %*%solve(hess_ours)%*%matrix(grad(func = fun_incubation, x = est_ours, x1 = xall[i, 1], p = pi) , ncol = 1)))
    ours_incubationci <- ours_incubation + c(-1, 1) * qnorm(0.975) * ours_incubationse
    
    result[i, pp] <- paste(round(ours_incubation,1), " (", paste(round(ours_incubationci,1), sep = ", ", collapse = ", "), ")", sep = "")
    if(pi == 0.5) resultp_status[i,] <- c(median = ours_incubation, lower = ours_incubationci[1], ours_incubationci[2])
    
  }
  
}

res_incubation <- cbind(xall, result)
colnames(res_incubation) <- c("status", p)
res_incubation
resultp_status

### quantiles for vaccine
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  b_initial <- c(res_vactype$est[4:5])  
  ini.val0 <- set_ini[i, ]
  res <- IncPeriod(date.exposure, date.onset, date.hosp, X[, -c(1:2)], ini.val = c(ini.val0, b_initial))
  set_lik[i] <- res$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res <- IncPeriod(date.exposure, date.onset, date.hosp, X[, -c(1:2)], ini.val = c(ini.valf, b_initial))
summary(res)


### Medians
fun_incubation <- function(par, x1, x2, p) {
  
  lambda <- exp(par[1])
  phi <- exp(par[2])
  rho <- exp(par[3])
  b <- par[-c(1:3)]
  xb <- sum(par[-c(1:3)] * c(x1, x2))

  ((p^(-rho) - 1)/(rho * exp(xb)))^(1/phi) * lambda
  
}

xall <- expand.grid(mRNA = c(1,0), Adeno = c(1, 0))
result <-matrix(NA, nrow = nrow(xall), ncol = length(p))
resultp_type <- matrix(NA, nrow = nrow(xall), ncol = 3)

est_ours <- res$est
hess_ours <- res$hess
for (i in 1:nrow(xall)) {
  for (pp in 1:length(p)) {
    pi <- 1 - p[pp]
    ours_incubation <- fun_incubation(est_ours, x1 = xall[i, 1], x2 = xall[i, 2], p = pi)
    ours_incubationse <- sqrt(as.numeric(matrix(grad(func = fun_incubation, x = est_ours, x1 = xall[i, 1], x2 = xall[i, 2], p = pi) , 1)
                                         %*%solve(hess_ours)%*%matrix(grad(func = fun_incubation, x = est_ours, x1 = xall[i, 1], x2 = xall[i, 2], p = pi) , ncol = 1)))
    ours_incubationci <- ours_incubation + c(-1, 1) * qnorm(0.975) * ours_incubationse
    
    result[i, pp] <- paste(round(ours_incubation,1), " (", paste(round(ours_incubationci,1), sep = ", ", collapse = ", "), ")", sep = "")
    if(pi == 0.5) resultp_type[i,] <- c(median = ours_incubation, lower = ours_incubationci[1], ours_incubationci[2])
    
  }
  
}

res_incubation <- cbind(xall, result)
colnames(res_incubation) <- c("mRNA", "Adeno", p)


# storing medians for the forest plot

round(rbind(resultp_sex, resultp_status, resultp_type[c(3, 2, 4),]), 2)

# variable <- X3$vacstat
variable <- d_vactype
table(variable)
textp <- paste(table(variable), " (", paste(round(prop.table(table(variable)), 2) * 100, "%", sep = ""), ")", sep = "")
names(textp) <- names(table(variable))
data.frame(textp)



## Figure
# install.packages("forestplot")
library(forestplot)
dat <- read.csv("forestdata.csv", stringsAsFactors=FALSE)
subgps <- c(2,3,6,7,10, 11, 12)
dat$Variable[subgps] <- paste("              ",dat$ï..Variable[subgps]) 
dat$Variable[(1:nrow(dat))[-subgps]] <- dat$ï..Variable[(1:nrow(dat))[-subgps]]
tabletext <- cbind(c("Variable",dat$Variable), 
                   c("Number of \n patients (%)",dat$np), 
                   c("    Median incubation \n duration  (95% CI)",dat$Median_display))
dat
# tabletext <- tabletext[-2, ]


png("Forestplot.png",width=960, height=640)
forestplot(labeltext=tabletext, graph.pos=4,  grid = FALSE,
           mean=c(NA,dat$Median), 
           lower=c(NA,dat$lower), upper=c(NA,dat$upper),
           # title="Median Survivals",
           xlab="Days",
           # hrzl_lines=list("1" = gpar(lwd=100, col="#99999922", lineend="butt", columns = c(1:3))),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=1, cex=0.5, lineheight = "auto", boxsize=0.2, colgap=unit(7,"mm"),
           lwd.ci=1.5, ci.vertices=TRUE, ci.vertices.height = 0.2)
dev.off()




data <- read.csv("ForestPlotData.csv", stringsAsFactors=FALSE)

## Labels defining subgroups are a little indented!
subgps <- c(4,5,8,9,12,13,16,17,20,21,24,25,28,29,32,33)
data$Variable[subgps] <- paste("  ",data$ï..Variable[subgps]) 

## Combine the count and percent column
np <- ifelse(!is.na(data$Count), paste(data$Count," (",data$Percent,")",sep=""), NA)

## The rest of the columns in the table. 
tabletext <- cbind(c("Subgroup","\n",data$Variable), 
                   c("No. of Patients (%)","\n",np), 
                   c("4-Yr Cum. Event Rate\n PCI","\n",data$PCI.Group), 
                   c("4-Yr Cum. Event Rate\n Medical Therapy","\n",data$Medical.Therapy.Group), 
                   c("P Value","\n",data$P.Value))

# library(forestplot)
# png(file.path(workdir,"Figures\\Forestplot.png"),width=960, height=640)
forestplot(labeltext=tabletext, graph.pos=3, 
           mean=c(NA,NA,data$Point.Estimate), 
           lower=c(NA,NA,data$Low), upper=c(NA,NA,data$High),
           title="Hazard Ratio",
           xlab="     <---PCI Better---    ---Medical Therapy Better--->",
           hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                           "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "15" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                           "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")),
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.5, colgap=unit(6,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.4)
# dev.off()



### Multivariate analysis with status only
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  b_initial <- c(res_vacstat$est[4])
  ini.val0 <- set_ini[i, ]
  res <- IncPeriod(date.exposure, date.onset, date.hosp, X2[, -c(1:2)], ini.val = c(ini.val0, b_initial))
  set_lik[i] <- res$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res <- IncPeriod(date.exposure, date.onset, date.hosp, X2[, -c(1:2)], ini.val = c(ini.valf, b_initial))
summary(res)


### Medians
fun_incubation <- function(par, x1, p) {
  
  lambda <- exp(par[1])
  phi <- exp(par[2])
  rho <- exp(par[3])
  b <- par[-c(1:3)]
  xb <- sum(par[-c(1:3)] * x1)
  
  ((p^(-rho) - 1)/(rho * exp(xb)))^(1/phi) * lambda
  
}

library(numDeriv)
# Incubation period (quantiles)
xall <- expand.grid(status = c(1,0))
#p <- c(0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99, 0.999)
p <- c(0.025, 0.25, 0.5, 0.75, 0.975)
result <-matrix(NA, nrow = nrow(xall), ncol = length(p))

est_ours <- res$est
hess_ours <- res$hess
for (i in 1:nrow(xall)) {
  for (pp in 1:length(p)) {
    pi <- 1 - p[pp]
    ours_incubation <- fun_incubation(est_ours, x1 = xall[i, 1], p = pi)
    ours_incubationse <- sqrt(as.numeric(matrix(grad(func = fun_incubation, x = est_ours, x1 = xall[i, 1], p = pi) , 1)
                                         %*%solve(hess_ours)%*%matrix(grad(func = fun_incubation, x = est_ours, x1 = xall[i, 1], p = pi) , ncol = 1)))
    ours_incubationci <- ours_incubation + c(-1, 1) * qnorm(0.975) * ours_incubationse
    
    result[i, pp] <- paste(round(ours_incubation,2), " (", paste(round(ours_incubationci,2), sep = ", ", collapse = ", "), ")", sep = "")
    
  }
  
}

res_incubation <- cbind(xall, result)

colnames(res_incubation) <- c("status", p)
res_incubation


### Multivariate analysis with kind only
set_lik <- c()
for (i in 1:nrow(set_ini)) {
  ini.val0 <- set_ini[i, ]
  res <- IncPeriod(date.exposure, date.onset, date.hosp, vackind_x, ini.val = c(ini.val0, rep(0, 4)))
  set_lik[i] <- res$fit$value
  print(i)
}
ini.valf <- set_ini[which.min(set_lik),]
res <- IncPeriod(date.exposure, date.onset, date.hosp, vackind_x, ini.val = c(ini.valf, rep(0, 4)))
summary(res)


fun_incubation <- function(par, xalli, p) {
  
  lambda <- exp(par[1])
  phi <- exp(par[2])
  rho <- exp(par[3])
  b <- par[-c(1:3)]
  xb <- sum(par[-c(1:3)] * xalli)
  
  ((p^(-rho) - 1)/(rho * exp(xb)))^(1/phi) * lambda
  
}

library(numDeriv)
# Incubation period (quantiles)
xall <- rbind(diag(4), rep(0, 4))
#p <- c(0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99, 0.999)
p <- c(0.025, 0.25, 0.5, 0.75, 0.975)
result <-matrix(NA, nrow = nrow(xall), ncol = length(p))

est_ours <- res$est
hess_ours <- res$hess
for (i in 1:nrow(xall)) {
  xalli = xall[i, ]
  for (pp in 1:length(p)) {
    pi <- 1 - p[pp]
    ours_incubation <- fun_incubation(est_ours, xalli = xall[i, ], p = pi)
    ours_incubationse <- sqrt(as.numeric(matrix(grad(func = fun_incubation, x = est_ours, xalli = xall[i, ], p = pi) , 1)
                                         %*%solve(hess_ours)%*%matrix(grad(func = fun_incubation, x = est_ours, xalli = xall[i, ], p = pi) , ncol = 1)))
    ours_incubationci <- ours_incubation + c(-1, 1) * qnorm(0.975) * ours_incubationse
    
    result[i, pp] <- paste(round(ours_incubation,2), " (", paste(round(ours_incubationci,2), sep = ", ", collapse = ", "), ")", sep = "")
    
  }
  
}
res_incubation <- cbind(xall, result)

colnames(res_incubation) <- c(colnames(vackind_x), p)
res_incubation

# 
# ### Multivariate analysis with interaction
# #all
# X2 <- data.frame(X, X$age*X$sex, X$age*X$vaccine, X$sex*X$vaccine)
# b_initial <- c(res_age$est[4], res_sex$est[4], res_vac$est[4], 0, 0, 0)
# res <- IncPeriod(date.exposure, date.onset, date.hosp, X2, ini.val = c(0.01, 0.01, -0.001, b_initial))
# summary(res)
# res$fit$value
# 
# # excluding Case 4
# X2_wo <- data.frame(X_wo, X_wo$age*X_wo$sex, X_wo$age*X_wo$vaccine, X_wo$sex*X_wo$vaccine)
# b_initial_wo <- c(res_age_wo$est[4], res_sex_wo$est[4], res_vac_wo$est[4], 0, 0, 0)
# res <- IncPeriod(date.exposure_wo, date.onset_wo, date.hosp_wo, X2_wo, ini.val = c(0.01, 0.01, -0.001, b_initial_wo))
# summary(res)
# res$fit$value
# 
 





##






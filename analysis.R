# Logistic funciton
exp_fnc <- function(x, alpha) {
  alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3]))
}

exp_fnc_ss <- function(par) {
  y2 <- par[1]/(1 + exp(-(x - par[2])/par[3]))
  return(sum((y2-y)^2))
}

# Load NYT data
data <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", as.is=TRUE)
data <- data[data$state=="Hawaii",] # Could do other states
data[nrow(data),] # NYT a day behind, see optional component below

# --------- Optional: add in current day for Hawaii --------- #
data <- rbind(data, data.frame(date="2020-08-16", state="Hawaii", fips=NA, cases=data$cases[nrow(data)] + 220, deaths=NA))
# ----------------------------------------------------------- #

data$day <- 1:nrow(data)
lock <- "2020-04-30" # lockdown lifted
phase1 <- data[1:which(data$date==lock),]
# phase1_mod <- nls(cases ~ SSlogis(day, phi1, phi2, phi3), phase1)
x <- phase1$day
y <- phase1$cases
phase1_mod2 <- optim(par=c(500, 10, 5), exp_fnc_ss)
phase1_mod2$par
# phase1_pars <- coef(phase1_mod)  #extracting coefficients
phase1_pars <- phase1_mod2$par  #extracting coefficients

for (i in 30:0) {
  dat <- data[1:(nrow(data)-i),]
  phase2 <- dat[which(dat$date==lock):nrow(dat),]
  phase2$day_adj <- 1:nrow(phase2)
  phase2$cases_adj <- phase2$cases - phase1$cases[nrow(phase1)]
  
  png(paste0("figs/nyt_", dat$date[nrow(dat)], ".png"), width = 5, height = 4, units = 'in', res = 150)
  
  plot(cases ~ day, phase1, main = "Hawai'i: logistic growth models", xlab = "Days since March 7", ylab = "Positive cases", xlim = c(1, 250), ylim = c(0, 15000), pch=1, col=rgb(0,0,0,0.3), type="p")  # Census data
  lines(dat$day, exp_fnc(dat$day, phase1_pars), col="black")
  
  # phase2_mod <- nls(cases_adj ~ SSlogis(day_adj, phi1, phi2, phi3), phase2, start=list(phi1=1300, phi2=2, phi3=0.5))

  x <- phase2$day_adj
  y <- phase2$cases_adj
  phase2_mod2 <- optim(par=c(1000, 250, 10), exp_fnc_ss)
  # phase2_mod2$par
  
  # phase2_pars <- coef(phase2_mod)  #extracting coefficients
  phase2_pars <- phase2_mod2$par
  ss <- phase2$day[1]:(phase2$day[length(phase2$day)] + 14)
  points(cases ~ day, phase2, pch=1, col=rgb(0,0,1,0.3))
  lines(ss, exp_fnc(1:length(ss), phase2_pars) + phase1$cases[nrow(phase1)], col="red")

  abline(v=phase2$day[length(phase2$day)], lty=2)
  text(240, 0, "Data: NYT", cex=0.5)
  text(phase2$day[length(phase2$day)], 50, data$date[nrow(dat)], cex=0.7)
  
  legend("topleft", legend=c("Phase 1 data", "Phase 1 model shows lock down worked", "Phase 2 data (lockdown lifted)", "Phase 2 model + stay-the-course projection"), col=c("black", "black", "blue", "red"), pch=c(1, NA, 1, NA), lty=c(NA, 1, NA, 1), bty="n", cex=0.6)
  
  dev.off()
}

# Scenarios

png(paste0("figs/nyt_2020-08-99.png"), width = 5, height = 4, units = 'in', res = 150)

plot(cases ~ day, phase1, main = "Hawai'i: logistic growth models", xlab = "Days since March 7", ylab = "Positive cases", xlim = c(1, 250), ylim = c(0, 15000), pch=1, col=rgb(0,0,0,0.3), type="p")  # Census data
lines(dat$day, exp_fnc(dat$day, phase1_pars), col="black")

# phase2_mod <- nls(cases_adj ~ SSlogis(day_adj, phi1, phi2, phi3), phase2, start=list(phi1=1300, phi2=2, phi3=0.5))

x <- phase2$day_adj
y <- phase2$cases_adj
phase2_mod2 <- optim(par=c(1000, 250, 10), exp_fnc_ss)
# phase2_mod2$par

# phase2_pars <- coef(phase2_mod)  #extracting coefficients
phase2_pars <- phase2_mod2$par
ss <- phase2$day[1]:(phase2$day[length(phase2$day)] + 60)
points(cases ~ day, phase2, pch=1, col=rgb(0,0,1,0.3))
lines(ss, exp_fnc(1:length(ss), phase2_pars) + phase1$cases[nrow(phase1)], col="red")

text(ss[length(ss)] - 20, 15000, paste0("Status quo"), cex=0.7)

abline(v=phase2$day[length(phase2$day)], lty=2)
text(240, 0, "Data: NYT", cex=0.5)
text(phase2$day[length(phase2$day)], 50, data$date[nrow(dat)], cex=0.7)

# legend("topleft", legend=c("Phase 1 data", "Phase 1 model shows lock down worked", "Phase 2 data (lockdown lifted)", "Phase 2 model + stay-the-course projection"), col=c("black", "black", "blue", "red"), pch=c(1, NA, 1, NA), lty=c(NA, 1, NA, 1), bty="n", cex=0.7)


x <- 1:(length(phase2$day_adj)+60)
y <- phase2$cases_adj

# 2 weeks
y1 <- seq(220, by=-220/14, length=60)
y1[y1 < 0] <- 0
y <- c(phase2$cases_adj, phase2$cases_adj[length(phase2$cases_adj)] + cumsum(y1))

phase2_mod2 <- optim(par=c(1000, 250, 10), exp_fnc_ss)
phase2_pars <- phase2_mod2$par
ss <- phase2$day[1]:(phase2$day[length(phase2$day)] + 60)
pp <- exp_fnc(1:length(ss), phase2_pars) + phase1$cases[nrow(phase1)]
lines(ss, pp, col="red", lty=2)

text(ss[length(ss)], pp[length(pp)]+200, paste0("2 weeks (", round(pp[length(pp)], 0), ")"), cex=0.7)

# 1 month
y1 <- seq(220, by=-220/30, length=60)
y1[y1 < 0] <- 0
y <- c(phase2$cases_adj, phase2$cases_adj[length(phase2$cases_adj)] + cumsum(y1))

phase2_mod2 <- optim(par=c(2000, 250, 10), exp_fnc_ss)
phase2_pars <- phase2_mod2$par
ss <- phase2$day[1]:(phase2$day[length(phase2$day)] + 60)
pp <- exp_fnc(1:length(ss), phase2_pars) + phase1$cases[nrow(phase1)]
lines(ss, pp, col="red", lty=2)

text(ss[length(ss)], pp[length(pp)]+200, paste0("1 month (", round(pp[length(pp)], 0), ")"), cex=0.7)

# 2 months
y1 <- seq(220, by=-220/60, length=60)
y1[y1 < 0] <- 0
y <- c(phase2$cases_adj, phase2$cases_adj[length(phase2$cases_adj)] + cumsum(y1))

phase2_mod2 <- optim(par=c(5000, 250, 10), exp_fnc_ss)
phase2_pars <- phase2_mod2$par
ss <- phase2$day[1]:(phase2$day[length(phase2$day)] + 60)
pp <- exp_fnc(1:length(ss), phase2_pars) + phase1$cases[nrow(phase1)]
lines(ss, pp, col="red", lty=2)

text(ss[length(ss)], pp[length(pp)]+200, paste0("2 months (", round(pp[length(pp)], 0), ")"), cex=0.7)

legend("topleft", legend=c("Phase 1 data", "Phase 1 model shows lock down worked", "Phase 2 data (lockdown lifted)", "Phase 2 model + stay-the-course projection"), col=c("black", "black", "blue", "red"), pch=c(1, NA, 1, NA), lty=c(NA, 1, NA, 1), bty="n", cex=0.6)

dev.off()

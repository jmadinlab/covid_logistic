# Logistic funciton
exp_fnc <- function(x, alpha) {
  alpha[1]/(1 + exp(-(x - alpha[2])/alpha[3]))
}

# Load NYT data
data <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", as.is=TRUE)
data <- data[data$state=="Hawaii",] # Could do other states
data$date[nrow(data)] # NYT a day behind, see optional component below

# --------- Optional: add in current day for Hawaii --------- #
data <- rbind(data, data.frame(date="2020-07-28", state="Hawaii", fips=NA, cases=data$cases[nrow(data)] + 47, deaths=NA))
# ----------------------------------------------------------- #

data$day <- 1:nrow(data)
lock <- "2020-04-30" # lockdown lifted
phase1 <- data[1:which(data$date==lock),]
phase1_mod <- nls(cases ~ SSlogis(day, phi1, phi2, phi3), phase1)
summary(phase1_mod)
phase1_pars <- coef(phase1_mod)  #extracting coefficients

for (i in 0:25) {
  dat <- data[1:(nrow(data)-i),]
  phase2 <- dat[which(dat$date==lock):nrow(dat),]
  phase2$day_adj <- 1:nrow(phase2)
  phase2$cases_adj <- phase2$cases - phase1$cases[nrow(phase1)]
  
  png(paste0("figs/nyt_", dat$date[nrow(dat)], ".png"), width = 4, height = 4, units = 'in', res = 300)
  
  plot(cases ~ day, phase1, main = "Hawai'i: logistic growth models", xlab = "Days since March 7", ylab = "Positive cases", xlim = c(1, 250), ylim = c(0, 3000), pch=1, col=rgb(0,0,0,0.3), type="p")  # Census data
  lines(dat$day, exp_fnc(dat$day, phase1_pars), col="black")
  phase2_mod <- nls(cases_adj ~ SSlogis(day_adj, phi1, phi2, phi3), phase2)
  summary(phase2_mod)
  phase2_pars <- coef(phase2_mod)  #extracting coefficients
  ss <- phase2$day[1]:(phase2$day[length(phase2$day)] + 30)
  points(cases ~ day, phase2, pch=1, col=rgb(1,0,0,0.3))
  lines(ss, exp_fnc(1:length(ss), phase2_pars) + phase1$cases[nrow(phase1)], col="red")
  lines(ss, exp_fnc(1:length(ss), confint(phase2_mod)[,1]) + phase1$cases[nrow(phase1)], col="red", lty=3)
  lines(ss, exp_fnc(1:length(ss), confint(phase2_mod)[,2]) + phase1$cases[nrow(phase1)], col="red", lty=3)
  
  abline(v=phase2$day[length(phase2$day)], lty=2)
  text(240, 0, "Data: NYT", cex=0.5)
  text(phase2$day[length(phase2$day)] + 50, 1500, "Projection", col="red")
  text(phase2$day[length(phase2$day)], 50, paste0("Today (", data$date[nrow(dat)], ")"), cex=0.7)
  
  legend("topleft", legend=c("Phase 1", "Phase 2 (lockdown lifted)"), col=c("black", "red"), lty=1, bty="n", cex=0.7)
  
  dev.off()
}

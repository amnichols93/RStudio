MechaCar_mpg <- read.csv('MechaCar_mpg.csv') #import dataset
Suspension_Coil <- read.csv('Suspension_Coil.csv') #import dataset

mecha_matrix <- as.matrix(MechaCar_mpg[,c("vehicle.length","vehicle.weight","spoiler.angle","ground.clearance","AWD","mpg")]) #convert data frame into numeric matrix
cor(mecha_matrix)

summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_mpg))
summary(lm(mpg ~ vehicle.length + ground.clearance,data=MechaCar_mpg))
summary(lm(mpg ~ vehicle.length,data=MechaCar_mpg))

PSIsummary <- data.frame(Num_of_Vehicles=nrow(Suspension_Coil), PSI_mean=mean(Suspension_Coil$PSI), PSI_median=median(Suspension_Coil$PSI), PSI_variance=var(Suspension_Coil$PSI), PSI_sd=sd(Suspension_Coil$PSI))

shapiro.test(Suspension_Coil$PSI)
t.test(Suspension_Coil$PSI,mu=1500)
WaPo_binom_test <- function(x, n, alpha)
{
	if ((n > 0) && (n >= x))
	{
		result = binom.test(x, n, conf.level = (1-alpha))$conf.int
	}
	else
	{
		print("Warning: criteria for binom.test not satisfied")
		result = c(0.0, 0.0)
	}
	return(result)
}

# Save plots to pdf
pdf("WaPo.pdf")

# read in WaPo data
# Google file ID
id <- "1GKV93hkun6syJNMZMHU_7FTNpHaQbHjn"

# Read in data
data <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

# See data summary
str(data)
head(data)

# Display disposition levels
disp <- levels(as.factor(data$disposition))

# Display race levels
race <- levels(as.factor(data$victim_race))

#######################################################
# Race vs disposition (all cities)
#######################################################

# Create two way table race vs disposition
race_v_disp <- with(data, table(victim_race, disposition))
race_v_disp

# proportions by race
prop.table(race_v_disp, 1)

# Get overall percent of unsolved homicides
obs <- addmargins(race_v_disp)
d <- dim(obs)
max_row <- d[1]
max_col <- d[2]
obs[max_row,]/obs[max_row,max_col]

# Chi-Square test for independence (Method 1)
summary(race_v_disp)

# Chi-Square test for independence (Method 2)
chisq.test(race_v_disp)

# Chi-Square test for independence (Manual Method)
# Create empty expected table
exp <- as.table(matrix(0, nrow = length(race), ncol = length(disp), dimnames = c(list(race), list(disp))))

# Calculate expected values
for (r in race)
{
	for (c in disp)
	{
		exp[r,c] <- (obs[max_row, c] * obs[r, max_col]) / obs[max_row, max_col]	
	}
}

# Calculate TS
TS = 0
for (r in race)
{
	for (c in disp)
	{
		TS = TS + (obs[r,c] - exp[r,c])^2 / exp[r,c]
	}
}

dof = (length(race) - 1) * (length(disp) - 1)
pchisq(TS, dof, lower.tail = FALSE)

# Calculate 95% Simultaneous CI
dims = dimnames(race_v_disp)
alpha = 0.05 / (length(dims[["victim_race"]]))  #Bonferroni Correction
p_hat = matrix(race_v_disp[,3]/rowSums(race_v_disp))

# Method 1 - Normal Approximation
rep(p_hat, 2) + sqrt(p_hat * (1 - p_hat) / rowSums(race_v_disp)) %*% c(qnorm(alpha/2), -qnorm(alpha/2))

# Method 2 - binom.test
boxplot(mapply(WaPo_binom_test, race_v_disp[,3], rowSums(race_v_disp), alpha), ylim=c(0,1))
title("All cities - Open homicides by victim race")

#######################################################
# Race vs disposition vs city
#######################################################
# Create three way table race vs disposition vs city
race_disp_city <- with(data, table(victim_race, disposition, city))
race_disp_city

# proportion by city
prop.table(race_disp_city, 3)

dims = dimnames(race_disp_city)

alpha = 0.05 / (length(dims[["victim_race"]]))  #Bonferroni Correction
for ( i in dims[["city"]] )
{
	open = race_disp_city[,3,i]
	total = rowSums(race_disp_city[,,i])
	result = mapply(WaPo_binom_test, open, total, alpha)
	boxplot(result, ylim=c(0,1))
	title(paste(i," - Open homicides by victim race"))
}


homicides_by_city = addmargins(race_disp_city)[,4,]

# Increase bottom margin due to vertical orientation of xlabels
par(mar=c(7, 4, 4, 2))
barplot(homicides_by_city[1:6,1:50], las=2)
homicide_mean = mean(homicides_by_city[7,1:50])
homicide_sd = sd(homicides_by_city[7,1:50])
abline(h = homicide_mean, lty=2)
abline(h = homicide_mean + homicide_sd, lty=3)
abline(h = homicide_mean - homicide_sd, lty=3)
title("Total Homicides by City")

dev.off()
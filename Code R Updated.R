# Sets the path to the parent directory of RR classes
setwd("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git")

# Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data = read.csv("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\onet_tasks.csv")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
library(readxl) 

isco1 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO1")
isco2 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO2")
isco3 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO3")
isco4 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO4")
isco5 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO5")
isco6 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO6")
isco7 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO7")
isco8 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO8")
isco9 <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet="ISCO9")

#UPDATED

sheets <- c("ISCO1", "ISCO2", "ISCO3", "ISCO4", "ISCO5", "ISCO6", "ISCO7", "ISCO8", "ISCO9")
isco <- list()

for (i in 1:length(sheets)) {
  isco[[i]] <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet = sheets[i])
}

sheets <- c("ISCO1", "ISCO2", "ISCO3", "ISCO4", "ISCO5", "ISCO6", "ISCO7", "ISCO8", "ISCO9")
isco <- list()

for (i in 1:length(sheets)) {
  isco[[i]] <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet = sheets[i])
  isco[[i]]$ISCO <- i
}
# We will focus on three countries, but perhaps we could clean this code to allow it
# to easily run for all the countries in the sample?
# This will calculate worker totals in each of the chosen countries.

countries <- c("Belgium", "Spain", "Poland", "Czechia", "Denmark", "Italy", "Lithuania", "Finland", "Sweden")
total <- vector(mode = "numeric", length = length(countries))

for (i in 1:length(countries)) {
  country_total <- 0
  for (j in 1:9) {
    country_total <- country_total + get(paste0("isco", j, "$", countries[i]))
  }
  total[i] <- country_total
}

# Let's merge all these datasets. We'll need a column that stores the occupation categories:

sheets <- c("ISCO1", "ISCO2", "ISCO3", "ISCO4", "ISCO5", "ISCO6", "ISCO7", "ISCO8", "ISCO9")
isco <- list()

for (i in 1:length(sheets)) {
  isco[[i]] <- read_excel("C:\\Users\\sever\\OneDrive\\Masaüstü\\Git\\Eurostat_employment_isco.xlsx", sheet = sheets[i])
  isco[[i]]$ISCO <- i
}

# and this gives us one large file with employment in all occupations.
all_data <- rbind(isco1, isco2, isco3, isco4, isco5, isco6, isco7, isco8, isco9)

# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals
countries <- c("Belgium", "Spain", "Poland", "Czechia", "Denmark", "Italy", "Lithuania", "Finland", "Sweden")
for (country in countries) {
  for (i in 1:9) {
    all_data[[paste0("total_", country)]][i:((i-1)*9+i)] <- rep(get(paste0("total_", country)), each = 9)
  }
}

# And this will give us shares of each occupation among all workers in a period-country

all_data$share_Belgium = all_data$Belgium/all_data$total_Belgium
all_data$share_Spain = all_data$Spain/all_data$total_Spain
all_data$share_Poland = all_data$Poland/all_data$total_Poland
all_data$share_Czechia = all_data$Czechia/all_data$total_Czechia
all_data$share_Denmark = all_data$Denmark/all_data$total_Denmark
all_data$share_Italy = all_data$Italy/all_data$total_Italy
all_data$share_Lithuania = all_data$Lithuania/all_data$total_Lithuania
all_data$share_Finland = all_data$Finland/all_data$total_Finland
all_data$share_Sweden = all_data$Sweden/all_data$total_Sweden

# Now let's look at the task data. We want the first digit of the ISCO variable only
library(stringr)

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)

aggdata <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

#These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

#Let's combine the data.
library(dplyr)

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:

#install.packages("Hmisc")
library(Hmisc)

# first task item

countries <- c("Belgium", "Poland", "Spain", "Czechia", "Denmark", "Italy", "Lithuania", "Finland", "Sweden")
variable <- "t_4A2a4"

for (country in countries) {
  temp_mean <- wtd.mean(combined[[variable]], combined[[paste0("share_", country)]])
  temp_sd <- wtd.var(combined[[variable]], combined[[paste0("share_", country)]]) %>% sqrt()
  combined[[paste0("std_", country, "_", variable)]] <- (combined[[variable]]-temp_mean)/temp_sd
}

# second task item

countries <- c("Belgium", "Poland", "Spain", "Czechia", "Denmark", "Italy", "Lithuania", "Finland", "Sweden")
questions <- c("t_4A2a4", "t_4A2b2")

for (country in countries) {
  for (question in questions) {
    col_name <- paste("std", country, question, sep = "_")
    temp_mean <- wtd.mean(combined[[question]], combined[[paste("share", country, sep = "_")]])
    temp_sd <- wtd.var(combined[[question]], combined[[paste("share", country, sep = "_")]]) %>% sqrt()
    combined[[col_name]] <- (combined[[question]] - temp_mean) / temp_sd
  }
}

# third task item

countries <- c("Belgium", "Poland", "Spain", "Czechia", "Denmark", "Italy", "Lithuania", "Finland", "Sweden")
variables <- c("t_4A4a1", "t_4A4a2", "t_4A4a3")

for (country in countries) {
  for (variable in variables) {
    temp_mean <- wtd.mean(combined[[variable]], combined[[paste0("share_", country)]])
    temp_sd <- wtd.var(combined[[variable]], combined[[paste0("share_", country)]]) %>% sqrt()
    combined[[paste0("std_", country, "_", variable)]] <- (combined[[variable]] - temp_mean)/temp_sd
  }
}

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:

country_list <- c("Belgium", "Poland", "Spain", "Czechia", "Denmark", "Italy", "Lithuania", "Finland", "Sweden")

for (country in country_list) {
  combined[[paste0(country, "_NRCA")]] <- combined[[paste0("std_", country, "_t_4A2a4")]] +
    combined[[paste0("std_", country, "_t_4A2b2")]] +
    combined[[paste0("std_", country, "_t_4A4a1")]]
}

# And we standardise NRCA in a similar way.

countries <- c("Belgium", "Poland", "Spain", "Czechia", "Denmark", "Italy", "Lithuania", "Finland", "Sweden")
for (country in countries) {
  temp_mean <- wtd.mean(combined[[paste0("std_", country, "_t_4A2a4")]], combined[[paste0("share_", country)]])
  temp_sd <- wtd.var(combined[[paste0("std_", country, "_t_4A2a4")]], combined[[paste0("share_", country)]]) %>% sqrt()
  combined[[paste0("std_", country, "_t_4A2a4")]] <- (combined[[paste0("t_4A2a4_", country)]] - temp_mean) / temp_sd
  
  temp_mean <- wtd.mean(combined[[paste0("std_", country, "_t_4A2b2")]], combined[[paste0("share_", country)]])
  temp_sd <- wtd.var(combined[[paste0("std_", country, "_t_4A2b2")]], combined[[paste0("share_", country)]]) %>% sqrt()
  combined[[paste0("std_", country, "_t_4A2b2")]] <- (combined[[paste0("t_4A2b2_", country)]] - temp_mean) / temp_sd
  
  temp_mean <- wtd.mean(combined[[paste0("std_", country, "_t_4A4a1")]], combined[[paste0("share_", country)]])
  temp_sd <- wtd.var(combined[[paste0("std_", country, "_t_4A4a1")]], combined[[paste0("share_", country)]]) %>% sqrt()
  combined[[paste0("std_", country, "_t_4A4a1")]] <- (combined[[paste0("t_4A4a1_", country)]] - temp_mean) / temp_sd
  
  combined[[paste0(country, "_NRCA")]] <- combined[[paste0("std_", country, "_t_4A2a4")]] + combined[[paste0("std_", country, "_t_4A2b2")]] + combined[[paste0("std_", country, "_t_4A4a1")]]
  
  temp_mean <- wtd.mean(combined[[paste0(country, "_NRCA")]], combined[[paste0("share_", country)]])
  temp_sd <- wtd.var(combined[[paste0(country, "_NRCA")]], combined[[paste0("share_", country)]]) %>% sqrt()
  combined[[paste0("std_", country, "_NRCA")]] <- (combined[[paste0(country, "_NRCA")]] - temp_mean) / temp_sd
}

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.

combined$multip_Spain_NRCA <- (combined$std_Spain_NRCA*combined$share_Spain)
combined$multip_Belgium_NRCA <- (combined$std_Belgium_NRCA*combined$share_Belgium)
combined$multip_Poland_NRCA <- (combined$std_Poland_NRCA*combined$share_Poland)
combined$multip_Czechia_NRCA <- (combined$std_Czechia_NRCA*combined$share_Czechia)
combined$multip_Denmark_NRCA <- (combined$std_Denmark_NRCA*combined$share_Denmark)
combined$multip_Italy_NRCA <- (combined$std_Italy_NRCA*combined$share_Italy)
combined$multip_Lithuania_NRCA <- (combined$std_Lithuania_NRCA*combined$share_Lithuania)
combined$multip_Finland_NRCA <- (combined$std_Finland_NRCA*combined$share_Finland)
combined$multip_Sweden_NRCA <- (combined$std_Sweden_NRCA*combined$share_Sweden)

# Step 2: sum it up (it basically becomes another weighted mean)

plot_country_data <- function(country_name, data) {
  country_cols <- grep(paste0("^std_", country_name), names(data), value = TRUE)
  nrca_col <- paste0(country_name, "_NRCA")
  share_col <- paste0("share_", country_name)
  temp_mean <- wtd.mean(data[, nrca_col], data[, share_col])
  temp_sd <- wtd.var(data[, nrca_col], data[, share_col]) %>% sqrt()
  data[paste0("std_", country_name, "_NRCA")] <- (data[, country_cols] - temp_mean) / temp_sd
  data[paste0("multip_", country_name, "_NRCA")] <- data[, paste0("std_", country_name, "_NRCA")] * data[, share_col]
  agg_data <- aggregate(data[, paste0("multip_", country_name, "_NRCA")], by=list(data$TIME), FUN=sum, na.rm=TRUE)
  plot(agg_data$x, xaxt="n")
  axis(1, at=seq(1, nrow(agg_data), 3), labels=agg_data$Group.1[seq(1, nrow(agg_data), 3)])
  title(main = paste0("Aggregated ", country_name, " data"))
}



# We can plot it now!

countries <- c("Poland", "Spain", "Belgium", "Czechia", "Denmark", "Italy", "Lithuania", "Finland", "Sweden")
aggregates <- list(agg_Poland, agg_Spain, agg_Belgium)

for(i in seq_along(countries)) {
  plot(aggregates[[i]]$x, xaxt="n")
  axis(1, at=seq(1, 40, 3), labels=aggregates[[i]]$Group.1[seq(1, 40, 3)])
}

# If this code gets automated and cleaned properly,
#  you should be able to easily add other countries as well as other tasks.
# E.g.:

# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment









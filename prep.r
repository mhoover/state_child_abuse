# script preps raw data from websites to create cloropleths by state 
# of child abuse incidence

# set up directories
dir <- getwd()
if(!dir.exists(file.path(dir, 'data'))) {
    dir.create(file.path(dir, 'data'))
}
if(!dir.exists(file.path(dir, 'output'))) {
    dir.create(file.path(dir, 'output'))
}

# libraries
library(devtools)
library(readxl)
library(maps)
library(ggplot2)
library(reshape2)
if(!('FormatFunctions' %in% rownames(installed.packages()))) {
    install_github('mhoover/state_child_abuse/FormatFunctions')
}
library(FormatFunctions)

# get data
pops <- read.csv(url('https://www.census.gov/popest/data/national/totals/2014/files/NST-EST2014-popchg2010_2014.csv'), 
                 header = TRUE, sep = ",", stringsAsFactors = FALSE)

abuse_url <- get_excel_url('http://www.acf.hhs.gov/sites/default/files/cb/cm2012_table3_4.xlsx')
d <- read_excel(abuse_url, skip = 2)

perps_url <- get_excel_url('http://www.acf.hhs.gov/sites/default/files/cb/cm2012_table5_1.xlsx')
perps <- read_excel(perps_url, skip = 1)

census <- read.csv(paste(dir, "/data/census_regions.csv", sep = ''), 
                   header = TRUE, sep = ",", stringsAsFactors = FALSE)

# load map data
states <- map_data("state")

# data cleaning
d <- d[-53:-55, -which(names(d) == '')]
names(d) <- c('state', paste0('total', 2008:2012), paste0('rate', 2008:2012))
d$state <- gsub("\\s+$", "", tolower(d$state))
d[, 2:6] <- apply(d[, 2:6], 2, function(x) {as.numeric(gsub(",", "", x))})
d$rate2012[d$state == "idaho"] <- apply(d[d$state == "idaho", 2:5], 1, 
    mean) / mean(as.numeric(d[d$state == "idaho", 2:5] / d[d$state == "idaho", 
    7:10]))

perps <- perps[-53:-54, ]
names(perps) <- c('state', 'perps')
perps$state <- gsub("\\s+$", "", tolower(d$state))
perps$perps <- as.numeric(gsub(",", "", perps$perps))

pops <- pops[6:57, grep('(?i)name|popestimate', names(pops))]
names(pops) <- c('state', paste0('pop', 2010:2014))
pops$state <- tolower(pops$state)
pops[, 2:6] <- apply(pops[, 2:6], 2, function(x) {as.numeric(gsub(",", "", x))})

# merge in census region classifications
d <- merge(d, census, by = "state", all.x = TRUE)
d$census <- factor(d$census, levels = c("south", "northeast", "midwest", 
	"west"))

# create population perpetrator percentages
perps <- merge(perps, pops[, c(1, 4)], by = "state")
perps$pct.perps <- perps$perps / perps$pop2012

# merge plotting data with map data
states <- merge(states, d[, c(1, 7:11)], by.x = "region", by.y = "state")
states <- merge(states, perps[, c(1, 4)], by.x = "region", by.y = "state")

# melt data for plotting in ggplot2
states <- melt(states, id.var = c("region", "long", "lat", "group"), 
	measure.var = 7:12)
states$variable <- factor(states$variable, labels = c(2008:2012, "perps"))

# figure 1: child abuse by state, 2008-2012
fig1 <- ggplot(data = subset(states, variable != "perps"), aes(x = long, 
	y = lat, group = group, fill = value)) + 
	geom_polygon() + 
	scale_fill_continuous(name = "Incidence (per 1,000 children)", 
		low = "thistle2", high = "darkred") + 
	theme_bw() + 
	scale_y_continuous(breaks = NULL) + 
	scale_x_continuous(breaks = NULL) + 
	facet_wrap(~ variable, nrow = 3, ncol = 2) + 
	theme(legend.position = "bottom", panel.border = element_blank()) + 
	labs(title = "Child Abuse by State, 2008-2012", x = "", y = "")

# figure 2: perpetrator percent by state, 2012
fig2 <- ggplot(data = subset(states, variable == "perps"), aes(x = long, 
	y = lat, group = group, fill = value)) + 
	geom_polygon() + 
	scale_fill_continuous(name = "Perpetrator Percentage", 
		low = "thistle2", high = "darkred") + 
	theme_bw() + 
	scale_y_continuous(breaks = NULL) + 
	scale_x_continuous(breaks = NULL) + 
	theme(legend.position = "bottom", panel.border = element_blank()) + 
	labs(title = "2012 Perpetrator Percentage by State", x = "", y = "")

# regressions
census.reg <- data.frame(var = c("Intercept", "Northeast", "Midwest", "West"), 
	yr08 = reg_output("rate2008", "census", d), 
	yr09 = reg_output("rate2009", "census", d), 
	yr10 = reg_output("rate2010", "census", d), 
	yr11 = reg_output("rate2011", "census", d), 
	yr12 = reg_output("rate2012", "census", d))
south.reg <- data.frame(var = c("Intercept", "South"), 
	yr08 = reg_output("rate2008", "south", d), 
	yr09 = reg_output("rate2009", "south", d), 
	yr10 = reg_output("rate2010", "south", d), 
	yr11 = reg_output("rate2011", "south", d), 
	yr12 = reg_output("rate2012", "south", d))

# create output
write.table(census.reg, file = paste(dir, "output/census_regression.csv", 
            sep = "/"), sep = ",", row.names = FALSE, col.names = TRUE)
write.table(south.reg, file = paste(dir, "output/south_regression.csv", 
            sep = "/"), sep = ",", row.names = FALSE, col.names = TRUE)
pdf(paste(dir, "output/state_abuse_maps.pdf", sep = "/"))
	print(fig1)
	print(fig2)
dev.off()

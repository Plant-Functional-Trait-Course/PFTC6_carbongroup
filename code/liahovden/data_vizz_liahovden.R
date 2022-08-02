
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# A few constants
neeColor <- "#69b3a2"
erColor <- rgb(0.2, 0.6, 0.9, 1)

data = cflux_liahovden

# I am creating here a unique ID for each plot and measurement
data = data %>% 
  mutate(
    uniqueID =
      case_when(
        type == "NEE" ~ fluxID,
        type == "ER" ~ fluxID-1
      )
  )

NEE_lia_60 <- data %>% 
  select(fluxID, PARavg,temp_soilavg, turfID, type, datetime, flux, uniqueID) %>% 
  filter(type=="NEE") %>% 
  rename(fluxID_NEE = fluxID, PARavg_NEE= PARavg, temp_soilavg_NEE = temp_soilavg, datetime_NEE = datetime)

NEE_lia_60

ER_lia_60 <- data %>% 
  select(fluxID, PARavg,temp_soilavg, turfID, type, datetime, flux, uniqueID) %>% 
  filter(type=="ER") %>% 
  rename(fluxID_ER = fluxID, PARavg_ER= PARavg, temp_soilavg_ER = temp_soilavg, datetime_ER = datetime)

ER_lia_60 

ER_lia_60 <- ER_lia_60 %>% 
  pivot_wider(names_from = type, values_from = flux)

NEE_lia_60 <- NEE_lia_60 %>% 
  pivot_wider(names_from = type, values_from = flux)


flux_lia_60 <- merge(NEE_lia_60, ER_lia_60, by = c("turfID", "uniqueID"))


flux_lia_60 <- flux_lia_60 %>% 
  select("turfID", "uniqueID", "fluxID_NEE", "fluxID_ER", "datetime_NEE", "datetime_ER", "PARavg_NEE", "PARavg_ER", "temp_soilavg_NEE", "temp_soilavg_ER", "NEE", "ER")


flux_lia_60 <- flux_lia_60 %>% 
  mutate(
    GPP = NEE - ER
  )

plot(x = flux_lia_60$datetime_ER, y = flux_lia_60$GPP, col="red")

points(x = flux_lia_60$datetime_ER, y = flux_lia_60$ER, col ="blue")
#points(x = flux_lia_60$datetime_ER, y = flux_lia_60$NEE, col ="blue")

flux_lia_60 %>% 
  ggplot(aes(x = datetime_NEE, y = GPP)) +
  geom_line(aes(color = turfID)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

flux_lia_60 %>% 
  ggplot(aes(x = datetime_NEE, y = ER)) +
  geom_line(aes(color = turfID))

##############NEWWWWWWWW!!!!
## fluxes plots for presenting

source("code/liahovden/cleaning_liahovden.R")
source("code/metaturf.R")

# right join the metaturf     ------------------------------------------------
#(we are adding here the treatments, sites, and so on)

cflux_liahovden <- right_join(
  cflux_liahovden, metaturf)

## GPP over 24 h  -----------------------------------------------------------
# filter(type =="ER") %>% 
# filter(flux>=0) %>% 

vikes_shade <- data.frame(xmin=as.Date('2022-07-23 22:30:00'), 
                          xmax=as.Date('2022-07-24 04:46:00'), 
                          ymin=-Inf, 
                          ymax=Inf)

FluxPlot_liahovden <- 
  
  cflux_liahovden %>% 
  filter(type != "NEE") %>%
  mutate(
    type = str_replace_all(type, c(
      "ER" = "Ecosystem Respiration",
      "GEP" = "Gross Primary Production"
    ))
  ) %>% 
  # ggplot( aes(
  #  x = datetime, y = flux, group= turfID, color = warming)) +
  ggplot( aes(
    x = datetime, y = flux, color = warming, shape=type)) +
  geom_point() +
  facet_grid(type ~ ., scales = "free") +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 9),
              se = TRUE, size = 0.5, fullrange = FALSE) +
  
  geom_hline(
    yintercept = 0, linetype = "dashed", colour = "black") +
  
  geom_vline(
    xintercept = as.numeric(cflux_liahovden$datetime[505]),
    linetype = 2, colour = "orange", size=1) +
  geom_vline(
    xintercept = as.numeric(cflux_liahovden$datetime[81]),
    linetype = 2, colour = "blue", size=1) +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%b %d - %H:%M")) +
  ggtitle("Liahovden (1267 m a.s.l.)\nFluctuating Ecosystem Respiration pattern over 24 hours") +
  ylab("CO2 umol m2 h-1") +
  xlab("Time of the day (hours)") +
  theme(strip.text.y = element_text(size = 14, colour = "black"),
        axis.ticks = element_line(size=1.5), 
        axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
        axis.title = element_text(size = 16, color ="darkgrey"),
        #axis.title.x = element_blank(),
        axis.line = element_line(color = "grey"),
        axis.text = element_text(size = 12),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        # legend.title = element_text(size = 8),
        #legend.key.width = unit(0.4, 'cm'),
        #panel.grid.major.x = element_blank(), 
        #panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size=16)) +
  # panel.background = element_rect(
  #  fill = 'white', colour = 'grey')) +
  scale_color_manual(values = c(
    "ambient" = "#1e90ff",
    "transplant" = "#ff0800"
  )) 


FluxPlot_liahovden







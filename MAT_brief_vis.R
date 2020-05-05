# Visualisations for Matopiba Blog

library(tidyverse)
library(rgdal)
library(maptools)
library(chorddiag)
library(RColorBrewer)
options(scipen = 999)


# basic data --------------------------------------------------------------

trase <- read_csv('./Matopiba/BRAZIL_SOY.csv') %>%  filter(!is.na(GEOCODE), COUNTRY != 'BRAZIL')
mat_gc <- read_csv('./Matopiba/mat_gc.csv')
mat_shp <- readOGR('./Matopiba/mato_muni.shp')
mat_dat <- trase[trase$GEOCODE %in% mat_gc$GEOCODE,]

# define colours

colred <- colorRampPalette(c('#470306', '#F14A52', '#F6AEA2', '#F4EBDF'))
colblue <- colorRampPalette(c('#06425F', '#3881A4', '#8DB6CA', '#A1D9D5', '#EAF2EB'))

# 1 - Piaui Analysis -------------------------------------------------------
# Piaui maps -------------

# Maps
# just piaui muni

dat <- trase %>% filter(GEOCODE %in% mat_gc$GEOCODE, STATE == 'PIAUI') %>% group_by(GEOCODE, YEAR) %>% 
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% spread(YEAR, soy) %>% mutate_if(is.numeric, funs(replace(.,is.na(.), 0))) 

# all mato muni

dat2 <- trase %>% filter(GEOCODE %in% mat_gc$GEOCODE) %>% group_by(GEOCODE, YEAR) %>% 
  summarise(soy_mat = sum(SOY_TONS, na.rm = TRUE)) %>% spread(YEAR, soy_mat) %>% mutate_if(is.numeric, funs(replace(.,is.na(.), 0))) %>% 
  mutate(change_mat = `2015`-`2010`) %>% select(GEOCODE, change_mat)

# merge into shapefile
mat_shp <- merge(mat_shp, dat, by.x = 'codigo_ibg', by.y = 'GEOCODE', all= TRUE)
mat_shp <- merge(mat_shp, dat2, by.x = 'codigo_ibg', by.y = 'GEOCODE', all= TRUE)
writeSpatialShape(mat_shp, './Matopiba/piaui_change.shp')





# Piaui charts ----------

# stacked area chart of expansion per muni in Piaui
n <- 8


t10 <- trase %>% filter(GEOCODE %in% mat_gc$GEOCODE, STATE == 'PIAUI') %>% 
  group_by(GEOCODE, MUNICIPALITY) %>% summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% 
  arrange(desc(soy)) %>% head(.,n)

dat <- trase %>% filter(GEOCODE %in% mat_gc$GEOCODE, STATE == 'PIAUI') %>% 
  group_by(MUNICIPALITY, YEAR) %>% summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>%  spread(YEAR, soy) %>% 
  mutate_if(is.numeric, funs(replace(.,is.na(.), 0))) %>% gather(YEAR, soy, -MUNICIPALITY) %>% filter(MUNICIPALITY %in% t10$MUNICIPALITY)


# reorder factor levels
dat$MUNICIPALITY <- factor(dat$MUNICIPALITY, levels = unique(as.character(t10$MUNICIPALITY)))

ggplot(dat, aes(x = YEAR, y = soy, group = MUNICIPALITY)) + 
  geom_area(aes(fill = MUNICIPALITY)) + scale_fill_manual(values = colred(n)) + scale_y_continuous(limits = c(0,600000), expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = 'black'), plot.margin = unit(c(1,1,1,1), "cm"), text = element_text(family = 'DecimaMonoPro'))

ggsave(filename = './Matopiba/piaui.svg', device = 'svg')


colred <- colorRampPalette(c('#470306', '#F14A52', '#F6AEA2', '#F4EBDF'))
colred <- colorRampPalette(c('#470306', '#F14A52', '#F6AEA2', '#F4EBDF'))
colblue <- colorRampPalette(c('#06425F', '#3881A4', '#8DB6CA', '#A1D9D5', '#EAF2EB'))
colblue2 <- colorRampPalette(c('#06425F', '#EAF2EB'))

colmix <- colorRampPalette(c('#06425F', '#3881A4', '#8DB6CA', '#A1D9D5', '#EAF2EB'))







# 2 - New entrants analysis ---------------------------------------------------
comp <- c('AMAGGI', 'AGREX INC', 'NIDERA', 'NATURALLE AGRO MERCANTIL', 'CHS')
residents <- c("CARGILL", "BUNGE", "ADM", "MULTIGRAIN S.A.", "ABC INDUSTRIA E COMERCIO")

mat_dat %>%  filter(YEAR %in% c(2010,2015)) %>% 
  group_by(EXPORTER,YEAR) %>% summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% spread(YEAR, soy) %>%
  mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
  mutate(diff = `2015`-`2010`) %>% select(-c(`2015`, `2010`)) %>% arrange(desc(diff))


mutate(EXPORTER = ifelse(EXPORTER %in% comp, 'COMP', 'OTHER'))

# new entrants chord diagram ------------------------------------------

# state
dat <- mat_dat %>% filter(EXPORTER %in% comp) %>% 
  group_by(EXPORTER, STATE) %>% 
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% rename(from = EXPORTER, vol = soy, to = STATE)

# state
dat <- mat_dat %>% filter(EXPORTER %in% comp) %>% 
  group_by(EXPORTER, COUNTRY) %>%  
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% mutate(STATE = 'MATOPIBA SOY EXPORTS 2010-15') %>% 
  rename(from = EXPORTER, vol = soy, to = STATE)

# country
countries <- mat_dat %>% filter(EXPORTER %in% comp) %>% group_by(COUNTRY) %>% 
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% arrange(desc(soy)) %>% 
  top_n(4, soy) %>% .$COUNTRY

dat <- mat_dat %>% filter(EXPORTER %in% comp) %>% 
  mutate(COUNTRY = replace(COUNTRY, !COUNTRY %in% countries, 'OTHER COUNTRIES')) %>% 
  group_by(EXPORTER, COUNTRY) %>%  
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% 
  rename(from = COUNTRY, vol = soy, to = EXPORTER) %>% ungroup




matrix <- dat %>% mutate(to = as.character(to)) %>% 
  spread(from, vol) %>% 
  select(-to) %>%
  mutate_if(is.numeric, funs(replace(.,is.na(.), 0))) %>% 
  data.matrix()
rownames(matrix) <- dat$to %>% unique()


matrix2 = data.frame(matrix)
colnames(matrix2) <- colnames(matrix)

matrix2 <- matrix2[,c(2,4,1,3,5)]
matrix2 <- data.matrix(matrix2)


group_colours <- dat %>% 
  gather() %>%
  unique() %>%
  filter(key != "vol") %>%
  arrange(desc(key)) %>%
  mutate(colour = "#34444C") %>% 
  mutate(colour = replace(colour, value == 'GERMANY', "#FEEFC2")) %>% 
  mutate(colour = replace(colour, value == 'CHINA', "#ED6E6C")) %>%
  mutate(colour = replace(colour, value == 'NETHERLANDS', "#C277B1")) %>%
  mutate(colour = replace(colour, value == 'JAPAN', "#157F8A")) %>%
  mutate(colour = replace(colour, value == 'OTHER COUNTRIES', "#6FAF8C")) %>%
  .$colour
  
  
  
  mutate(colour = replace(colour, value == 'OTH', "#6FB08D")) %>%
  mutate(colour = replace(colour, value == 'NETHERLANDS', "#C277B1")) %>%
  mutate(colour = replace(colour, value == 'SPAIN', "#157F8A")) %>%
  .$colour





suh <- chorddiag(matrix, 
                 type = "bipartite",
                 showGroupnames = TRUE,
                 groupnamePadding = 15, 
                 groupnameFontsize = 10,
                 groupColors = group_colours,
                 groupedgeColor = "None",
                 chordedgeColor = "None",
                 groupThickness = 0.05,
                 groupPadding = 2,
                 categorynamePadding = 130, categorynameFontsize = 20,
                 showTicks = FALSE,
                 fadeLevel = 0,
                 tooltipGroupConnector = " to ", #&#x25c0;
                 tooltipUnit = "Tn",
                 tickInterval = NULL)

suh



dat <- mat_dat %>% filter(EXPORTER %in% comp) %>% 
  mutate(COUNTRY = replace(COUNTRY, !COUNTRY %in% countries, 'OTHER COUNTRIES')) %>% 
  group_by(EXPORTER, COUNTRY) %>%  
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% mutate(pc = soy/sum(soy))

sum(dat[dat$COUNTRY == 'CHINA',]$soy)/sum(dat$soy)



# new entrants stacked area -----------------------------------------------


dat <- mat_dat %>% filter(!EXPORTER %in% residents) %>% 
  mutate(EXPORTER = replace(EXPORTER, !EXPORTER %in% comp, 'OTHER EXPORTERS')) %>% group_by(EXPORTER,YEAR) %>% 
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% spread(YEAR, soy) %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
  gather(YEAR, soy, -EXPORTER)


dat <- mat_dat %>% 
  mutate(EXPORTER = replace(EXPORTER, !EXPORTER %in% c(comp, residents), 'OTHER EXPORTERS')) %>% 
  mutate(EXPORTER = replace(EXPORTER, EXPORTER %in% residents, 'BIG_5')) %>% 
  group_by(EXPORTER,YEAR) %>% 
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% spread(YEAR, soy) %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>% 
  gather(YEAR, soy, -EXPORTER)



order <- c('BIG_5', "OTHER EXPORTERS", "NIDERA", "NATURALLE AGRO MERCANTIL", "CHS", "AMAGGI", "AGREX INC")
dat$EXPORTER <- factor(dat$EXPORTER, levels = rev(as.character(order)))

ggplot(dat, aes(x = YEAR, y = soy, group = EXPORTER)) + geom_area(aes(fill = EXPORTER)) + 
  scale_y_continuous(limits = c(0,3000000), expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = 'black'), plot.margin = unit(c(1,1,1,1), "cm"))

ggsave('./Matopiba/newenetrants_sa2.svg', device = 'svg')





# 3 - Countries Analysis --------------------------------------------------

dat <- mat_dat %>% group_by(YEAR, COUNTRY) %>% 
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>%  mutate(soypc = soy/sum(soy)*100) %>% 
   select(-soy) %>% top_n(4,soypc) %>% spread(COUNTRY, soypc) %>% mutate_if(is.numeric, funs(replace(.,is.na(.), 0))) %>% ungroup %>% 
  mutate(Others = 100 - rowSums(.[2:7])) %>% gather(COUNTRY, soy, -YEAR)


countries <- mat_dat %>% group_by(COUNTRY) %>% summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% 
  arrange(desc(soy)) %>% filter(COUNTRY %in% dat$COUNTRY) %>% .$COUNTRY

countries <- c(countries, 'Others')

dat$COUNTRY <- factor(dat$COUNTRY, levels = rev(countries))
dat$YEAR <- as.character(dat$YEAR)

cols <- c('#EA6869', '#FFEB8B', '#2D586E', '#FFBD78', '#B0DE82', '#C2DFED', '#F2F2F2')



# proportional bar chart

ggplot(dat, aes(x=YEAR, y = soy, group = COUNTRY)) + geom_bar(stat = 'identity', aes(fill = COUNTRY), width = 0.7) + 
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0.8)) + scale_fill_manual(values = rev(colblue(7))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.y =  element_line(colour = 'black'), plot.margin = unit(c(1,1,1,1), "cm")) +coord_polar('y')


#pie  chart
years <- unique(dat$YEAR)

i <- 1

for (i in 1:length(years)){
 plot <- ggplot(dat[dat$YEAR == years[i],], aes(x='', y = soy, group = COUNTRY)) + geom_bar(stat = 'identity', aes(fill = COUNTRY), width = 1) + 
  scale_fill_manual(values = rev(colblue(7))) + coord_polar('y') +theme_void() + ggtitle(years[i])
 print(plot)
 ggsave(filename = paste('./Matopiba/', 'pie', years[i], '.svg', sep = ''), device = 'svg')
}

ggsave(filename = paste('./Matopiba/', 'pie', years[i], '.svg', sep = ''), device = 'svg')






# create bar chart with raw data ------------------------------------------------

dat <- mat_dat %>% group_by(YEAR, COUNTRY) %>% 
  summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>%  mutate(yrsum = sum(soy)) %>% 
  top_n(4, soy) %>% spread(COUNTRY, soy) %>% mutate_if(is.numeric, funs(replace(.,is.na(.), 0))) %>% ungroup %>% 
  mutate(Others = yrsum - rowSums(.[3:length(.)])) %>% select(-yrsum) %>% gather(COUNTRY, soy, -YEAR)


# plot bar chart

# reorder factor levels 
countries <- mat_dat %>% group_by(COUNTRY) %>% summarise(soy = sum(SOY_TONS, na.rm = TRUE)) %>% 
  arrange(desc(soy)) %>% filter(COUNTRY %in% dat$COUNTRY) %>% .$COUNTRY

countries <- c(countries, 'Others')

dat$COUNTRY <- factor(dat$COUNTRY, levels = rev(countries))
dat$YEAR <- as.character(dat$YEAR)

# plot 
ggplot(dat, aes(x=YEAR, y = soy, group = COUNTRY)) + geom_bar(stat = 'identity', aes(fill = COUNTRY), width = 0.7) + 
  scale_y_continuous(limits = c(0,8000000), expand = c(0,0)) +
  scale_x_discrete(expand = c(0,0.8)) + scale_fill_manual(values = rev(colblue(7))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.y =  element_line(colour = 'black'), plot.margin = unit(c(1,1,1,1), "cm"))

ggsave(filename = './Matopiba/raw_countries2.svg', device = 'svg')

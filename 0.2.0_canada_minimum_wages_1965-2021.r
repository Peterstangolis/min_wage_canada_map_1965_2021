

#=================  CANADIAN MINIMUM WAGE BY PROVINCES   ============#
##                        1965 - 2020                                #


## LOAD LIBRARIES
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gifski)

## TO RETRIEVE CANADA PROVINCES SHAPE FILE
library(rnaturalearth)
library(rnaturalearthhires)

library(wesanderson)
names(wes_palettes)
#?wes_palettes
pal = wes_palette(8, name = "Zissou1", type = "continuous")


library(extrafont)
library(sysfonts)

library(rnaturalearth)

library(sf)
library(sp)

library(stringr)
library(janitor)

library(tmap)

## clear memory
gc()


## Load in google fonts
font_add_google("Montserrat")


## Getting the Canadian flag
library(flagon)
library(magick)
library(png)
library(ragg)

## To View the Canadian Flag
magick::image_read(flagon::flags("ca"))

canada_flag <- magick::image_read(flagon::flags("ca"))
canada_mg <- rasterGrob(canada_flag, interpolate = TRUE)
ca_file <- flags(ccodes = "ca", filetype = "png")


## LOAD ONTARIO WAGES DATASET
df <- read_csv("data/min_wages_canada_prov_1965-2020.csv")



## ENSURE EVERY PROVINCE HAS A YEAR ENTERED AND A MINIMUM WAGE
## USING THE MINIMUM WAGE OF THE YEAR PREVIOUS IF THERE IS NO YEAR
df2 <- df %>% 
  group_by(Jurisdiction, Abbreviation) %>% 
  complete(Year = full_seq(1965:2021, 1)) %>% 
  fill(avg_wage) %>% 
  select(-median_wage) %>% 
  mutate(hover = paste0(Jurisdiction, "/n$", avg_wage))


## REPLACE NA VALUES WITH 0
df2[is.na(df2)] = 0

## NA count in each column
na_count <- sapply(df2, function(y) sum(length(which(is.na(y)))))


## =========  CREATING THE GEO DATASET WITH ALL YEARS  ========== ##


# minwage_all_years <- df2 %>% 
#   inner_join(prov2, by = c("Jurisdiction" = "Province"))
# 
# minwage_all_years <- minwage_all_years %>% 
#   #select(-Provinces) %>% 
#   rename(province = Jurisdiction) %>% 
#   relocate(province) %>% 
#   clean_names(case = "small_camel")
#   
# glimpse(minwage_all_years)
# 
# minwage_all_years <- minwage_all_years %>% 
#   st_as_sf(sf_column_name = "geometry")
# 
# 
# ### write the file
# st_write(minwage_all_years, "data/min_wage_canada_provinces_1965-2020.shp")



# ==============        2nd CANADA SHAPE FILE       ================ #

## RETRIEVE CANADIAN PROVINCES SHAPEFILE, RETURN SF CLASS OBJECT
canada <- ne_states(country = "Canada", returnclass = "sf")


## SELECT 'name', 'region', 'geometry' from CANADA sf DATASET
canada_prov <- canada %>% 
  select(c("name", "region", "geometry"))



# =======   USING THE SMALLER CANADA PROVINCES SHAPE FILE    ======= #

## REVIEW THE PROVINCE NAMES 
unique(minwage_df2$Jurisdiction)
unique(canada_prov$name)

## CHANGE QUEBEC'S NAME TO REMOVE THE ALT0232 - è
canada_prov$name[6] <- "Quebec"


## JOIN THE MIN WAGE DATASET WITH THE GEOMETRY SF DATASET
minwage_df2 <- df2 %>% 
  inner_join(canada_prov, by = c("Jurisdiction" = "name"))


## CONVERT THE FILE TO SF FORMAT
minwage_sf <- minwage_df2 %>% 
  st_as_sf(sf_column_name = "geometry")

## Write the new sf file
write_sf(minwage_sf, "data/map_data/minwage_canada_prov_1965-2020.shp")



#==================== LOAD CANADA SHAPE =============================#

## SHAPE FILE FROM STATSCAN - TOO LARGE TO USE FOR ANALYSIS
#prov <- read_sf("mapping_in_R/map_data/lpr_000b16a_e/lpr_000b16a_e.shp")

#====================================================================#

# 
# prov2 <- prov %>% 
#   select(c("PRUID", "PRNAME", "geometry"))
# glimpse(prov2)
# 
# 
# prov3 <- prov2 %>% 
#   mutate(PRNAME = str_split(string = PRNAME, pattern = " /")) 
#   #mutate(PRNAME = PRNAME[[1]][1])
# prov3$PRNAME


# =================    CLEAN UP PROVINCE NAMES   ==================== #

# ## CREATE A VECTOR OF CLEAN PROVINCE NAMES WITH THE TEXT FOLLOWING THE '/'
# provinces <- c()
# for (i in seq_along(prov2$PRNAME)) {
#   s <- if_else(str_detect(prov2$PRNAME[i], " /"), str_split(prov2$PRNAME[i], " /")[[1]][1], prov2$PRNAME[i])
#   provinces <- c(provinces, s)
#   
# }
# 
# prov2$Province <- provinces
# glimpse(prov2)

## =======================   UNTIL HERE   ========================== ##


## This Works!!
#if_else(str_detect(prov2$PRNAME[2], " /"), str_split(prov2$PRNAME[2], " /")[[1]][1], prov2$PRNAME[2])



## ================       TMAP PLOTS      ========================= ##



## ==============   PLOTTING THE SMALL SHAPE FILE   =============== ##

## CHANGING THE MINIMUM WAGE IN SASKATCHEWAN FROM OVER 30 IN '65, '66', '67
## TO THE MINIMUM WAGE IN SASKATCHEWAN IN 1968: $1.05
minwage_sf %>% 
  filter(avg_wage > 20 & Year < 1970)

## FIND THE MINIMUM WAGE IN SASKATCHEWAN FOR THE FIRST 5 YEARS OF THE DATASET
minwage_sf %>% 
  filter(Jurisdiction == "Saskatchewan" & Year < 1970)

# CHANGE THE MINIMUM WAGE TO $1.05 IN '65, '66, '67
minwage_sf$avg_wage[which(minwage_sf$Jurisdiction == "Saskatchewan" & minwage_sf$Year < 1968)] <- 1.05


## Setting new break points
breaks = c(0,2,4,6,8,10,12,14,16)

## CHANGE tmap_mode to 'plot' from 'view':interactive
tmap_mode("plot")

## SAVE THE IMAGE OF MINIMUM WAGES ACROSS CANADIAN PROVINCES IN 1976
png(filename = "plots/min_wage_1976.png",
    width = 7, height = 6.5, units = "in", res = 200)

minwage_sf %>% 
  filter(Year == 1976) %>% 
  tm_shape() +
  tm_polygons(col = "avg_wage", border.col = "#C2CCCB", 
              palette = pal,
              border.alpha = 0.9,
              #palette = "PuBu",
              breaks = breaks,
              title = "Avg. Yearly Min. Wage\n 1965-2021",
              title.size = 0.5,
              title.color = '#3A434D',
              title.fontfamily = "regular"
              ) +
              #legend.hist = T) +
  tm_legend(outside = T) +
  tm_facets(by = "Year", nrow = 2, free.coords = FALSE) +
  tm_text("Abbreviation",
  
          #if_else("Abbreviation" %in% c("NT", "NU"), just = c(.5, .3), just = c(0,0)),
          #size = 0.7,
          xmod = -.1,
          #ymod = -0.5,
          remove.overlap = FALSE,
          scale = 1,
          size = .8,
          shadow = F,
          col = '#3A434D',
          overwrite.lines = TRUE,
          fontfamily = "regular"
          ) +
  tm_layout(frame = F,
            title = "Canada",
            title.size = 4,
            title.color =  '#3A434D',
            title.fontfamily = "regular",
            title.fontface = "bold",
            title.position = c("LEFT", "TOP"),
            #legend.title.color = "blue",
            #main.title = "Provincial Minimum Wages \n2000",
            #main.title.color = "#7A130D",
            #main.title.fontfamily = "Arial",
            bg.color = "#F2FFFD",
            legend.text.color = "#3A434D",
            #legend.text.fontface = "bold"
            #legend.hist.size = 0.5
            legend.format = list(prefix = "$",
                                 text.separator = "-")
            ) +
  tm_credits("(c) Source: Government of Canada, \n  'Minimum Wage Database'\n| Peter Stangolis | Twitter: @PeterStan525 |", 
             position=c("left", "bottom"),
             size = 0.77,
             fontfamily = "regular",
             col = "#616F7F") +
  tm_view(text.size.variable = FALSE) +
  tm_logo(ca_file,
          position = c(.01, .8), height = 1.5)
          #position=c(.85, .8), height = 1.5) +

dev.off()



tmaptools::palette_explorer()

# ==================      ANIMATED PLOT       ======================= #


minwage_anim = tm_shape(minwage_sf) + 
  tm_polygons(col = "avg_wage", border.col = "#C2CCCB", 
              palette = pal,
              border.alpha = 0.7,
              #palette = "PuBu",
              breaks = breaks,
              title = "Avg. Yearly Min. Wage\n 1965-2021",
              title.size = 0.8,
              title.font.family = "regular",
              title.color = "#3A434D"
  ) +
  tm_legend(outside = TRUE) +
  tm_text("Abbreviation",
          remove.overlap = TRUE,
          fontface = "bold",
          fontfamily = "regular",
          size = 0.8,
          xmod = -.1,
          col = '#3A434D',
          overwrite.lines = T) +
  tm_layout(frame = T,
            title = "Canada",
            title.size = 3,
            title.fontfamily = "regular",
            title.fontface = "bold",
            title.color = "#3A434D",
            #legend.title.color = "blue",
            #main.title = "Provincial Minimum Wages \n2000",
            #main.title.color = "#7A130D",
            #main.title.fontfamily = "Arial",
            bg.color = "#F2FFFD",
            legend.text.fontfamily = "regular",
            #legend.hist.size = 0.5,
            legend.text.color = "#3A434D",
            #legend.text.fontface = "1",
            legend.format = list(prefix = "$", text.separator = "-")
  ) +
  tm_credits("(c) Source: Government of Canada, 'Minimum Wage Database'\n| Peter Stangolis | Twitter: @PeterStan525 |", 
             position=c("left", "bottom"),
             size = 0.7,
             fontfamily = "regular",
             col = "#616F7F") +
  tm_logo(ca_file,
          position = c(.01, .88), height = 1.5) +
  tm_facets(along = "Year", free.coords = FALSE)


tmap_animation(minwage_anim, filename = "plots/minwage_anim.gif", 
               delay = 75, width = 900, height = 700, loop = FALSE,
               dpi = 100
               )







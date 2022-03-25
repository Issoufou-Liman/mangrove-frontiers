# Stable sites
Tanbi_wetland_reserve =   extent(
  matrix(
    c(
      -16.667836644854212, 13.491574747844856,
      -16.667836644854212, 13.367684925708033,
      -16.474545934404993, 13.367684925708033,
      -16.474545934404993, 13.491574747844856
    ),
    ncol = 2,
    byrow = TRUE
  )
)
# decrease
Bambali_Island = extent(
  matrix(
    c(
      -15.46311710139718, 13.542648099464277,
      -15.46311710139718, 13.418784703127137,
      -15.269826390947962, 13.418784703127137,
      -15.269826390947962, 13.542648099464277
    ),
    ncol = 2,
    byrow = TRUE
  )
)
# increase_spread
Bolingho = extent(
  matrix(
    c(
      -15.719922521319056, 13.54131298784182,
      -15.719922521319056, 13.417448899452445,
      -15.526631810869837, 13.417448899452445,
      -15.526631810869837, 13.54131298784182
    ),
    ncol = 2,
    byrow = TRUE
  )
)

legend_table <- ggplot(
  data=data.frame(
    x = rep(1, length(myPalette)), y = rep(1, length(myPalette)), 
    myPalette = factor(1:16, labels = myPalette),
    className = className,
    variable = '2000'
  ) %>% rbind(
    . 
  )
)+
  geom_bar(aes(x, fill = myPalette) 
           # colour = 'white', size=2
  )+
  scale_fill_identity(labels = className, guide = 'legend', name = 'Land use and land cover')+
  guides(fill=guide_legend(nrow=4))+
  theme(
    # legend.background = element_rect(fill = 'gray'),
    legend.background = element_rect(fill = NA),
    legend.title = element_text(size = 10, face = 'bold'),
    # legend.text = element_text(size = 10),
    legend.title.align=0.5,
    legend.position = 'right'
  )

legend_table <- cowplot::get_legend(legend_table) 

## Raster stack ####
stk <- raster::stack(
  list(
    'manuscript-gambia/ee_output/Gambia_classified_2000_2002.tif',
    'manuscript-gambia/ee_output/Gambia_classified_2010_2012.tif',
    'manuscript-gambia/ee_output/Gambia_classified_2018_2020.tif'
  )
) %>% 
  `names<-`(
    str_replace_all(names(.), c(
      "Gambia_classified_2000_2002" = "Gambia_classified_2000", 
      "Gambia_classified_2010_2012" = "Gambia_classified_2010",
      "Gambia_classified_2018_2020" = "Gambia_classified_2020"
    )
    )
  )

my_theme <-
  theme_linedraw ()+
  theme(
    axis.title = element_blank(),
    strip.text = element_text(colour = 'black', size = 12, face = 'bold', margin = margin(t = 0, r = 0, b = 0, l = 0)),
    legend.position = 'none',
    strip.background = element_blank(),
    panel.spacing = unit(0.5, 'mm'),
    plot.margin = margin(0,0,0,0)
  )
blanc_grid_theme = theme(
  strip.text = element_text(size = 10),
  strip.text.x = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)
facet_grid_theme = theme(
  strip.text = element_text(size = 10),
  # strip.text.x = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)


# magnitudeClassName = c(
#   'Mangrove increase',
#   'Mangrove decrease',
#   'Mangrove stable'
# )
# myMagnitudePalette <- c(
#   'green', # Icrease 
#   'red',   # Decrease
#   'blue'   # Stable
# )
# 
# magnitude_legend_table <- ggplot()+
#   geom_bar(data=data.frame(
#     x = 1, y = 1,
#     colour = factor(1:4, labels = c('green', 'red', 'blue', "#D8F1FF"))
#   ),
#   aes(x, fill = colour))+
#   
#   geom_line(data = data.frame(x = 1:2, y = 1:2, colour = factor(1, labels = 'Ghana coastline')),
#             aes(x, y, linetype  = colour), colour = 'black')+
#   geom_polygon(data = data.frame(x = c(1,2,2,1,1), y = c(1,1,2,2,1), colour = factor(1, labels = 'red')), 
#                aes(x, y, colour = colour), fill = NA)+
#   scale_fill_identity(labels = c('Mangrove increase','Mangrove decrease','Mangrove stable','Water'), guide = 'legend')+
#   scale_colour_identity(labels = c('Mangrove areas'), guide = 'legend') +
#   theme_minimal()+
#   theme(
#     legend.title = element_blank(),
#     # legend.text = element_text(size = 5),
#     # legend.key.size = unit(0.5,"line"),
#     legend.spacing.y = unit(0, "lines"),
#     # legend.background = element_rect(fill = 'lightgray'),
#     legend.justification="center",
#     # legend.margin=margin(0,0,0,0),
#     # legend.box.margin=margin(0,0,0,0),
#     legend.background = element_rect(colour = "white"),
#     legend.box.background = element_rect(colour = "white", fill = 'white'),
#     plot.margin = margin(0,0,0,0)
#   )
# 
# magnitude_legend_table <- cowplot::get_legend(magnitude_legend_table) 

water_poly_path <- "manuscript-gambia/ee_output/Gambia_waters_polygons/Gambia_waters_polygons.shp"

ghana_shp=read_rds('manuscript-gambia/data_files/Gambia_shp_edited_RDS.rds')

coast_line <- read_rds('manuscript-gambia/data_files/Gambia_shp_edited_RDS.rds') %>%
  `st_crs<-`(4326) 
# %>%
#   rbind(
#     getData('GADM', country='SEN', level=0, path = 'manuscript-gambia/data_files') %>%
#       st_as_sf() %>%
#       `st_crs<-`(4326)
#   )

coast_line = 
  ggplot()+ 
  # geom_sf(data = st_read(water_poly_path) %>% 
  #           st_transform(crs = 4326) %>%
  #           st_crop(st_bbox(coast_line)),
  #         size = 0.10,
  #         fill = "#D8F1FF",
  #         colour = "#D8F1FF"
  # )+
  geom_sf(data = coast_line, fill = NA)+ 
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(Tanbi_wetland_reserve)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(Tanbi_wetland_reserve)), crs = 4326),
               aes(label = 'Tanbi\nWetlands'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(Bambali_Island)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(Bambali_Island)), crs = 4326),
               aes(label = 'Bambali\nIsland'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(Bolingho)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(Bolingho)), crs = 4326),
               aes(label = 'Bolingho'), colour = "black")+
  
  coord_sf(expand = FALSE)+
  theme_minimal()+
  theme(
    plot.margin = margin(0,0,0,0),
    axis.title = element_blank()
  )+
  annotation_custom(grob = legend_table, xmin = -15.755, xmax = -14.45, ymin = 13.1, ymax = 13.4)


Tanbi_wetland_reserve <- stk %>%
  crop(Tanbi_wetland_reserve) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Tanbi_Wetland') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  mutate(aoi_name = gsub('_', ' ', aoi_name)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

Bambali_Island <- stk %>%
  crop(Bambali_Island) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Bambali_Island') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  mutate(aoi_name = gsub('_', ' ', aoi_name)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

Bolingho <- stk %>%
  crop(Bolingho) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Bolingho') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  mutate(aoi_name = gsub('_', ' ', aoi_name)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

grid.arrange(
  coast_line,
  Tanbi_wetland_reserve + facet_grid_theme,
  Bambali_Island + blanc_grid_theme,
  Bolingho + blanc_grid_theme,
  ncol = 1,
  # heights = c(1.1, 0.98, 0.95, 0.95)
  heights = c(1.1, 0.95, 0.9, 0.9)
  ) %>% ggsave(
    filename = 'manuscript-gambia/figures/Gambia_classifications_slices.png',
    width = 10.5,
    height = 10.1
    )










# Stable sites
Niumi_national_park = extent(
  matrix(
    c(
      -16.597675124757316, 13.678265021697056,
      -16.597675124757316, 13.516754360837236,
      -16.440433303468254, 13.516754360837236,
      -16.440433303468254, 13.678265021697056
    ),
    ncol = 2,
    byrow = TRUE
  )
)

Tanbi_wetland_reserve =   extent(
  matrix(
    c(
      -16.662473664376904, 13.474707224400321,
      -16.662473664376904, 13.377447257132173,
      -16.569089875314404, 13.377447257132173,
      -16.569089875314404, 13.474707224400321
    ),
    ncol = 2,
    byrow = TRUE
  )
)
Tanbi_wetland_reserve =   extent(
  matrix(
    c(
      -16.667836644854212, 13.491574747844856,
      -16.667836644854212, 13.367684925708033,
      -16.474545934404993, 13.367684925708033,
      -16.474545934404993, 13.491574747844856
    ),
    ncol = 2,
    byrow = TRUE
  )
)
# increase sites
Bitang_Bolon = extent(
  matrix(
    c(
      -16.311030849165963, 13.335423003157722,
      -16.311030849165963, 13.219140307381195,
      -15.901790126509713, 13.219140307381195,
      -15.901790126509713, 13.335423003157722
    ),
    ncol = 2,
    byrow = TRUE
  )
)
Mini_minium_Bolon = extent(
  matrix(
    c(
      -16.253942803444737, 13.693314407608959,
      -16.253942803444737, 13.418965600932344,
      -16.054472283425206, 13.418965600932344,
      -16.054472283425206, 13.693314407608959
    ),
    ncol = 2,
    byrow = TRUE
  )
)

# decrease
Bambali_Island = extent(
  matrix(
    c(
      -15.506414243203462, 13.585018232130317,
      -15.506414243203462, 13.418434980119612,
      -15.269178220254243, 13.418434980119612,
      -15.269178220254243, 13.585018232130317
    ),
    ncol = 2,
    byrow = TRUE
  )
)
Bambali_Island = extent(
  matrix(
    c(
      -15.46311710139718, 13.542648099464277,
      -15.46311710139718, 13.418784703127137,
      -15.269826390947962, 13.418784703127137,
      -15.269826390947962, 13.542648099464277
    ),
    ncol = 2,
    byrow = TRUE
  )
)
# increase_spread
Bolingho = extent(
  matrix(
    c(
      -15.719922521319056, 13.54131298784182,
      -15.719922521319056, 13.417448899452445,
      -15.526631810869837, 13.417448899452445,
      -15.526631810869837, 13.54131298784182
    ),
    ncol = 2,
    byrow = TRUE
  )
)



geometry =   extent(
  matrix(
    c(
      -16.66221302676315, 13.594545108132703,
      -16.66221302676315, 13.37787270768964,
      -16.440426527739714, 13.37787270768964,
      -16.440426527739714, 13.594545108132703
    ),
    ncol = 2,
    byrow = TRUE
  )
)
Darsilami =   extent(
  matrix(
    c(
      -16.746876292236728, 13.161934529721556,
      -16.746876292236728, 13.065301726044805,
      -16.67237525463907, 13.065301726044805,
      -16.67237525463907, 13.161934529721556
    ),
    ncol = 2,
    byrow = TRUE
  )
)
geometry2 =   extent(
  matrix(
    c(
      -16.107238732949913, 13.578219277220251,
      -16.107238732949913, 13.533579096959873,
      -16.078485452310264, 13.533579096959873,
      -16.078485452310264, 13.578219277220251
    ),
    ncol = 2,
    byrow = TRUE
  )
)

# x        y value    variable            aoi_name

# lapply(as.character(seq(2000, 2020, by = 10)), function (i){
#   data.frame(
#     x = rep(1, length(myPalette)), y = rep(1, length(myPalette)), 
#     myPalette = factor(1:16, labels = myPalette),
#     className = className,
#     variable = i
#   )
# }) %>% do.call(what = rbind) %>%
#   ggplot(aes(x, fill = myPalette) )+
#   # geom_bar(
#   #          # colour = 'white', size=2
#   # )+
#   scale_fill_identity(labels = className, guide = 'legend', name = 'Land use and land cover')+
#   guides(fill=guide_legend(nrow=2))+
#   theme(
#     legend.background = element_rect(fill = 'gray'),
#     legend.title = element_text(size = 10, face = 'bold'),
#     # legend.text = element_text(size = 10),
#     legend.position = 'bottom'
#   ) + 
#   facet_wrap(~variable)


legend_table <- ggplot(
  data=data.frame(
    x = rep(1, length(myPalette)), y = rep(1, length(myPalette)), 
    myPalette = factor(1:16, labels = myPalette),
    className = className,
    variable = '2000'
  ) %>% rbind(
    . 
  )
)+
  geom_bar(aes(x, fill = myPalette) 
           # colour = 'white', size=2
  )+
  scale_fill_identity(labels = className, guide = 'legend', name = 'Land use and land cover')+
  guides(fill=guide_legend(nrow=2))+
  theme(
    legend.background = element_rect(fill = 'gray'),
    legend.title = element_text(size = 10, face = 'bold'),
    # legend.text = element_text(size = 10),
    legend.position = 'bottom'
  )

legend_table <- cowplot::get_legend(legend_table) 

## Raster stack ####
stk <- raster::stack(
  list(
    'manuscript-gambia/ee_output/Gambia_classified_2000_2002.tif',
    'manuscript-gambia/ee_output/Gambia_classified_2010_2012.tif',
    'manuscript-gambia/ee_output/Gambia_classified_2018_2020.tif'
  )
) %>% 
  `names<-`(
    str_replace_all(names(.), c(
      "Gambia_classified_2000_2002" = "Gambia_classified_2000", 
      "Gambia_classified_2010_2012" = "Gambia_classified_2010",
      "Gambia_classified_2018_2020" = "Gambia_classified_2020"
    )
    )
  )

# stable_sites <- extent(
#   matrix(c
#          (
#            -16.66427296328659, 13.679957904306919,
#            -16.66427296328659, 13.375868658579918,
#            -16.438366591216276, 13.375868658579918,
#            -16.438366591216276, 13.679957904306919
#          ),
#          ncol = 2,
#          byrow = TRUE
#   )
# )
# my_theme <-
#   # scale_x_continuous(expand = c(0,0))+
#   # scale_y_continuous(expand = c(0,0))+
#   # labs(x = 'Longitude', y = 'Latitude')+
#   theme_linedraw ()+
#   theme(
#     # axis.title = element_text(size = 10),
#     # axis.text = element_text(size = 10),
#     axis.title = element_blank(),
#     strip.text = element_text(colour = 'black', size = 12, face = 'bold', margin = margin(t = 0, b = 0)),
#     legend.position = 'none',
#     strip.background = element_blank(),
#     # plot.margin = margin(0.5,0.5,0.5,0.5),
#     plot.margin = margin(0,0,0,0)
#   )
my_theme <-
  theme_linedraw ()+
  theme(
    axis.title = element_blank(),
    strip.text = element_text(colour = 'black', size = 12, face = 'bold', margin = margin(t = 0, r = 0, b = 0, l = 0)),
    legend.position = 'none',
    strip.background = element_blank(),
    panel.spacing = unit(0.5, 'mm'),
    plot.margin = margin(0,0,0,0)
  )

# stable_sites <- stk %>%
#   crop(stable_sites) %>%
#   gplot_data(maxpixels = maxpixels) %>%
#   add_column(aoi_name = 'stable_sites') %>%
#   mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
#   mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
#   mutate(variable = gsub('_', ' - ', variable)) %>%
#   ggplot()+
#   geom_raster(aes(x, y, fill = value))+
#   geom_sf(data = st_as_sf(st_as_sfc(st_bbox(Niumi_national_park)),  crs = 4326), fill = NA, colour = 'red')+
#   geom_sf(data = st_as_sf(st_as_sfc(st_bbox(Tanbi_wetland_reserve)),  crs = 4326), fill = NA, colour = 'blue')+
#   facet_grid(rows = vars(aoi_name), cols = vars(variable))+
#   scale_fill_gradientn(colours = myPalette, na.value=NA) +
#   coord_sf(crs = 4326, expand = FALSE) +
#   my_theme+
#   geom_sf(st_as_sf(st_as_sfc(st_bbox(stable_sites)),  crs = 4326))




Niumi_national_park <- stk %>%
  crop(Niumi_national_park) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Niumi_national_park') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

Tanbi_wetland_reserve <- stk %>%
  crop(Tanbi_wetland_reserve) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Tanbi_wetland_reserve') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

Bitang_Bolon <- stk %>%
  crop(Bitang_Bolon) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Bitang_Bolon') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

Mini_minium_Bolon <- stk %>%
  crop(Mini_minium_Bolon) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Mini_minium_Bolon') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

Bambali_Island <- stk %>%
  crop(Bambali_Island) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Bambali_Island') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

Bolingho <- stk %>%
  crop(Bolingho) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Bolingho') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

Darsilami <- stk %>%
  crop(Darsilami) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Darsilami') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  ggplot()+
  geom_raster(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

grid.arrange(
  # Niumi_national_park + facet_grid_theme, 
  Tanbi_wetland_reserve + blanc_grid_theme,
  # Bitang_Bolon + blanc_grid_theme,
  # Mini_minium_Bolon + blanc_grid_theme,
  Bambali_Island + blanc_grid_theme,
  Bolingho + blanc_grid_theme,
  # Darsilami + blanc_grid_theme,
  ncol = 1
  # nrow = 2,
  # heights = c(1, 0.9675)
)










blanc_grid_theme = theme(
  strip.text = element_text(size = 10),
  strip.text.x = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)
stables <- arrangeGrob(
  Niumi_national_park + facet_grid_theme, 
  Tanbi_wetland_reserve + blanc_grid_theme
)
increases <- arrangeGrob(
  Bitang_Bolon + blanc_grid_theme,
  arrangeGrob(
    Mini_minium_Bolon + blanc_grid_theme,
    Bambali_Island + blanc_grid_theme, 
    nrow = 1,
    widths = c(1/3, 2/3)
  )
)
grid.arrange(increases)
grid.arrange(
  Niumi_national_park + facet_grid_theme, 
  Tanbi_wetland_reserve + blanc_grid_theme,
  Bitang_Bolon + blanc_grid_theme,
  Mini_minium_Bolon + blanc_grid_theme,
  Bambali_Island + blanc_grid_theme,
  Bolingho + blanc_grid_theme,
  Darsilami + blanc_grid_theme,
  ncol = 1
  # nrow = 2,
  # heights = c(1, 0.9675)
)

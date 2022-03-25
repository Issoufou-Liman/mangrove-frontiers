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
  scale_fill_identity(labels = className, guide = 'legend', name = 'Land use\nand\nland cover')+
  # guides(fill=guide_legend(nrow=4))+
  guides(fill=guide_legend(nrow=2))+
  
  theme(
    legend.background = element_rect(fill = 'gray'),
    # legend.background = element_rect(fill = NA),
    legend.title = element_text(size = 10, face = 'bold'),
    # legend.text = element_text(size = 10),
    legend.title.align=0.5,
    legend.position = 'bottom'
  )

legend_table <- cowplot::get_legend(legend_table) 

my_theme <-
  theme_linedraw ()+
  theme(
    axis.title = element_blank(),
    strip.text = element_text(colour = 'black', size = 12, face = 'bold', margin = margin(t = 0, r = 0, b = 0, l = 0)),
    legend.position = 'none',
    # strip.background = element_blank(),
    strip.background = element_rect(fill = 'gray', colour = 'gray'),
    panel.spacing = unit(0.5, 'mm'),
    plot.margin = margin(0,0,0,0)
  )
blanc_grid_theme_x = theme(
  strip.text = element_text(size = 10),
  strip.text.x = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)
blanc_grid_theme_y = theme(
  strip.text = element_text(size = 10),
  strip.text.y = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)
facet_grid_theme = theme(
  strip.text = element_text(size = 10),
  # strip.text.x = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank()
)

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
  )
# +
#   annotation_custom(grob = legend_table, xmin = -15.755, xmax = -14.45, ymin = 13.1, ymax = 13.4)


Tanbi_wetland_reserve <- stk %>%
  crop(Tanbi_wetland_reserve) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Tanbi_Wetland') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  mutate(aoi_name = gsub('_', ' ', aoi_name)) %>%
  ggplot()+
  geom_tile(aes(x, y, fill = value))+
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
  geom_tile(aes(x, y, fill = value))+
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
  geom_tile(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

# grid.arrange(
#   coast_line,
#   Tanbi_wetland_reserve + facet_grid_theme,
#   Bambali_Island + blanc_grid_theme_x,
#   Bolingho + blanc_grid_theme_x,
#   legend_table,
#   ncol = 1,
#   # heights = c(1.1, 0.98, 0.95, 0.95)
#   heights = c(1.2, 0.95, 0.9, 0.9, 0.1)
#   ) 
grid.arrange(
  coast_line,
  Tanbi_wetland_reserve + facet_grid_theme,
  Bambali_Island + blanc_grid_theme_x,
  Bolingho + blanc_grid_theme_x,
  legend_table,
  ncol = 1,
  # heights = c(1.1, 0.98, 0.95, 0.95)
  heights = c(1.095, 0.95, 0.9, 0.9, 0.2)
) %>% ggsave(
  filename = 'manuscript-gambia/figures/Gambia_classifications_slices_1.png',
  # width = 10.5,
  width = 10.25,
  height = 10.1
)

# stable
Niumi_national_park = extent(
  matrix(
    c(
      -16.63915093332755, 13.766020186456542,
      -16.63915093332755, 13.491755590673906,
      -16.439680413308018, 13.491755590673906,
      -16.439680413308018, 13.766020186456542
    ),
    ncol = 2,
    byrow = TRUE
  )
)

# Decrease
Darsilami = extent(
  matrix(
    c(
      -16.815618828835362, 13.339472434884375,
      -16.815618828835362, 13.064720113622773,
      -16.61614830881583, 13.064720113622773,
      -16.61614830881583, 13.339472434884375
    ),
    ncol = 2,
    byrow = TRUE
  )
)
# the small one
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
# increase
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
    data = st_as_sf(st_as_sfc(st_bbox(Mini_minium_Bolon)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(Mini_minium_Bolon)), crs = 4326),
               aes(label = 'Mini\nminium\nBolon'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(Bitang_Bolon)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(Bitang_Bolon)), crs = 4326),
               aes(label = 'Bitang\nBolon'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(Darsilami)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(Darsilami)), crs = 4326),
               aes(label = 'Darsilami'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(Niumi_national_park)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(Niumi_national_park)), crs = 4326),
               aes(label = 'Niumi\nnational\npark'), colour = "black")+
  
  coord_sf(expand = FALSE)+
  theme_minimal()+
  theme(
    plot.margin = margin(0,0,0,0),
    axis.title = element_blank()
  )
# +
#   annotation_custom(grob = legend_table, xmin = -15.755, xmax = -14.45, ymin = 13.1, ymax = 13.4)

Niumi_national_park <- stk %>%
  crop(Niumi_national_park) %>%
  gplot_data(maxpixels = maxpixels) %>%
  add_column(aoi_name = 'Niumi_national_park') %>%
  mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
  mutate(variable = gsub('Gambia_classified_', '', variable)) %>%
  mutate(variable = gsub('_', ' - ', variable)) %>%
  mutate(aoi_name = gsub('_', ' ', aoi_name)) %>%
  ggplot()+
  geom_tile(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  # facet_grid(rows = vars(variable), cols = vars(aoi_name))+
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
  mutate(aoi_name = gsub('_', ' ', aoi_name)) %>%
  ggplot()+
  geom_tile(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  # facet_grid(rows = vars(variable), cols = vars(aoi_name))+
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
  mutate(aoi_name = gsub('_', ' ', aoi_name)) %>%
  ggplot()+
  geom_tile(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  # facet_grid(rows = vars(variable), cols = vars(aoi_name))+
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
  mutate(aoi_name = gsub('_', ' ', aoi_name)) %>%
  ggplot()+
  geom_tile(aes(x, y, fill = value))+
  facet_grid(rows = vars(aoi_name), cols = vars(variable))+
  # facet_grid(rows = vars(variable), cols = vars(aoi_name))+
  scale_fill_gradientn(colours = myPalette, na.value=NA) +
  coord_sf(crs = 4326, expand = FALSE) +
  my_theme

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
  # guides(fill=guide_legend(nrow=2))+
  guides(fill=guide_legend(ncol=4))+
  # guides(fill=guide_legend(ncol=1, title.position = 'top'))+
  
  theme(
    legend.background = element_rect(fill = 'gray'),
    # legend.background = element_rect(fill = NA),
    legend.title = element_text(size = 10, face = 'bold'),
    # legend.text = element_text(size = 10),
    legend.title.align=0.5,
    legend.position = 'right'
  )

legend_table <- cowplot::get_legend(legend_table) 
grid.arrange(
  coast_line,
  Bitang_Bolon + facet_grid_theme,
  arrangeGrob(
    Niumi_national_park + facet_grid_theme,
    Mini_minium_Bolon + facet_grid_theme,
    ncol = 2
  )
  ,
  # legend_table,
  arrangeGrob(
    Darsilami + blanc_grid_theme_x,
    legend_table,
    ncol = 2
  ),
  ncol = 1,
  # heights = c(
  #   0.6,
  #   0.25,
  #   # 0.675,
  #   0.5, 
  #   0.45
  # )
  # heights = c(
  #   0.55,
  #   0.25,
  #   # 0.675,
  #   0.5, 
  #   0.45
  # )
  heights = c(
    0.55,
    0.225,
    # 0.675,
    0.5, 
    0.45
  )
)%>% ggsave(
  filename = 'manuscript-gambia/figures/Gambia_classifications_slices_2.png',
  # width = 9.56,
  # height = 8.62
  width = 12.1,
  height = 10.1
)


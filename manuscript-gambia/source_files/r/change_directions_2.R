magnitudeClassName = c(
  'Mangrove increase',
  'Mangrove decrease',
  'Mangrove stable'
)
myMagnitudePalette <- c(
  'green', # Icrease 
  'red',   # Decrease
  'blue'   # Stable
)

magnitude_legend_table <- ggplot()+
  geom_bar(data=data.frame(
    x = 1, y = 1,
    colour = factor(1:4, labels = c('green', 'red', 'blue', "#D8F1FF"))
  ),
  aes(x, fill = colour))+
  
  geom_line(data = data.frame(x = 1:2, y = 1:2, colour = factor(1, labels = 'Ghana coastline')),
            aes(x, y, linetype  = colour), colour = 'black')+
  geom_polygon(data = data.frame(x = c(1,2,2,1,1), y = c(1,1,2,2,1), colour = factor(1, labels = 'red')), 
               aes(x, y, colour = colour), fill = NA)+
  scale_fill_identity(labels = c('Mangrove increase','Mangrove decrease','Mangrove stable','Water'), guide = 'legend')+
  scale_colour_identity(labels = c('Mangrove areas'), guide = 'legend') +
  theme_minimal()+
  theme(
    legend.title = element_blank(),
    # legend.text = element_text(size = 5),
    # legend.key.size = unit(0.5,"line"),
    legend.spacing.y = unit(0, "lines"),
    # legend.background = element_rect(fill = 'lightgray'),
    legend.justification="center",
    # legend.margin=margin(0,0,0,0),
    # legend.box.margin=margin(0,0,0,0),
    legend.background = element_rect(colour = "white"),
    legend.box.background = element_rect(colour = "white", fill = 'white'),
    plot.margin = margin(0,0,0,0)
  )

magnitude_legend_table <- cowplot::get_legend(magnitude_legend_table) 

water_poly_path <- "data_files/Ghana_waters_polygons_cleaned/Ghana_waters_polygons_cleaned.shp"

ghana_shp=read_rds('data_files/Ghana_shp_edited_RDS.rds')
ghana_shp <- as(ghana_shp, 'SpatialLinesDataFrame')
ghana_shp = ghana_shp %>%
  st_as_sf()

coast_line = data.frame(
  matrix(
    c(
      -2.080081487787775, 4.650183138367896,
      0.6933146942776203, 5.660764034931583,
      1.0558635224026203, 5.731822534447855,
      1.1332828034084796, 6.07818941115139,
      1.1930209625881671, 6.108743189225873,
      1.1894160736721515, 6.1111327967829405,
      1.1389476288479328, 6.10234240216034,
      -2.6403492461520672, 5.047191291479991,
      -3.001181460507536, 5.071557730764194,
      -3.107096530087614, 5.092247108309992,
      -3.112418032773161, 5.0927600596375875,
      -3.1119030486423016, 5.086091660470985,
      -2.080081487787775, 4.650183138367896
    ),
    ncol = 2,
    byrow = TRUE
  )
) %>%
  purrr::set_names(c('lon', 'lat'))%>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

coast_line <- st_intersection(ghana_shp, coast_line)

coast_line = ggplot()+ 
  # geom_sf(data = st_read(water_poly_path) %>%
  #           st_crop(st_bbox(coast_line)),
  #         size = 0.10,
  #         fill = "#D8F1FF",
  #         colour = "#D8F1FF"
  # )+
  geom_sf(data = coast_line)+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_west_coast)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_west_coast)), crs = 4326),
               aes(label = 'West Coasts'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_kpani_river)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_kpani_river)), crs = 4326),
               aes(label = 'Kpani River'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_takoradi)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_takoradi)), crs = 4326),
               aes(label = 'Takoradi'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_pra)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_pra)), crs = 4326),
               aes(label = 'Pra River'), colour = "black")+
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_benya)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_benya)), crs = 4326),
               aes(label = 'Benya - Fosu Lagoon'), colour = "black")+
  
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_narkwa)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_narkwa)), crs = 4326),
               aes(label = 'Narkwa Basin'), colour = "black")+
  
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_muni)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_muni)), crs = 4326),
               aes(label = 'Muni Lagoon'), colour = "black")+
  
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_densu)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_densu)), crs = 4326),
               aes(label = 'Densu - Kepshi Lagoon'), colour = "black")+
  
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_laiwi)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_laiwi)), crs = 4326),
               aes(label = 'Laiwi - Moyo Lagoon'), colour = "black")+
  
  ##
  geom_sf(
    data = st_as_sf(st_as_sfc(st_bbox(aoi_keta)), crs = 4326),
    fill = NA, colour = 'red'
  )+
  geom_sf_text(data = st_as_sf(st_as_sfc(st_bbox(aoi_keta)), crs = 4326),
               aes(label = 'East Coasts'), colour = "black")+
  
  coord_sf(expand = FALSE)+
  theme_minimal()+
  theme(
    plot.margin = margin(0,0,0,0),
    axis.title = element_blank()
  )+
  annotation_custom(grob = magnitude_legend_table, xmin = 0.627, xmax = Inf, ymin = -Inf, ymax = 5.4)


my_theme <-theme_void()+
  theme(
    legend.position = 'none',
    # plot.background = element_rect(fill = 'gray'),
    plot.margin = margin(0,0,0,0),
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 12, face = 'bold', colour = "grey", margin = margin(b = 5)),
    strip.text.y = element_text(size = 12, face = 'bold', colour = "grey", margin = margin(r =5))
  )

## Raster stack ####
stk <- raster::stack(
  list(
    'ee_output/mangrove_change_magnitude_2000_2010.tif ',
    'ee_output/mangrove_change_magnitude_2010_2020.tif ',
    'ee_output/mangrove_change_magnitude_2000_2020.tif '
  )
) 
# %>% focal_stack()

# focal(raster('ee_output/mangrove_change_magnitude_2010_2020.tif'), w=matrix(1,3,3), fun= modal, ties = 'NA', pad=TRUE, filename = 'ee_output/mangrove_change_magnitude_2010_2020_focal.tif', format = 'GTiff')
# focal(raster('ee_output/mangrove_change_magnitude_2000_2020.tif'), w=matrix(1,3,3), fun= modal, ties = 'NA', pad=TRUE, filename = 'ee_output/mangrove_change_magnitude_2000_2020_focal.tif', format = 'GTiff')

## Last 5 AOI ####

grid.arrange(
  arrangeGrob(
    arrangeGrob(
      stk %>%
        crop(aoi_west_coast) %>%
        gplot_data(maxpixels = maxpixels) %>%
        add_column(aoi_name = 'West Coast') %>%
        mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
        mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
        mutate(variable = gsub('_', ' - ', variable))%>%
        mutate(variable = factor(variable, levels = unique(variable))) %>%
        ggplot()+
        # geom_polygon(data = sf::st_read(water_poly_path)%>%
        #           st_crop(st_bbox(aoi_west_coast))%>%
        #             as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
        #         size = 0.10,
        #         fill = "#D8F1FF",
        #         colour = "#D8F1FF"
        # )+
        geom_raster(aes(x, y, fill = value))+
        facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'y', scales = 'free'
        )+
        scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
        geom_text(
          data =  data.frame(
            lon = c(-2.8677856251686373),
            lat = c(4.976726073803081),
            label = c("Atlantic Ocean"),
            angle = c(0)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_text(
          data = data.frame(
            lon = c(-2.8828918263405123,-2.694064311692075,-2.5272094532936373,-2.4709045216530123),
            lat = c(5.04923208715484,5.061543627095028,5.084798118354067,5.0369203134713105),
            label = c("Half Assini","Tikobo 1","Tikobo 2","Aiyinase"),
            angle = c(0,0,0,0)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_point(
          data = data.frame(
            lon = c(-2.8828918263405123,-2.694064311692075,-2.5272094532936373,-2.4709045216530123),
            lat = c(5.04923208715484,5.061543627095028,5.084798118354067,5.0369203134713105),
            label = c("Half Assini","Tikobo 1","Tikobo 2","Aiyinase"),
            angle = c(0,0,0,0)
          ),
          aes(lon, lat), shape = '*', size = 4
        )+
        my_theme+
        geom_hline(yintercept = extent(aoi_west_coast)@ymax)+
        geom_hline(yintercept = extent(aoi_west_coast)@ymin)+
        geom_vline(xintercept = extent(aoi_west_coast)@xmax)+
        geom_vline(xintercept = extent(aoi_west_coast)@xmin)+
        scale_x_continuous(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0)),
      
      stk %>%
        crop(aoi_kpani_river) %>%
        gplot_data(maxpixels = maxpixels) %>%
        add_column(aoi_name = 'Kpani River')%>%
        mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
        mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
        mutate(variable = gsub('_', ' - ', variable))%>%
        mutate(variable = factor(variable, levels = unique(variable))) %>%
        ggplot()+
        # geom_polygon(data = sf::st_read(water_poly_path)%>%
        #           st_crop(st_bbox(aoi_kpani_river))%>%
        #             as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
        #         size = 0.10,
        #         fill = "#D8F1FF",
        #         colour = "#D8F1FF"
        # )+
        geom_raster(aes(x, y, fill = value))+
        facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'y', scales = 'free'
        )+
        scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
        geom_text(
          data =  data.frame(
            lon = c(-2.1493744347126253,-2.120052885802617,-2.102714825307248),
            lat = c(4.781471590291316,4.7921165770896526,4.807196908808476),
            label = c("Atlantic Ocean","Ehunli Lagoon","Kpani River"),
            angle = c(0,-25,30)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_text(
          data = data.frame(
            lon = c(-2.1698360499346436,-2.176746642586011,-2.085679059883374),
            lat = c(4.809183444659566,4.816153983015051,4.813246037044116),
            label = c("Miemia","Domunli","Asubey"),
            angle = c(0,0,0)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_point(
          data = data.frame(
            lon = c(-2.1698360499346436,-2.176746642586011,-2.085679059883374),
            lat = c(4.809183444659566,4.816153983015051,4.813246037044116),
            label = c("Miemia","Domunli","Asubey"),
            angle = c(0,0,0)
          ),
          aes(lon, lat), shape = '*', size = 4
        )+
        my_theme+
        geom_hline(yintercept = extent(aoi_kpani_river)@ymax)+
        geom_hline(yintercept = extent(aoi_kpani_river)@ymin)+
        geom_vline(xintercept = extent(aoi_kpani_river)@xmax)+
        geom_vline(xintercept = extent(aoi_kpani_river)@xmin)+
        scale_x_continuous(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))+
        theme(
          strip.background.y = element_blank(),
          strip.text.y = element_blank()
        ),
      
      stk %>%
        crop(aoi_takoradi) %>%
        gplot_data(maxpixels = maxpixels) %>%
        add_column(aoi_name = 'Takoradi')%>%
        mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
        mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
        mutate(variable = gsub('_', ' - ', variable))%>%
        mutate(variable = factor(variable, levels = unique(variable))) %>%
        ggplot()+
        # geom_polygon(data = sf::st_read(water_poly_path)%>%
        #           st_crop(st_bbox(aoi_takoradi))%>%
        #             as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
        #         size = 0.10,
        #         fill = "#D8F1FF",
        #         colour = "#D8F1FF"
        # )+
        geom_raster(aes(x, y, fill = value))+
        facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'y', scales = 'free'
        )+
        scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
        geom_text(
          data =  data.frame(
            lon = c(-1.8416234443311708, -1.7674228141431825, -1.9100192605603294),
            lat = c(4.841712950093481, 4.874232826047205, 4.821100503966857),
            label = c("Atlantic Ocean", "Whin Estuary", "Butre Estuary"),
            angle = c(0, 0, 0)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_text(
          data = data.frame(
            lon = c(-1.7784826039028379),
            lat = c(4.9316354963270985),
            label = c("Takoradi"),
            angle = c(0)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_point(
          data = data.frame(
            lon = c(-1.7784826039028379),
            lat = c(4.9316354963270985),
            label = c("Takoradi"),
            angle = c(0)
          ),
          aes(lon, lat), shape = '*', size = 4
        )+
        my_theme+
        geom_hline(yintercept = extent(aoi_takoradi)@ymax)+
        geom_hline(yintercept = extent(aoi_takoradi)@ymin)+
        geom_vline(xintercept = extent(aoi_takoradi)@xmax)+
        geom_vline(xintercept = extent(aoi_takoradi)@xmin)+
        scale_x_continuous(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))+
        theme(
          strip.background.y = element_blank(),
          strip.text.y = element_blank()
        ),
      
      stk %>%
        crop(aoi_pra) %>%
        gplot_data(maxpixels = maxpixels) %>%
        add_column(aoi_name = 'Pra River')%>%
        mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
        mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
        mutate(variable = gsub('_', ' - ', variable))%>%
        mutate(variable = factor(variable, levels = unique(variable))) %>%
        ggplot()+
        # geom_polygon(data = sf::st_read(water_poly_path)%>%
        #           st_crop(st_bbox(aoi_pra))%>%
        #             as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
        #         size = 0.10,
        #         fill = "#D8F1FF",
        #         colour = "#D8F1FF"
        # )+
        geom_raster(aes(x, y, fill = value))+
        facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'y', scales = 'free'
        )+
        scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
        geom_text(
          data =  data.frame(
            lon = c(-1.5764157055568906,-1.616756129140875),
            lat = c(5.009457975895493,5.079625280635088),
            label = c("Atlantic Ocean","Pra River"),
            angle = c(0,90)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_text(
          data = data.frame(
            lon = c(-1.6605297802639218,-1.5887753246975156,-1.6325489758205625),
            lat = c(5.046223160923197,5.065203442075796,5.009457975895493),
            label = c("Assoko\nEssaman","Fawumaye","Shama"),
            angle = c(0,0,0)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_point(
          data = data.frame(
            lon = c(-1.6605297802639218,-1.5887753246975156,-1.6325489758205625),
            lat = c(5.046223160923197,5.065203442075796,5.009457975895493),
            label = c("Assoko\nEssaman","Fawumaye","Shama"),
            angle = c(0,0,0)
          ),
          aes(lon, lat), shape = '*', size = 4
        )+
        my_theme+
        geom_hline(yintercept = extent(aoi_pra)@ymax)+
        geom_hline(yintercept = extent(aoi_pra)@ymin)+
        geom_vline(xintercept = extent(aoi_pra)@xmax)+
        geom_vline(xintercept = extent(aoi_pra)@xmin)+
        scale_x_continuous(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))+
        theme(
          strip.background.y = element_blank(),
          strip.text.y = element_blank()
        ),
      
      stk %>%
        crop(aoi_benya) %>%
        gplot_data(maxpixels = maxpixels) %>%
        add_column(aoi_name = 'Benya - Fosu Lagoon')%>%
        mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
        mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
        mutate(variable = gsub('_', ' - ', variable))%>%
        mutate(variable = factor(variable, levels = unique(variable))) %>%
        ggplot()+
        # geom_polygon(data = sf::st_read(water_poly_path)%>%
        #           st_crop(st_bbox(aoi_benya))%>%
        #           as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
        #         size = 0.10,
        #         fill = "#D8F1FF",
        #         colour = "#D8F1FF"
        # )+
        geom_raster(aes(x, y, fill = value))+
        facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'y', scales = 'free'
        )+
        scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
        geom_text(
          data = data.frame(
            lon = c(-1.303345370168707,-1.3559166668606015,-1.2609709042263242),
            lat = c(5.088030339151958,5.084375525149499,5.107436839672913),
            label = c("Atlantic Ocean","Benya Lagoon","Fosu Lagoon"),
            angle = c(0,0,0)
          ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_text(
          data = data.frame(
            lon = c(-1.296307,-1.282552),
            lat = c(5.108140,5.103523),
            label = c("Amamoma","University of Cape Coast"),
            angle = c(0,0)
            ),
          aes(lon, lat, label = label, angle = angle), size = 1
        )+
        geom_point(
          data = data.frame(
            lon = c(-1.296307,-1.282552),
            lat = c(5.108140,5.103523),
            label = c("Amamoma","University of Cape Coast"),
            angle = c(0,0)
          ),
          aes(lon, lat), shape = '*', size = 4
        )+
        my_theme+
        geom_hline(yintercept = extent((aoi_benya))@ymax)+
        geom_hline(yintercept = extent((aoi_benya))@ymin)+
        geom_vline(xintercept = extent((aoi_benya))@xmax)+
        geom_vline(xintercept = extent((aoi_benya))@xmin)+
        scale_x_continuous(expand = c(0, 0))+
        scale_y_continuous(expand = c(0, 0))+
        theme(
          strip.background.y = element_blank(),
          strip.text.y = element_blank()
        ), nrow=1
    ), ggplotGrob(coast_line), ncol = 1
  ),
  
  arrangeGrob(
    stk %>%
      crop(aoi_narkwa) %>%
      gplot_data(maxpixels = maxpixels) %>%
      add_column(aoi_name = 'Narkwa Basin') %>%
      mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
      mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
      mutate(variable = gsub('_', ' - ', variable))%>%
      mutate(variable = factor(variable, levels = unique(variable))) %>%
      ggplot()+
      # geom_polygon(data = sf::st_read(water_poly_path)%>%
      #           st_crop(st_bbox(aoi_narkwa))%>%
      #           as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
      #         size = 0.10,
      #         fill = "#D8F1FF",
      #         colour = "#D8F1FF"
      # )+
      geom_raster(aes(x, y, fill = value))+
      facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'both', scales = 'free'
      )+
      scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
      geom_text(
        data = data.frame(
          lon = c(-0.9140737262054732,-0.913,-1.007208),
          lat = c(5.201008741520865,5.22,5.206487),
          label = c("Atlantic Ocean","Narkwa Lagoon","Amisa Lagoon"),
          angle = c(0,10,0)
        ),
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_text(
        data = data.frame(
          lon = c(-0.9318406787201217,-0.9226567950531295,-0.9513242450043013,-1.0268559115698461,-1.026663),
          lat = c(5.20844522324624,5.231608872010555,5.2432331108684265,5.239728471432195,5.200710),
          label = c("Ekumfi Narkwa","Ekumfi Atwa","Ekumfi Nanabin","Abonko","Ankaful"),
          angle = c(0,0,0,0,-10)
        ),
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_point(
        data = data.frame(
          lon = c(-0.9318406787201217,-0.9226567950531295,-0.9513242450043013,-1.0268559115698461,-1.026663),
          lat = c(5.20844522324624,5.231608872010555,5.2432331108684265,5.239728471432195,5.200710),
          label = c("Ekumfi Narkwa","Ekumfi Atwa","Ekumfi Nanabin","Abonko","Ankaful"),
          angle = c(0,0,0,0,-10)
        ),
        aes(lon, lat), shape = '*', size = 4
      )+
      my_theme+
      geom_hline(yintercept = extent(aoi_narkwa)@ymax)+
      geom_hline(yintercept = extent(aoi_narkwa)@ymin)+
      geom_vline(xintercept = extent(aoi_narkwa)@xmax)+
      geom_vline(xintercept = extent(aoi_narkwa)@xmin)+
      scale_x_continuous(expand = c(0, 0))+
      scale_y_continuous(expand = c(0, 0)),
    
    stk %>%
      crop(aoi_muni) %>%
      gplot_data(maxpixels = maxpixels) %>%
      add_column(aoi_name = 'Muni Lagoon')%>%
      mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
      mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
      mutate(variable = gsub('_', ' - ', variable))%>%
      mutate(variable = factor(variable, levels = unique(variable))) %>%
      ggplot()+
      # geom_polygon(data = sf::st_read(water_poly_path)%>%
      #           st_crop(st_bbox(aoi_muni))%>%
      #           as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
      #         size = 0.10,
      #         fill = "#D8F1FF",
      #         colour = "#D8F1FF"
      # )+
      geom_raster(aes(x, y, fill = value))+
      facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'both', scales = 'free'
      )+
      scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
      geom_text(
        data = data.frame(
          lon = c(-0.6510470964960313,-0.5820145368849117,-0.5935578336082736),
          lat = c(5.3265635653079055,5.347582941945204,5.375997915558867),
          label = c("Muni Lagoon","Atlantic Ocean","Ayensu\nRiver"),
          angle = c(0,0,-80)
        ),
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_text(
        data = data.frame(
          lon = c(-0.630371307205293),
          lat = c(5.361990080180997),
          label = c("Winneba"),
          angle = c(0)
          ),
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_point(
        data = data.frame(
          lon = c(-0.630371307205293),
          lat = c(5.361990080180997),
          label = c("Winneba"),
          angle = c(0)
        ),
        aes(lon, lat), shape = '*', size = 4
      )+
      my_theme+
      geom_hline(yintercept = extent(aoi_muni)@ymax)+
      geom_hline(yintercept = extent(aoi_muni)@ymin)+
      geom_vline(xintercept = extent(aoi_muni)@xmax)+
      geom_vline(xintercept = extent(aoi_muni)@xmin)+
      scale_x_continuous(expand = c(0, 0))+
      scale_y_continuous(expand = c(0, 0))+
      theme(
        strip.background.y = element_blank(),
        strip.text.y = element_blank()
      ),
    
    stk %>%
      crop(aoi_densu) %>%
      gplot_data(maxpixels = maxpixels) %>%
      add_column(aoi_name = 'Densu - Kepshi Lagoon')%>%
      mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
      mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
      mutate(variable = gsub('_', ' - ', variable))%>%
      mutate(variable = factor(variable, levels = unique(variable))) %>%
      ggplot()+
      # geom_polygon(data = sf::st_read(water_poly_path)%>%
      #           st_crop(st_bbox(aoi_densu))%>%
      #           as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
      #         size = 0.10,
      #         fill = "#D8F1FF",
      #         colour = "#D8F1FF"
      # )+
      geom_raster(aes(x, y, fill = value))+
      facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'both', scales = 'free'
      )+
      scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
      geom_text(
        data = data.frame(
          lon = c(-0.13700229596279812,-0.21424991559170437, -0.219691928134425, -0.31916969607876),
          lat = c(5.553870898096414,5.515939750823568, 5.53331551450448, 5.53690359483003),
          label = c("Kpeshi\nLagoon","Atlantic Ocean", "Korle Lagoon", "Densu\nRiver"),
          angle = c(15,0, 30, 0)
          ),
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_text(
        data = data.frame(
          lon = c(-0.23735159228847724),
          lat = c(5.554950453973257),
          label = c("Sabon Zongo (Accra)"),
          angle = c(0)
        )
        ,
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_point(
        data = data.frame(
          lon = c(-0.23735159228847724),
          lat = c(5.554950453973257),
          label = c("Sabon Zongo (Accra)"),
          angle = c(0)
        ),
        aes(lon, lat), shape = '*', size = 4
      )+
      my_theme+
      geom_hline(yintercept = extent(aoi_densu)@ymax)+
      geom_hline(yintercept = extent(aoi_densu)@ymin)+
      geom_vline(xintercept = extent(aoi_densu)@xmax)+
      geom_vline(xintercept = extent(aoi_densu)@xmin)+
      scale_x_continuous(expand = c(0, 0))+
      scale_y_continuous(expand = c(0, 0))+
      theme(
        strip.background.y = element_blank(),
        strip.text.y = element_blank()
      ),
    
    stk %>%
      crop(aoi_laiwi) %>%
      gplot_data(maxpixels = maxpixels) %>%
      add_column(aoi_name = 'Laiwi - Moyo Lagoon')%>%
      mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
      mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
      mutate(variable = gsub('_', ' - ', variable))%>%
      mutate(variable = factor(variable, levels = unique(variable))) %>%
      ggplot()+
      # geom_polygon(data = sf::st_read(water_poly_path)%>%
      #           st_crop(st_bbox(aoi_laiwi))%>%
      #           as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
      #         size = 0.10,
      #         fill = "#D8F1FF",
      #         colour = "#D8F1FF"
      # )+
      geom_raster(aes(x, y, fill = value))+
      facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'both', scales = 'free'
      )+
      scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
      geom_text(
        data = data.frame(
          lon = c(0.07703279283794995,0.21406148699078198,0.15217756059918042),
          lat = c(5.699291421576897,5.75070377133814,5.7124438579498715),
          label = c('Laiwi Lagoon',"Moyo Lagoon","Atlantic Ocean"),
          angle = c(0,0,0)
        ),
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_text(
        data = data.frame(
          lon = c(0.160202729971739),
          lat = c(5.736783608634826),
          label = c("New Ningo"),
          angle = c(0)
        ),
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_point(
        data = data.frame(
          lon = c(0.160202729971739),
          lat = c(5.736783608634826),
          label = c("New Ningo"),
          angle = c(0)
        ),
        aes(lon, lat), shape = '*', size = 4
      )+
      my_theme+
      geom_hline(yintercept = extent(aoi_laiwi)@ymax)+
      geom_hline(yintercept = extent(aoi_laiwi)@ymin)+
      geom_vline(xintercept = extent(aoi_laiwi)@xmax)+
      geom_vline(xintercept = extent(aoi_laiwi)@xmin)+
      scale_x_continuous(expand = c(0, 0))+
      scale_y_continuous(expand = c(0, 0))+
      theme(
        strip.background.y = element_blank(),
        strip.text.y = element_blank()
      ),
    
    stk %>%
      crop(aoi_keta) %>%
      gplot_data(maxpixels = maxpixels) %>%
      add_column(aoi_name = 'East Coasts')%>%
      mutate(aoi_name = factor(aoi_name, levels = unique(aoi_name))) %>%
      mutate(variable = gsub('mangrove_change_magnitude_', '', variable))%>%
      mutate(variable = gsub('_', ' - ', variable))%>%
      mutate(variable = factor(variable, levels = unique(variable))) %>%
      ggplot()+
      # geom_polygon(data = sf::st_read(water_poly_path)%>%
      #           st_crop(st_bbox(aoi_keta))%>%
      #             as('Spatial')%>%fortify(), aes(x = long, y = lat, group = group),
      #         size = 0.10,
      #         fill = "#D8F1FF",
      #         colour = "#D8F1FF"
      # )+
      geom_raster(aes(x, y, fill = value))+
      facet_grid(rows = vars(variable), cols = vars(aoi_name), switch = 'both', scales = 'free'
      )+
      scale_fill_gradientn(colours = myMagnitudePalette, na.value=NA) +
      geom_text(
        data = data.frame(
        lon = c(0.9421122710843255,1.0075683690740078,0.50,0.6498423558904141,0.6108670751286953),
        lat = c(5.940164152749496,5.828306581471259,5.799615817251133,5.811102077266027,5.960511339004582),
        label = c("Keta Lagoon","Atlantic Ocean","Lake Songaw Lagoon","volta Estuary","Volta River"),
        angle = c(0,90,0,-57,-50)),
        aes(lon, lat, label = label, angle = angle), size = 1
        )+
      geom_text(
        data = data.frame(
          lon = c(0.9550399877263516),
          lat = c(5.818060047934325),
          label = c("Woe"),
          angle = c(0)
        ),
        aes(lon, lat, label = label, angle = angle), size = 1
      )+
      geom_point(
        data = data.frame(
          lon = c(0.9550399877263516),
          lat = c(5.818060047934325),
          label = c("Woe"),
          angle = c(0)
        ),
        aes(lon, lat), shape = '*', size = 4
      )+
      my_theme+
      geom_hline(yintercept = extent(aoi_keta)@ymax)+
      geom_hline(yintercept = extent(aoi_keta)@ymin)+
      geom_vline(xintercept = extent(aoi_keta)@xmax)+
      geom_vline(xintercept = extent(aoi_keta)@xmin)+
      scale_x_continuous(expand = c(0, 0))+
      scale_y_continuous(expand = c(0, 0))+
      theme(
        strip.background.y = element_blank(),
        strip.text.y = element_blank()
      ), nrow=1
  ), ncol = 1, heights = c(1, 0.5)) %>%
  ggsave(filename = 'figures/test2.png', device = 'png', dpi = 300, units = 'mm', width = 337, height = 229+114)

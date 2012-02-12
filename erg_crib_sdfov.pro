
;Do not need to do below again if the data have already been loaded.
timespan, '2010-01-01' & erg_load_sdfit, site='ksr bks rkn'
timespan, '2007-06-21' & erg_load_sdfit, site='hok'

sd_map_set, /geo_plot, center_glat=89., center_glon=0., /erase

overlay_map_coast, /geo
;;overlay_map_earth_image, /geo,time=!sdarn.sd_polar.plot_time
overlay_map_sdfov, site='hok bks ksr',/geo


end




PRO sd_map_set, time, erase=erase, clip=clip

  npar = N_PARAMS()
  IF npar LT 1 THEN time = !sdarn.sd_polar.plot_time
  
  ;Calculate the rotation angle of the north dir in a polar plot
  ;ts = time_struct(time)
  ;aacgm_conv_coord, 60., 0., 400., mlat,mlon,err, /TO_AACGM
  ;mlt = aacgm_mlt( ts.year, long((ts.doy-1)*86400.+ts.sod), mlon)
  
  ;Set the lat-lon canvas and draw the continents
  map_set, 89., 0., 0,/satellite, sat_p=[6.6, 0., 0.], scale=50e+6, $
    /isotropic, /horizon, noerase=~KEYWORD_SET(erase)
  ;map_continents, /coast
  map_grid, latdel=10., londel=15.
  
  RETURN
END

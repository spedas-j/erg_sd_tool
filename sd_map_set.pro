PRO sd_map_set, time, erase=erase, clip=clip, $
  center_glat=glatc, center_glon=glonc, lonlab=lonlab, position=position

  npar = N_PARAMS()
  IF npar LT 1 THEN time = !sdarn.sd_polar.plot_time
  
  if keyword_set(glatc) or keyword_set(glonc) then begin
    glonc = (glonc+360.) mod 360.
    if glonc gt 180. then glonc -= 360. 
  endif else begin
    glatc = 89. & glonc = 0.
  endelse
  
  aacgm_conv_coord, glatc, glonc,0.1, mlatc,mlonc,err, /TO_AACGM
  ts = time_struct(time) & yrsec = (ts.doy-1)*86400L + long(ts.sod)
  mltc = ( aacgm_mlt(ts.year, yrsec, mlonc) + 24. ) mod 24.
  mltc_lon = 360./24.* mltc
  
  rot_angle = (-mltc_lon +360.) mod 360. 
  if rot_angle gt 180. then rot_angle -= 360.
  
  if keyword_set(clip) then scale=30e+6 else scale=50e+6
  
  ;Calculate the rotation angle of the north dir in a polar plot
  ;ts = time_struct(time)
  ;aacgm_conv_coord, 60., 0., 400., mlat,mlon,err, /TO_AACGM
  ;mlt = aacgm_mlt( ts.year, long((ts.doy-1)*86400.+ts.sod), mlon)
  
  if keyword_set(position) then begin
    pre_pos = !p.position
    !p.position = position
  endif
  
  ;Set the lat-lon canvas and draw the continents
  map_set, mlatc, mltc_lon, rot_angle, $
    /satellite, sat_p=[6.6, 0., 0.], scale=scale, $
  ;map_set, 80., 0., 0,/satellite, sat_p=[2.6, 0., 0.], scale=30e+6, $
    /isotropic, /horizon, noerase=~KEYWORD_SET(erase)
  ;map_continents, /coast
  
  map_grid, latdel=10., londel=15.
  
  ;Write the MLT labels
  lons = 15.*findgen(24)
  ori = lons + 90 & ori[where(ori gt 180)] -= 360.
  
  idx=where(lons gt 180. ) & lons[idx] -= 360.
  lonnames=['00hMLT','','02hMLT','','04hMLT','','06hMLT','','08hMLT','','10hMLT','','12hMLT','', $
            '14hMLT','','16hMLT','','18hMLT','','20hMLT','','22hMLT','']
  if ~keyword_set(lonlab) then lonlab = 80.
  for i=0,n_elements(lons)-1 do begin
    nrmcord = convert_coord(lons[i],lonlab,/data,/to_normal)
    if nrmcord[0] le 0. or nrmcord[0] ge 1. or $
      nrmcord[1] le 0. or nrmcord[1] ge 1. then continue
    xyouts, lons[i], lonlab, lonnames[i], orientation=ori[i], font=1, charsize=1.4
    
  endfor
  
  !p.position = pre_pos
  
  RETURN
END

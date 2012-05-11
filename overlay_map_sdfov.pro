PRO overlay_map_sdfov, site=site, force_nhemis=force_nhemis, $
    geo_plot=geo_plot, linestyle=linestyle
    
  ;Set the list of the available sites
  valid_sites = [ 'hok','ksr','sye','sys','bks','rkn','unw','tig', $
    'kod','inv','han','pyk', 'cve', 'cvw', 'fhe', 'fhw' ]
  
  ;Check the site name
  IF ~KEYWORD_SET(site) THEN BEGIN
    PRINT, 'Keyword SITE should be given'
    RETURN
  ENDIF
  stns = thm_check_valid_name( site, valid_sites, /ignore_case, /include_all )
  IF STRLEN(stns[0]) EQ 0 THEN BEGIN
    PRINT, 'No valid radar name in sites!'
    PRINT, 'Data currently available: ',valid_sites
    RETURN
  ENDIF
  
  ;The loop to draw fovs of multiple stations
  FOR i=0, N_ELEMENTS(stns)-1 DO BEGIN
    stn = stns[i]
    ptbl_vn = tnames('sd_'+STRLOWCASE(stn)+'_position_tbl_?')
    IF STRLEN(ptbl_vn) LT 10 THEN CONTINUE
    
    
    ;Load the position table
    get_data, ptbl_vn,data=d
    glat=REFORM(d.y[0,*,*,1])  ;--> array size [76,17]
    glon=REFORM(d.y[0,*,*,0])
    alt = glat & alt[*] = 400 ;km
    
    IF ~KEYWORD_SET(geo_plot) THEN BEGIN
      aacgmconvcoord,glat,glon,alt,mlat,mlon,err,/TO_AACGM
      mlon = (mlon + 360.) MOD 360.
      ts = time_struct(!sdarn.sd_polar.plot_time)
      yrsec = LONG( alt )
      yrsec[*] = LONG((ts.doy-1)*86400L + ts.sod)
      yr = yrsec & yr[*] = ts.year
      
      tmlt = aacgmmlt(yr, yrsec, mlon)
      tmlt = ( (tmlt + 24. ) MOD 24. ) /24.*360.  ;[deg]
      
      ;Forcibly draw in the northern hemisphere
      if keyword_set(force_nhemis) then mlat = abs(mlat)
      
    ENDIF ELSE BEGIN
      mlat = glat & tmlt = glon
    ENDELSE
    
    n_rg = N_ELEMENTS(mlat[*,0])-1
    n_az = N_ELEMENTS(mlat[0,*])-1
    
    PLOTS,tmlt[0,0:n_az],mlat[0,0:n_az], linestyle=linestyle
    PLOTS,tmlt[0:n_rg,n_az],mlat[0:n_rg,n_az], linestyle=linestyle
    PLOTS,tmlt[n_rg,0:n_az],mlat[n_rg,0:n_az], linestyle=linestyle
    PLOTS,tmlt[0:n_rg,0],mlat[0:n_rg,0], linestyle=linestyle
    
  ENDFOR
  
  RETURN
END

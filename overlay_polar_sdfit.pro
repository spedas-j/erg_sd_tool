;+
; :DESCRIPTION:
;    Describe the procedure.
;
; :PARAMS:
;    datvn
;
; :KEYWORDS:
;    time
;
; :AUTHOR: T. Hori
; :HISTORY:
; 	2010/03/09: Created
;-
PRO overlay_polar_sdfit, datvn, time=time, position=position, $
  erase=erase, clip=clip, geo_plot=geo_plot, $
  nogscat=nogscat

  ;Initialize SDARN system variable
  sd_init
  
  ;Check argument and keyword
  npar=N_PARAMS()
  IF npar LT 1 THEN RETURN
  IF ~KEYWORD_SET(time) THEN BEGIN
    t0 = !sdarn.sd_polar.plot_time
    get_timespan, tr
    if t0 ge tr[0] and t0 le tr[1] then time = t0 else begin
      time = (tr[0]+tr[1])/2.  ; Take the center of the designated time range
    endelse
  ENDIF
  
  ;if datvn is the index number for tplot var
  IF SIZE(datvn[0], /type) EQ 2 OR SIZE(datvn[0], /type) EQ 3 THEN BEGIN
    datvn = tnames(datvn[0])
  ENDIF
  ;then datvn should be a string now, which is checked below
  IF SIZE(datvn[0], /type) NE 7 THEN RETURN
  IF N_ELEMENTS(datvn) EQ 0 THEN RETURN
  
  ;get the radar name and the suffix
  stn = STRMID(datvn, 3,3)
  suf = STRMID(datvn, 0,1,/reverse)
  
  ;Load the data to be drawn and to be used for drawing on a 2-d map
  get_data, datvn, data=d, dl=dl, lim=lim
  get_data, 'sd_'+stn+'_azim_no_'+suf, data=az
  get_data, 'sd_'+stn+'_position_tbl_'+suf, data=tbl
  get_data, 'sd_'+stn+'_scanstartflag_'+suf, data=stflg
  get_data, 'sd_'+stn+'_scanno_'+suf, data=scno
  if strlen(tnames('sd_'+stn+'_echo_flag_'+suf)) gt 6 then begin
    get_data, 'sd_'+stn+'_echo_flag_'+suf, data=echflg
  endif else begin
    print, 'Cannot find the echo_flag data, which should be loaded in advance'
    return
  endelse
  
  ;Choose data for the time given by keyword
  idx = nn( scno.x, time_double(time) )
  bmno = WHERE( scno.y EQ scno.y[idx] )
  
  ;;for debugging
  PRINT, 'time: '+time_string(time)
  PRINT, 'time by nn: '+time_string(scno.x[idx])
  ;print, 'scan no: ',scno.y[idx]
  ;print, 'beam no:', bmno
  ;print, 'scan time: '+time_string(min(scno.x[bmno]))+' -- '+time_string(max(scno.x[bmno]))
  ;;
  
  ;Set the range of the plotted values
  str_element, lim, 'zrange', val, success=s
  IF s EQ 1 THEN valrng = val ELSE valrng=[-1000.,1000]
  
  ;Set color level for contour
  clmax = !d.table_size-1
  clmin = 8L
  cnum = clmax-clmin
  
  
  ;Set the plot position
  IF KEYWORD_SET(position) THEN BEGIN
    pre_position = !p.position
    !p.position = position
  ENDIF
  
  ;Set the lat-lon canvas 
  sd_map_set, erase=erase 
  
  ;Set the SD color table
  loadct_sd, previous_ct=prevct, /reverse
  
  ;Draw the data
  FOR i=0L, N_ELEMENTS(bmno)-1 DO BEGIN
    
    bn = bmno[i]
    valarr = REFORM(d.y[bn, *, 0])
    echflgarr = REFORM(echflg.y[bn,*])
    rgmax = N_ELEMENTS(valarr)
    azno = az.y[bn]
    tblidx = MAX(WHERE(tbl.x LE d.x[bn], cnt))
    IF tblidx EQ -1 THEN BEGIN
      PRINT, 'beam time does not fall in any time range of the position table!'
      loadct2, prevct ;Resotre the original color table before returing
      RETURN
    ENDIF
    pos = REFORM(tbl.y[tblidx,*,azno:(azno+1),*])
    
    ;For plotting in GEO
    pos_plt = pos 
    
    ;Convert to AACGM 
    if ~keyword_set(geo_plot) then begin
      ts = time_struct(time)
      year = ts.year & yrsec = long((ts.doy-1)*86400. + ts.sod)
      glat = reform(pos[*,*,1]) & glon = reform((pos[*,*,0]+360.) mod 360.) 
      hgt = glat & hgt[*,*] = 400.
      year_arr = long(glat) & year_arr[*,*] = year
      yrsec_arr= long(glat) & yrsec_arr[*,*] = yrsec 
      aacgm_conv_coord, glat,glon,hgt, mlat,mlon,err,/TO_AACGM
      mlt_arr = aacgm_mlt( year_arr, yrsec_arr, mlon )
      plt_lon = ( (mlt_arr + 24.) mod 24. ) * 180./12.
      
      pos_plt = pos ;replicate as an array with same numbers of elements
      pos_plt[*,*,0] = plt_lon
      pos_plt[*,*,1] = mlat
    endif
    
    FOR j=0, rgmax-1 DO BEGIN
      val = valarr[j]
      IF ~FINITE(val) THEN CONTINUE ;Skip drawing for NaN
      
      ;Color level for val
      if fix(echflgarr[j]) eq 1 then begin
        clvl = clmin + cnum*(val-valrng[0])/(valrng[1]-valrng[0])
        clvl = (clvl > clmin)
        clvl = (clvl < clmax) ; clmin <= color level <= clmax
      endif else begin
        if keyword_set(nogscat) then continue ;skip plotting ground scatter
      endelse
      
      ;Lon and Lat for a square to be filled
      lon = [ pos_plt[j,0,0], pos_plt[j,1,0], pos_plt[j+1,1,0], pos_plt[j+1,0,0] ]
      lat = [ pos_plt[j,0,1], pos_plt[j,1,1], pos_plt[j+1,1,1], pos_plt[j+1,0,1] ]
      
      ;Draw
      if fix(echflgarr[j]) eq 1 then begin
        POLYFILL, lon, lat, color=clvl
      endif else begin
        POLYFILL, lon, lat, color=5 ;grey in the color table by loadct_sd
      endelse
      
    ENDFOR
    
  ENDFOR
  
  ;Restore the original color table
  loadct2, prevct
  
  ;Resotre the original plot position
  IF ~KEYWORD_SET(pre_position) THEN pre_position=0
  !p.position = pre_position
  
  ;Normal end
  RETURN
  
END


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
PRO overlay_polar_sdfit, datvn, time=time, position=position, erase=erase, clip=clip

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
  ;then datvn should a string now
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
  
  ;Set the lat-lon canvas and draw the continents
  map_set, 70., 190., 0,/satellite, sat_p=[6.6, 0., 80.], scale=40e+6, $
    isotropic=0, /horizon, noerase=~KEYWORD_SET(erase)
  map_continents, /coast
  map_grid, latdel=10., londel=15.
  
  ;Draw the data
  FOR i=0L, N_ELEMENTS(bmno)-1 DO BEGIN
    bn = bmno[i]
    valarr = REFORM(d.y[bn, *, 0])
    rgmax = N_ELEMENTS(valarr)
    azno = az.y[bn]
    tblidx = MAX(WHERE(tbl.x LE d.x[bn], cnt))
    IF tblidx EQ -1 THEN BEGIN
      PRINT, 'beam time does not fall in any time range of the position table!'
      RETURN
    ENDIF
    pos = REFORM(tbl.y[tblidx,*,azno:(azno+1),*])
    ;;;;;;;;;;;;;;;;;;;;;;
    ;; The routine to convert pos to GEO has been inserted temorarily.
    ;; This part should be removed as soon as the bug in make_sd_fitacf_cdf_file.pro
    ;; is fixed.
    ;  for k=0L, n_elements(pos[*,0,0])-1 do begin
    ;    for l=0L, n_elements(pos[0,*,0])-1 do begin
    ;      ts = time_struct(tbl.x[0])
    ;      yrsec = long(ts.doy*86400. + ts.sod)
    ;      aacgm_load_coef, 2005
    ;      aacgm_conv_coord, (pos[k,l,1]), (pos[k,l,0]+360.) mod 360., $
    ;        400., glat, glon, err, /TO_GEO
    ;      pos[k,l,1] = glat & pos[k,l,0] = glon
    ;    endfor
    ;  endfor
    ;;;;;;;;;;;;;;;;;;;;;;
    
    FOR j=0, rgmax-1 DO BEGIN
      val = valarr[j]
      IF ~FINITE(val) THEN CONTINUE ;Skip drawing for NaN
      
      ;Color level for val
      clvl = clmin + cnum*(val-valrng[0])/(valrng[1]-valrng[0])
      clvl = (clvl > clmin)
      clvl = (clvl < clmax) ; clmin <= color level <= clmax
      
      ;Lon and Lat for a square to be filled
      lon = [ pos[j,0,0], pos[j,1,0], pos[j+1,1,0], pos[j+1,0,0] ]
      lat = [ pos[j,0,1], pos[j,1,1], pos[j+1,1,1], pos[j+1,0,1] ]
      
      ;Draw
      POLYFILL, lon, lat, color=clvl
      
    ENDFOR
    
  ENDFOR
  
  
  ;Resotre the original plot position
  IF ~KEYWORD_SET(pre_position) THEN pre_position=0
  !p.position = pre_position
  
  ;Normal end
  RETURN
  
END


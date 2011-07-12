;+
;	PROCEDURE overlay_map_sdfit
;
; :DESCRIPTION:
;    Plot a 2-D scan data of a SD radar on the plot window set up by map_set.
;
; :PARAMS:
;    datvn:   tplot variable names (as strings) to be plotted
;
; :KEYWORDS:
;    time:    Set the time (UNIX time) to plot a 2-D scan for
;    position:  Set the location of the plot frame in the plot window
;    erase:   Set to forcibly erase the plot window before plotting data
;    clip:    Set to scale in to get a magnified map
;    geo_plot:  Set to plot in the geographical coordinates
;    nogscat: Set to prevent the ground scatter data from appearing on the plot
;    notimelabel: Set to prevent the time label from appearing on the plot
;    nocolorscale: Set to surpress drawing the color scale 
;    colorscalepos: Set the position of the color scale in the noraml 
;                   coordinates. Default: [0.85, 0.1, 0.87, 0.45] 
;
; :AUTHOR:
; 	Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
;
; :HISTORY:
; 	2011/01/11: Created
; 	2011/06/15: renamed to overlay_map_sdfit
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $;
;-
PRO overlay_map_sdfit, datvn, time=time, position=position, $
    erase=erase, clip=clip, geo_plot=geo_plot, $
    nogscat=nogscat, gscatmaskoff=gscatmaskoff, $
    notimelabel=notimelabel, $
    nocolorscale=nocolorscale, colorscalepos=colorscalepos, $
    charscale=charscale
    
  ;Initialize SDARN system variable and get the default charsize
  sd_init
  
  ;Size of characters
  if ~keyword_set(charscale) then charscale=1.0
  charsz = !sdarn.sd_polar.charsize * charscale
  
  ;Check argument and keyword
  npar=N_PARAMS()
  IF npar LT 1 THEN RETURN
  IF ~KEYWORD_SET(time) THEN BEGIN
    t0 = !sdarn.sd_polar.plot_time
    get_timespan, tr
    IF t0 GE tr[0] AND t0 LE tr[1] THEN time = t0 ELSE BEGIN
      time = (tr[0]+tr[1])/2.  ; Take the center of the designated time range
    ENDELSE
  ENDIF
  
  ;if datvn is the index number for tplot var
  datvn = tnames(datvn)
  IF datvn EQ '' THEN BEGIN
    PRINT, 'Given tplot var(s) does not exist?'
    RETURN
  ENDIF
  
  ;Loop for processing multiple arguments
  tmp_datvn = datvn
  FOR nv=0L, N_ELEMENTS(tmp_datvn)-1 DO BEGIN
  
    datvn = tmp_datvn[nv]
    
    ;get the radar name and the suffix
    stn = STRMID(datvn, 3,3)
    suf = STRMID(datvn, 0,1,/reverse)
    
    ;Load the data to be drawn and to be used for drawing on a 2-d map
    get_data, datvn, data=tmp_d, dl=dl, lim=lim
    ;;if (size(d))[2] ne 8 then get_data, d[0], data=d, dl=dl, lim=lim ;For multi-tplot vars
    
    ;Loop for processing a multi-tplot vars
    FOR n=0L, N_ELEMENTS(tmp_d)-1 DO BEGIN
    
      d = tmp_d[n]
      ;For multi-tplot variable case
      IF (SIZE(d))[2] EQ 1 THEN get_data, tmp_d[n], data=d, dl=dl, lim=lim
      
      ;Get "fill_color" attribute if exists as well as the other 
      ;necessary variables
      str_element, dl, 'fill_color', val=fill_color, success=s
      if s eq 0 then fill_color = -1
      
      get_data, 'sd_'+stn+'_azim_no_'+suf, data=az
      get_data, 'sd_'+stn+'_position_tbl_'+suf, data=tbl
      get_data, 'sd_'+stn+'_scanstartflag_'+suf, data=stflg
      get_data, 'sd_'+stn+'_scanno_'+suf, data=scno
      IF STRLEN(tnames('sd_'+stn+'_echo_flag_'+suf)) GT 6 THEN BEGIN
        get_data, 'sd_'+stn+'_echo_flag_'+suf, data=echflg
      ENDIF ELSE BEGIN
        PRINT, 'Cannot find the echo_flag data, which should be loaded in advance'
        RETURN
      ENDELSE
      
      ;Choose data for the time given by keyword
      idx = nn( scno.x, time_double(time) )
      bmno = WHERE( scno.y EQ scno.y[idx] )
      
      ;;for debugging
      PRINT, '    time by sd_time: '+time_string(time)
      PRINT, 'selected time frame: '+time_string(scno.x[idx])
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
      pre_position = !p.position
      IF KEYWORD_SET(position) THEN BEGIN
        !p.position = position
      ENDIF ELSE position = !p.position
      
      ;Set the lat-lon canvas
      ;sd_map_set, erase=erase
      
      ;Set the SD color table
      ;loadct_sd, 44, previous_ct=prevct
      
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
          ;loadct2, prevct ;Resotre the original color table before returing
          RETURN
        ENDIF
        pos = REFORM(tbl.y[tblidx,*,azno:(azno+1),*])
        
        ;For plotting in GEO
        pos_plt = pos
        
        ;Convert to AACGM
        IF ~KEYWORD_SET(geo_plot) THEN BEGIN
          ts = time_struct(time)
          year = ts.year & yrsec = LONG((ts.doy-1)*86400. + ts.sod)
          glat = REFORM(pos[*,*,1]) & glon = REFORM((pos[*,*,0]+360.) MOD 360.)
          hgt = glat & hgt[*,*] = 400.
          year_arr = LONG(glat) & year_arr[*,*] = year
          yrsec_arr= LONG(glat) & yrsec_arr[*,*] = yrsec
          aacgmconvcoord, glat,glon,hgt, mlat,mlon,err,/TO_AACGM
          if (size(mlat))[0] eq 0 then begin ; For Unix ver. AACGM DLM bug 
            mlat = reform(mlat, n_elements(glat[*,0]), n_elements(glat[0,*]) )
            mlon = reform(mlon, n_elements(glat[*,0]), n_elements(glat[0,*]) )
          endif
          mlt_arr = aacgmmlt( year_arr, yrsec_arr, mlon )
          if (size(mlt_arr))[0] eq 0 then begin ; For Unix ver. AACGM DLM bug 
            mlt_arr = reform(mlt_arr, n_elements(mlon[*,0]), n_elements(mlon[0,*]) )
          endif
          plt_lon = ( (mlt_arr + 24.) MOD 24. ) * 180./12.
          
          pos_plt = pos ;replicate as an array with same numbers of elements
          pos_plt[*,*,0] = plt_lon
          pos_plt[*,*,1] = mlat
        ENDIF
        
        FOR j=0, rgmax-1 DO BEGIN
          val = valarr[j]
          IF ~FINITE(val) THEN CONTINUE ;Skip drawing for NaN
          
          ;Color level for val
          clvl = clmin + cnum*(val-valrng[0])/(valrng[1]-valrng[0])
          clvl = (clvl > clmin)
          clvl = (clvl < clmax) ; clmin <= color level <= clmax
          IF FIX(echflgarr[j]) ne 1 AND strpos(datvn,'_pwr') lt 0 $
            AND strpos(datvn,'spec_width') lt 0 THEN BEGIN
            ;ground echo case
            IF KEYWORD_SET(nogscat) THEN CONTINUE ;skip plotting if nogscat keyword i set
            if ~keyword_set(gscatmaskoff) then begin
              if fill_color ge 0 then clvl = fill_color else clvl=5 
            endif
          ENDIF
          
          ;Lon and Lat for a square to be filled
          lon = [ pos_plt[j,0,0], pos_plt[j,1,0], pos_plt[j+1,1,0], pos_plt[j+1,0,0] ]
          lat = [ pos_plt[j,0,1], pos_plt[j,1,1], pos_plt[j+1,1,1], pos_plt[j+1,0,1] ]
          
          ;Draw the pixel for a range gate in a beam 
          POLYFILL, lon, lat, color=clvl  
          
        ENDFOR ; for j
        
      ENDFOR ; for i
      
    ENDFOR ;End of the loop for multi-tplot var
    
  ENDFOR ;End of the loop for multi arguments
  
  
  ;Time label
  IF ~KEYWORD_SET(notimelabel) THEN BEGIN
    t = time
    tstr = time_string(t, tfor='hh:mm')+' UT'
    XYOUTS, !x.window[0]+0.02, !y.window[0]+0.02, tstr, /normal, $
      font=1, charsize=charsz*2.5
  ENDIF
  
  ;Color scale
  if ~keyword_set(nocolorscale) then begin
    str_element, lim, 'ztitle', val=ztitle, success=s
    if s eq 0 then ztitle = ''
    str_element, lim, 'zrange', val=zrange, success=s
    if s eq 0 then zrange = [-1000,1000]
    if keyword_set(colorscalepos) then cspos=colorscalepos $
      else cspos = [0.85,0.1,0.87,0.45]
    
    draw_color_scale, range=zrange,$
      pos=cspos,$
      title=ztitle
  endif
  
  
  ;Resotre the original plot position
  IF ~KEYWORD_SET(pre_position) THEN pre_position=0
  !p.position = pre_position
  
  ;Normal end
  RETURN
  
END


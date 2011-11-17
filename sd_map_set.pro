;+
; PROCEDURE/FUNCTION sd_map_set
;
; :DESCRIPTION:
;		A wrapper routine for the IDL original "map_set" enabling some
;		annotations regarding the visualization of SD data.
;
;	:PARAMS:
;    time:   time (in double Unix time) for which the magnetic local time for the
;            world map is calculated. In AACGM plots, the magnetic local noon comes
;            on top in plot.
;
;	:KEYWORDS:
;    erase:   set to erase pre-existing graphics on the plot window.
;    clip:    set to zoom in roughly to a region encompassing a field of view of one radar.
;             Actually 30e+6 (clip is on) or 50e+6 (off) is put is "scale" keyword of map_set.
;    position:  gives the position of a plot panel on the plot window as the normal coordinates.
;    center_glat: geographical latitude at which a plot region is centered.
;    center_glon: geographical longitude at which a plot region is centered.
;                 (both center_glat and center_glon should be given, otherwise ignored)
;    mltlabel:    set to draw the MLT labels every 2 hour.
;    lonlab:      a latitude from which (toward the poles) the MLT labels are drawn.
;    force_scale:   Forcibly put a given value in "scale" of map_set.
;    stereo: Use the stereographic mapping, instead of satellite mapping (default)
;
; :EXAMPLES:
;    sd_map_set
;    sd_map_set, /clip, center_glat=70., center_glon=180., /mltlabel, lonlab=74.
;
; :AUTHOR:
; 	Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
;
; :HISTORY:
; 	2011/01/11: Created
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-
PRO sd_map_set, time, erase=erase, clip=clip, position=position, $
    center_glat=glatc, center_glon=glonc, $
    mltlabel=mltlabel, lonlab=lonlab, $
    force_scale=force_scale, $
    geo_plot=geo_plot, $
    stereo=stereo, $
    charscale=charscale
    
  ;Initialize the SD plot environment
  sd_init
  
  npar = N_PARAMS()
  IF npar LT 1 THEN time = !sdarn.sd_polar.plot_time
  
  IF KEYWORD_SET(glatc) OR KEYWORD_SET(glonc) THEN BEGIN
    glonc = (glonc+360.) MOD 360.
    IF glonc GT 180. THEN glonc -= 360.
  ENDIF ELSE BEGIN
    glatc = 89. & glonc = 0.
  ENDELSE
  
  ;Hemisphere flag
  IF glatc GT 0 THEN hemis = 1 ELSE hemis = -1
  
  ;Calculate the rotation angle regarding MLT
  IF ~KEYWORD_SET(geo_plot) THEN BEGIN
    aacgmconvcoord, glatc, glonc,0.1, mlatc,mlonc,err, /TO_AACGM
    ts = time_struct(time) & yrsec = (ts.doy-1)*86400L + LONG(ts.sod)
    tmltc = aacgmmlt(ts.year, yrsec, mlonc)
    mltc = ( tmltc + 24. ) MOD 24.
    mltc_lon = 360./24.* mltc
    
    rot_angle = (-mltc_lon +360.) MOD 360.
    IF rot_angle GT 180. THEN rot_angle -= 360.
    
    ;Rotate oppositely for the S. hemis.
    if hemis lt 0 then begin 
      ;rot_angle = ( rot_angle + 180. ) mod 360.
      ;rot_angle *= (-1.)
      rot_angle = (rot_angle+360.) mod 360.
      if rot_angle gt 180. then rot_angle -= 360.
    endif
  ENDIF ELSE rot_angle = 0.
  
  ;Calculate the rotation angle of the north dir in a polar plot
  ;ts = time_struct(time)
  ;aacgm_conv_coord, 60., 0., 400., mlat,mlon,err, /TO_AACGM
  ;mlt = aacgm_mlt( ts.year, long((ts.doy-1)*86400.+ts.sod), mlon)
  
  ;Set the plot position
  pre_pos = !p.position
  IF KEYWORD_SET(position) THEN BEGIN
    !p.position = position
  ENDIF ELSE BEGIN
    nopos = 1
    position = !p.position
  ENDELSE
  IF position[0] GE position[2] OR position[1] GE position[3] THEN BEGIN
    PRINT, '!p.position is not set, temporally use [0,0,1,1]'
    position = [0.,0.,1.,1.]
  ENDIF
  
  ;Set the scale for drawing the map_set canvas
  IF KEYWORD_SET(clip) THEN scale=30e+6 ELSE scale=50e+6
  IF KEYWORD_SET(force_scale) THEN scale = force_scale
  
  ;Resize the canvas size for the position values
  IF ~KEYWORD_SET(nopos) THEN BEGIN
    scl = (position[2]-position[0]) < (position[3]-position[1])
  ENDIF ELSE BEGIN
    scl = 1.
    IF !x.window[1]-!x.window[0] GT 0. THEN $
      scl = (!x.window[1]-!x.window[0]) < (!y.window[1]-!y.window[0])
  ENDELSE
  scale /= scl
  
  
  ;Set the lat-lon canvas and draw the continents
  IF ~KEYWORD_SET(geo_plot) THEN BEGIN
    IF ~KEYWORD_SET(stereo) THEN BEGIN
      map_set, mlatc, mltc_lon, rot_angle, $
        /satellite, sat_p=[6.6, 0., 0.], scale=scale, $
        /isotropic, /horizon, noerase=~KEYWORD_SET(erase)
    ENDIF ELSE BEGIN
      map_set, mlatc, mltc_lon, rot_angle, $
        /stereo, sat_p=[6.6, 0., 0.], scale=scale, $
        /isotropic, /horizon, noerase=~KEYWORD_SET(erase)
    ENDELSE
  ENDIF ELSE BEGIN
    IF ~KEYWORD_SET(stereo) THEN BEGIN
      map_set, glatc, glonc, rot_angle, $
        /satellite, sat_p=[6.6, 0., 0.], scale=scale, $
        /isotropic, /horizon, noerase=~KEYWORD_SET(erase)
    ENDIF ELSE BEGIN
      map_set, glatc, glonc, rot_angle, $
        /stereo, sat_p=[6.6, 0., 0.], scale=scale, $
        /isotropic, /horizon, noerase=~KEYWORD_SET(erase)
    ENDELSE
  ENDELSE
  
  map_grid, latdel=10., londel=15.
  
  ;Resize the canvas size for the position values
  scl = (!x.window[1]-!x.window[0]) < (!y.window[1]-!y.window[0])
  scale /= scl
  ;Set charsize used for MLT labels and so on
  charsz = 1.4 * (KEYWORD_SET(clip) ? 50./30. : 1. ) * scl
  !sdarn.sd_polar.charsize = charsz
  
  ;Scale for characters applied only in sd_map_set
  IF ~KEYWORD_SET(charscale) THEN charscale=1.0
  
  IF KEYWORD_SET(mltlabel) THEN BEGIN
    ;Write the MLT labels
    lons = 15.*FINDGEN(24)
    ori = lons + 90 & ori[WHERE(ori GT 180)] -= 360.
    
    idx=WHERE(lons GT 180. ) & lons[idx] -= 360.
    lonnames=['00hMLT','','02hMLT','','04hMLT','','06hMLT','','08hMLT','','10hMLT','','12hMLT','', $
      '14hMLT','','16hMLT','','18hMLT','','20hMLT','','22hMLT','']
    IF ~KEYWORD_SET(lonlab) THEN lonlab = 77.
    FOR i=0,N_ELEMENTS(lons)-1 DO BEGIN
      nrmcord = CONVERT_COORD(lons[i],lonlab,/data,/to_normal)
      pos = [!x.window[0],!y.window[0],!x.window[1],!y.window[1]]
      IF nrmcord[0] LE pos[0] OR nrmcord[0] GE pos[2] OR $
        nrmcord[1] LE pos[1] OR nrmcord[1] GE pos[3] THEN CONTINUE
      XYOUTS, lons[i], lonlab, lonnames[i], orientation=ori[i], $
        font=1, charsize=charsz*charscale
        
    ENDFOR
    
  ENDIF
  
  ;Restore the original position setting
  !p.position = pre_pos
  
  RETURN
END

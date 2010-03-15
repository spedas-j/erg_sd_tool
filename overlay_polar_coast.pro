PRO overlay_polar_coast,south=south,fill=fill,col=col,force_year=force_year, $
    force_secs=force_secs,static=static,time=time, geo_plot=geo_plot
    
  OPENR,map_unit,'C:/cygwin/home/hori/work/IDL_project/SDtool_forERG/sd_world_data',/GET_LUN
  
  IF KEYWORD_SET(south) THEN hemisphere=-1 ELSE hemisphere=1
  
  IF NOT KEYWORD_SET(col) THEN col=0
  
  IF ~KEYWORD_SET(time) THEN BEGIN
    t0 = !sdarn.sd_polar.plot_time
    get_timespan, tr
    IF t0 GE tr[0] AND t0 LE tr[1] THEN time = t0 ELSE BEGIN
      time = (tr[0]+tr[1])/2.  ; Take the center of the designated time range
    ENDELSE
  ENDIF
  
  ts = time_struct(time)
  year=ts.year
  year_secs= LONG( (ts.doy-1)*86400L + ts.sod )
  IF KEYWORD_SET(force_year) THEN year=force_year
  IF KEYWORD_SET(force_secs) THEN year_secs=force_secs
  
  no_blocks=17
  block_len=INTARR(17)
  READF,map_unit,no_blocks,block_len
  
  coast=FLTARR(2,10000) & pts=0
  FOR read_block=0,no_blocks-1 DO BEGIN
    read_coast=FLTARR(2,block_len(read_block))
    READF,map_unit,read_coast
    coast(*,pts:pts+block_len(read_block)-1)=read_coast(*,*)
    pts=pts+block_len(read_block)
  ENDFOR
  CLOSE,map_unit
  FREE_LUN,map_unit
  
  plot_coast=FLTARR(2,5000) & plot_pts=0
  FOR i=0,pts-1 DO BEGIN
    IF (coast(0,i) NE 0 OR coast(1,i) NE 0) THEN BEGIN
    
      IF coast(0,i)*hemisphere GT 0 THEN BEGIN
      
        if ~keyword_set(geo_plot) then begin  ;For plotting in AACGM
          aacgm_conv_coord,coast[0,i],coast[1,i],400.,mlat,mlon,err,/TO_AACGM
          mag_pos = [mlat, mlon]
          ;mag_pos=cnvcoord(coast(0,i),coast(1,i),1)
          
          ;;;;;;For plotting with MAP_SET (stay in polar coordinates)
          
          IF NOT KEYWORD_SET(static) THEN BEGIN
            ;x0= mlt(year,year_secs,mag_pos[1])*180/12 ;longitude [deg]
            x0 = aacgm_mlt(year,year_secs,mag_pos[1])*180./12. ; [deg]
            x0= (x0 + 360. ) MOD 360.
            y0= mag_pos[0] ;latitude [deg]
            ;x0= ABS(hemisphere*90-mag_pos(0))* $
            ;  SIN(mlt(year,year_secs,mag_pos(1))*!pi/12)
            ;y0=-ABS(hemisphere*90-mag_pos(0))* $
            ;  COS(mlt(year,year_secs,mag_pos(1))*!pi/12)
          ENDIF ELSE BEGIN
            x0= mag_pos[1] ;longitude [deg]
            x0= (x0 + 360. ) MOD 360.
            y0= mag_pos[0] ;latitude [deg]
          ;x0= ABS(hemisphere*90-mag_pos(0))* $
          ;                                    SIN(mag_pos(1)*!pi/180)
          ;                            y0=-ABS(hemisphere*90-mag_pos(0))* $
          ;                                   COS(mag_pos(1)*!pi/180)
          ENDELSE
        endif else begin   ;For plotting in GEO
          y0 = coast[0,i]
          x0 = coast[1,i]
        endelse
        
        plot_coast(*,plot_pts)=[x0,y0]
        plot_pts=plot_pts+1
        
        
      ENDIF
    ENDIF ELSE BEGIN
      IF plot_pts GT 0 THEN BEGIN
        IF KEYWORD_SET(fill) THEN BEGIN
          POLYFILL,plot_coast(0,0:plot_pts-1),plot_coast(1,0:plot_pts-1),NOCLIP=0,COL=col
        ENDIF ELSE BEGIN
          OPLOT,plot_coast(0,[INDGEN(plot_pts),0]),plot_coast(1,[INDGEN(plot_pts),0]),COL=col
        ENDELSE
      ENDIF
      plot_pts=0
    ENDELSE
  ENDFOR
  
END

;-----------------------------------------------------------------------
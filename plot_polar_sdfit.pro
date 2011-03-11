;+
; PROCEDURE plot_polar_sdfit
;
; PURPOSE:
;		Draw a fan plot of SD data on the world map
;
;	:Params:
;    var:   tplot variable to be plotted
;
;	:Keywords:
;    noerase:     Set to plot data without erasing the screen 
;    clip:        Set to scale in to get a magnified map
;    position:    Set the location of the plot frame in the plot window
;    center_glat: geographical latitude at which a plot region is centered
;    center_glon: geographical longitude at which a plot region is centered
;    mltlabel:    Set to draw the MLT labels every 2 hour
;    lonlab:      a latitude from which (toward the poles) the MLT labels are drawn
;    force_scale: Forcibly put a given value in "scale" of map_set
;    geo_plot:    Set to plot in the geographical coordinates
;    fcoast:      Set to superpose the world map on the plot
;
; :EXAMPLES:
;   plot_polar_sdfit, 'sd_hok_vlos_bothscat'
;   plot_polar_sdfit, 'sd_hok_vlos_bothscat', center_glat=70., center_glon=180. 
;   
; :Author:
; 	Tomo Hori (E-mail: horit at stelab.nagoya-u.ac.jp)
;
; :HISTORY:
; 	2011/03/11: Created
;
;-
PRO plot_polar_sdfit, var $
    , noerase=noerase $
    , clip=clip $
    , position=position $
    , center_glat=glatc $
    , center_glon=glonc $
    , mltlabel=mltlabel $
    , lonlab=lonlab $
    , force_scale=force_scale $
    , geo_plot=geo_plot $
    , fcoast=fcoast
    
    
  ;the tplot var exists?
  IF STRLEN(tnames(var)) EQ 0 THEN BEGIN
    PRINT, 'Not find the tplot variable: '+var
    RETURN
  ENDIF
  
  ;Initialize the 2D plot environment
  sd_init
  
  ;Set map_set if any map projection is not defined
  sd_map_set, erase=(~KEYWORD_SET(noerase)), clip=clip, position=position, $
    center_glat=glatc, center_glon=glonc, $
    mltlabel=mltlabel, lonlab=lonlab, $
    force_scale=force_scale, $
    geo_plot=geo_plot
    
    
  ;Draw a fan plot on map
  overlay_polar_sdfit, var, $
    position=position, $
    erase=(~KEYWORD_SET(noerase)), clip=clip, geo_plot=geo_plot, $
    nogscat=nogscat
    
  ;Draw the world map
  IF KEYWORD_SET(fcoast) THEN BEGIN
    overlay_polar_coast, geo_plot=geo_plot, position=position
  ENDIF
  
  ;Draw the color scale on the right in screen
  ;overlay_color_scale     ;to be developed soon
  
  
  
  RETURN
END


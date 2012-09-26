PRO overlay_map_precal_sdfov, site=site, geo_plot=geo_plot, nh=nh, sh=sh, $
  fill=fill, $
  color=color 
  
  ;Check the keywords and generate the station list to be plotted
  stns = ''
  if keyword_set(nh) then append_array, stns, strsplit('hok ksr', /ext )
  if keyword_set(sh) then append_array, stns, strsplit('sye sys', /ext )
  if keyword_set(site) then append_array, stns, site 
  if stns[0] eq '' then return
  print, stns 
  
  if ~keyword_set(color) then color = 0
  
  
  ;Initialize !sdarn system variable
  sd_init
  
  
  ;Obtain the directory path where overlay_map_precal_sdfov.pro and save files are located.
  stack = SCOPE_TRACEBACK(/structure)
  filename = stack[SCOPE_LEVEL()-1].filename
  dir = FILE_DIRNAME(filename)
  
  for i=0, n_elements(stns)-1 do begin
    
    stn = stns[i]
    tblfn = dir +'/sdfovtbl_'+stn+'.sav
    if ~file_test(tblfn) then continue
    restore, tblfn 
    
    bm = n_elements( sdfovtbl.glat[*,0] )-1
    rg = n_elements( sdfovtbl.glat[0,*] )-1
    
    glats = [ sdfovtbl.glat[0:bm,0], reform(sdfovtbl.glat[bm,0:rg]), $
      reverse(sdfovtbl.glat[0:bm,rg]), reverse(reform(sdfovtbl.glat[0,0:rg])) ]
    glons = [ sdfovtbl.glon[0:bm,0], reform(sdfovtbl.glon[bm,0:rg]), $
      reverse(sdfovtbl.glon[0:bm,rg]), reverse(reform(sdfovtbl.glon[0,0:rg])) ]
    
    if keyword_set(geo_plot) then begin
      lats = glats & lons = glons 
    endif else begin
    
    endelse
    
    plots, lons, lats, color=color
    
    
    
  endfor
  
  
  
  return
end

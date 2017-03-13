PRO overlay_map_precal_sdfov, site=site, geo_plot=geo_plot, nh=nh, sh=sh, $
  linethick=linethick, $
  fill=fill, $
  color=color, $
  force_nhemis=force_nhemis, force_shemis=force_shemis, $
  get_sdfovtbl = get_sdfovtbl, $ ;get_sdfovtbl should have an integer "1" when calling 
  beams=beams, draw_beamnum=draw_beamnum, $
  rgrange=rgrange, pixelonly=pixelonly, bmfill=bmfill 
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  nh_list = strsplit('bks cve cvw ekb fhe fhw gbr han hok hkw inv kap kod ksr lyr pgr pyk rkn sas sto wal ade adw', /ext )
  sh_list = strsplit('bpk dce fir hal ker mcm san sps sye sys tig unw zho', /ext )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;Check the keywords and generate the station list to be plotted
  stns = ''
  if keyword_set(nh) then append_array, stns, nh_list
  if keyword_set(sh) then append_array, stns, sh_list
  if keyword_set(site) then append_array, stns, strsplit(site, /ext) 
  if stns[0] eq '' then return
  ;print, stns 
  
  if ~keyword_set(color) then color = 0 ;default color
  
  
  ;Initialize !sdarn system variable
  sd_init
  
  ;Prepare for AACGM conversion
  if ~keyword_set(geo_plot) and ~keyword_set(get_sdfovtbl) then begin
    ts = time_struct( !map2d.time)
    yrsec = long( (ts.doy-1)*86400L + ts.sod )
    aacgmloadcoef, ts.year 
  endif
  
  ;Obtain the directory path where overlay_map_precal_sdfov.pro and save files are located.
  stack = SCOPE_TRACEBACK(/structure)
  filename = stack[SCOPE_LEVEL()-1].filename
  dir = FILE_DIRNAME(filename)
  
  for i=0, n_elements(stns)-1 do begin
    
    stn = stns[i]
    tblfn = dir +'/sdfovtbl_'+stn+'.sav'
    if ~file_test(tblfn) then continue
    restore, tblfn 
    if keyword_set(get_sdfovtbl) then begin
      get_sdfovtbl=sdfovtbl & return
    endif
    
    n_bm = n_elements( sdfovtbl.glat[*,0] )-1
    n_rg = n_elements( sdfovtbl.glat[0,*] )-1
    
    glats = [ sdfovtbl.glat[0:n_bm,0], reform(sdfovtbl.glat[n_bm,0:n_rg]), $
      reverse(sdfovtbl.glat[0:n_bm,n_rg]), reverse(reform(sdfovtbl.glat[0,0:n_rg])) ]
    glons = [ sdfovtbl.glon[0:n_bm,0], reform(sdfovtbl.glon[n_bm,0:n_rg]), $
      reverse(sdfovtbl.glon[0:n_bm,n_rg]), reverse(reform(sdfovtbl.glon[0,0:n_rg])) ]
    
    if keyword_set(geo_plot) or !map2d.coord eq 0 then begin
      lats = glats & lons = glons 
    endif else begin
      ;AACGM conversion
      alt = glats & alt[*] = 400. ;[km]
      aacgmconvcoord, glats,glons,alt, mlats,mlons, err, /TO_AACGM
      years = long( glats ) & years[*] = ts.year 
      yrsecs = long( glats) & yrsecs[*] = yrsec
      mlts = aacgmmlt( years, yrsecs,  (mlons+360.) mod 360  )
      
      ;Project the fov to the northern hemisphere if force_nhemis is set.
      if keyword_set(force_nhemis) then mlats = abs( mlats ) 
      if keyword_set(force_shemis) then mlats = -1. * abs(mlats) 
      
      lats = mlats & lons = mlts /24. * 360.
    endelse
    
    ;Draw the f-o-v with the color given by "color" keyword
    plots, lons, lats, color=color, thick=linethick
    ;Fill the f-o-v with the color given by "color" keyword
    if keyword_set(fill) then polyfill, lons, lats, color=color
    
    
    ;Draw individual beams if keyword beams is set
    if keyword_set(beams) then begin
      
      glats = sdfovtbl.glat & glons = sdfovtbl.glon 
      
      if keyword_set(geo_plot) or !map2d.coord eq 0 then begin
        lats = glats & lons = glons
      endif else begin
        ;AACGM conversion
        alt = glats & alt[*] = 400. ;[km]
        aacgmconvcoord, glats,glons,alt, mlats,mlons, err, /TO_AACGM
        years = long( glats ) & years[*] = ts.year
        yrsecs = long( glats) & yrsecs[*] = yrsec
        mlts = aacgmmlt( years, yrsecs,  (mlons+360.) mod 360  )

        ;Project the fov to the northern hemisphere if force_nhemis is set.
        if keyword_set(force_nhemis) then mlats = abs( mlats )
        if keyword_set(force_shemis) then mlats = -1. * abs(mlats)

        lats = mlats & lons = mlts /24. * 360.
      endelse
      
      for n=0, n_elements(beams)-1 do begin
        
        bm = beams[n] 
        if bm lt 0 and bm gt n_bm then continue 
        
        if ~keyword_set(pixelonly) then begin
          PLOTS,lons[bm,0:n_rg],lats[bm,0:n_rg], linestyle=linestyle, color=color, thick=linethick
          PLOTS,lons[bm+1,0:n_rg],lats[bm+1,0:n_rg], linestyle=linestyle, color=color, thick=linethick
          
          if keyword_set(bmfill) then begin
            for rgnm=0, n_rg-1 do begin
              
              polyfill, $
                [ reform(lons[bm,rgnm:(rgnm+1)]), lons[bm:(bm+1),(rgnm+1)],$
                  reverse(reform(lons[(bm+1),rgnm:(rgnm+1)])), reverse(lons[bm:(bm+1),rgnm]) ], $ 
                [ reform(lats[bm,rgnm:(rgnm+1)]), lats[bm:(bm+1),(rgnm+1)],$
                  reverse(reform(lats[(bm+1),rgnm:(rgnm+1)])), reverse(lats[bm:(bm+1),rgnm]) ], $
                color=color
            endfor
          endif
          
        endif
        
        
        
      endfor
      
      
      
      
      
    endif
    
    
    
    
  endfor
  
   
  
  return
end

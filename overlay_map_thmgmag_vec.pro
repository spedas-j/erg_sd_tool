;+
; C:\cygwin\home\hori\work\IDL_project\SDtool_forERG\overlay_map_thmgmag_vec.pro
;
; :DESCRIPTION:
;		Describe the procedure.
; 
; :KEYWORDS:
; 
; bscale: Length [deg] of the vector scale for 10 nT. 
;         If bscale=1 then a B vector of 100 nT is drawn as a vector 
;         whose arc length on the world map is 10 deg.
; 
; :AUTHOR:
; 	Tomo Hori (E-mail: horit at stelab.nagoya-u.ac.jp)
;
; :HISTORY:
; 	2011/12/21: Created
;
;-
PRO overlay_map_thmgmag_vec, site=site, $
  ave_time_range=ave_time_range, bscale=bscale, $
  rot90=rot90, rot270=rot270 
  
  ;Check the arguments
  if ~keyword_set(site) then return
  if ~keyword_set(bscale) then bscale=1.
  
  ;Initialize !sdarn system variable
  sd_init
  
  for i=0, n_elements(site)-1 do begin
    
    stn = site[i]
    varn = 'thg_mag_'+stn
    if strlen(tnames(varn)) eq 0 then $
      thm_load_gmag,site=stn,lev=2,/subtract_ave
    if strlen(tnames(varn)) eq 0 then continue
    
    get_data, varn, data=d, dl=dl, lim=lim
    glat0 = dl.data_att.site_latitude
    glon0 = dl.data_att.site_longitude
    
    ;Subtract average values for a specific time range from the original
    if keyword_set(ave_time_range) and n_elements(ave_time_range) eq 2 then begin
      t0 = ave_time_range[0] & t1 = ave_time_range[1]
      ii = nn(d.x, [t0,t1])
      num = ii[1]-ii[0]+1L
      avehdz = replicate(1.,n_elements(d.y[*,0])) # transpose(total(d.y[(ii[0]):(ii[1]), *], 1)/num) 
      d.y -= avehdz 
    endif
    
    b_hd = sqrt( (d.y[*,0])^2 + (d.y[*,1])^2 )
    vec_len = b_hd / 10. * bscale  ; b_hd=100 nT and bscale=2 --> 20 deg 
    
    ;Time for plotting
    idx = nn(d.x, !sdarn.sd_polar.plot_time)
    t = d.x[idx]
    if i eq 0 then print, time_string(t)
    
    ;AACGM conversion
    lat0 = glat0 & lon0 = glon0
    if ~keyword_set(geo_plot) then begin
      aacgmconvcoord, glat0,glon0,0.001,mlat0,mlon0,err,/TO_AACGM
      ts=time_struct(t) & yrsec=(ts.doy-1)*86400L + long(ts.sod)
      mlt0 = aacgmmlt(ts.year, yrsec, (mlon0+360.) mod 360. )
      mlt0_deg = mlt0 /24.*360.
      
      lat0 = mlat0 & lon0 = mlt0_deg
    endif
    
    ;Rotate to the current dir or the convection dir
    blat = d.y[idx,0] & blon = d.y[idx,1]
    if keyword_set(rot90) then begin
      blat = -d.y[idx,1] & blon = d.y[idx,0]
    endif
    if keyword_set(rot270) then begin
      blat = d.y[idx,1] & blon = -d.y[idx,0]
    endif
    
    print, stn+': ',lat0,lon0/360.*24, blat,blon,vec_len[idx], $
      format='(5A,1X,5F6.1)'
    overlay_map_vec, lat0, lon0, blat,blon,$
      vec_len[idx], linethick=1.5
    
  endfor
  
    
    
     
    
    












  return

end

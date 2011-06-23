FUNCTION get_sd_ave, vn, latrng=latrng, lonrng=lonrng, maglat=maglat, new_vn=new_vn
  
  
  ;Check the arguments and keywords
  npar = n_params()
  if npar ne 1 then return, !values.f_nan
  if (tnames(vn[0])) eq '' then return, !values.f_nan
  
  if n_elements(latrng) ne 2 or n_elements(lonrng) ne 2 then return, !values.f_nan
  
  is_maglat = keyword_set(maglat)
  
  ;Initialize the AACGM environment
  if is_maglat then begin
    sd_init
    aacgmloadcoef, 2005
  endif
  
  ;Obtain parts of the variable name
  prefix = strmid(vn, 0,7)  ;e.g., 'sd_hok_'
  suf = strmid(vn,0,1,/reverse) ; e.g., '0'
  azmno_vn = prefix+'azim_no_'+suf
  ptbl_vn = prefix+ 'position_tbl_'+suf
  ctbl_vn = prefix+ 'positioncnt_tbl_'+suf
  
  ;Get variable values
  get_timespan, tr ;Time range
  get_data, vn, data=d, lim=vn_lim
  vn_v = d.v &  vn_time = d.x 
  get_data, azmno_vn, data=d
  azmno = d.y
  get_data, ctbl_vn, data= d 
  lontbl = reform(d.y[0,*,*,0])
  lattbl = reform(d.y[0,*,*,1])
  
  scan_str = get_scan_struc_arr(vn)
  scant = scan_str.x
  scan = scan_str.y
  
  ;If maglat is set, use the AACGM lat table
  if is_maglat then begin
    glat = lattbl & glon = lontbl
    alt = glat & alt[*] = 400. ;[km]
    aacgmconvcoord, glat,glon,alt, mlat,mlon,err, /TO_AACGM
    lattbl = mlat 
  endif
  
  idx = where(      lattbl ge latrng[0] and lattbl le latrng[1] $
                and lontbl ge lonrng[0] and lontbl le lonrng[1] )
  ;if idx[0] eq -1 then return, !values.f_nan
  
  val = fltarr(n_elements(scant))
  for i=0L, n_elements(scant)-1 do begin
    
    tmpscan = reform(scan[i,*,*])
    if idx[0] ne -1 then val[i] = mean( tmpscan[idx], /nan ) $
    else val[i] = !values.f_nan
    
  endfor
  
  ;Save as a tplot variable
  if keyword_set(new_vn) then begin
    if (size(new_vn))[1] eq 7 then begin
      store_data, new_vn, data={x:scant, y:val}, $
        lim={ytitle:vn_lim.ytitle }
    endif
  endif
  
  ;Return the data structure containing the average values
  return, {x:scant, y:val}
  

end

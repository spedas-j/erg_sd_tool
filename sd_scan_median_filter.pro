pro sd_scan_median_filter, vns, swidth=swidth, clipval=clipval

  npar = n_params()
  if npar ne 1 then return
  if strlen(tnames(vns[0])) lt 6 then return
  
  
  if n_elements(vns) gt 1 then begin
    for i=0L, n_elements(vns)-1 do begin
      sd_scan_median_filter, vns[i], swidth=swidth, clipval=clipval
    endfor
    return
  endif
  ;;tsmooth_in_time
  if ~keyword_set(swidth) then swidth = 3
  if ~keyword_set(clipval) then clipval = 1000. ;[m/s]
  
  vn = (tnames(vns))[0]
  prefix = strmid( vn, 0, 7 )
  suf = strmid( vn, 0, 1, /rev )
  len = strlen(vn) 
  trunk = strmid( vn, 0, len-2 )
  
  ;Obtain the scan structure for given data
  scanstr = get_scan_struc_arr( vn )
  nrg = n_elements(scanstr.y[0,*,0])
  nbm= n_elements(scanstr.y[0,0,*])
  
  ;Clip abnormal values and fill them with NaN
  idx = where( abs(scanstr.y) gt clipval )
  if idx[0] ne -1 then scanstr.y[idx] = !values.f_nan
  
  nscan = n_elements(scanstr.x)
  for n=0L, nscan-1 do begin
    scan = reform( scanstr.y[n,*,*] )
    scan = estimator_filter( scan, swidth, /median, /nan )
    scanstr.y[n,*,*] = reform( scan, 1, nrg, nbm )
  endfor
  
  ;Reform the scan structure into the original tplot variable
  get_data, vn, data=d, dl=dl, lim=lim 
  val = d.y & bmtime = d.x & v = d.v 
  scanno_vn = prefix + 'scanno_' + suf
  get_data, scanno_vn, data=d & scannoarr = d.y
  azmno_vn = prefix + 'azim_no_' + suf
  get_data, azmno_vn, data=d & azmnoarr = d.y
  
  newval = val & newval[*] = !values.f_nan
  for j=0L, n_elements(bmtime)-1 do begin
  
    scanno = scannoarr[j]
    azmno = azmnoarr[j]
    newval[j,*] = scanstr.y[ scanno, *, azmno ]
    
   endfor
    
    store_data, trunk+'_filtered_'+suf, data={x:bmtime, y:newval, v:v}, dl=dl, lim=lim
    
    return
  end

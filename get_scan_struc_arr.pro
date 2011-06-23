FUNCTION get_scan_struc_arr, vn 
  
  ;Check the argument
  npar = n_params()
  if npar ne 1 then return, 0
  vn = vn[0] ;only 1st element is processed below
  if (tnames(vn))[0] eq '' then return,0
  get_data, vn, data=d
  vartime = d.x & var = d.y 
  if (size(var))[0] ne 2 then return, 0
  
  ;Strings consisting of vn
  prefix = strmid(vn, 0,7)
  suf = strmid(vn, 0,1,/reverse) 
  azmno_vn = prefix+'azim_no_'+suf
  scanno_vn = prefix+'scanno_'+suf
  get_data, azmno_vn, data=d
  azmno = d.y 
  get_data, scanno_vn, data=d
  scanno = d.y
  
  scan = scanno[uniq(scanno)]
  azmno_sorted = azmno[sort(azmno)]
  azm = azmno_sorted[uniq(azmno_sorted)]
  azmmax = n_elements(azm)
  nrang = n_elements(var[0,*])
  
  ;Create a 2-D scan array and its time array
  vararr = fltarr( n_elements(scan), nrang, azmmax )
  timearr = dblarr(n_elements(scan))
  vararr[*] = !values.f_nan
  
  ;Store data into the 2-D scan array
  for i=0L, n_elements(scan)-1 do begin
    tscan = scan[i]
    idx = where(scanno eq tscan)
    if idx[0] ne -1 then begin
      timearr[i] = mean( vartime[idx] ) 
                  ;Time label is the average for tplot drawing
      tazmno = azmno[idx]
      tvar = transpose(var[idx,*])
      for j=0, n_elements(tazmno)-1 do begin
        vararr[i,*,j] = reform( tvar[*, (sort(tazmno))[j]], 1,nrang,1)
        ;debugging
        ;if (i mod 100) eq 0 then print, tscan,tazmno[(sort(tazmno))[j]]
      endfor
    endif
  endfor
  
  return, {x:timearr, y:vararr}
end

      
  
  
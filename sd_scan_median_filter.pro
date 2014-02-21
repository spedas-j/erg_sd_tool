;+
;  PROCEDURE sd_scan_median_filter 
;  
; :Description:
;    Detrend LOSV data by interpolating them by spatially median-filtering, then subtracting the smoothed-in-time trend. 
;
; :Params:
;    vns: a tplot variable to be median-filtered. Currently only sd_???_vlos_? is acceptable. 
;
; :Keywords:
;    swidth:  Spatial width in a 2D scan with which LOSV values are median-filtered 
;    clipval:   a value to clip the input data. Data beyond +/- this value are replaced by NaN, then ignored. 
;    detrend_t:   Time interval for which the median-interpolated data are running-averaged. 
;
; :Author: horit
;-
pro sd_scan_median_filter, vns, swidth=swidth, clipval=clipval, detrend_t=detrend_t 

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
  if ~keyword_set(detrend_t) then detrend_t = 1000.  ;[sec] 
  
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
    
    store_data, trunk+'_med-interp_'+suf, data={x:bmtime, y:newval, v:v}, dl=dl, lim=lim
    
    tsmooth_in_time, trunk+'_med-interp_'+suf, detrend_t, newname='tmp' 
    
    get_data, vn, data=d, dl=dl, lim=lim
    get_data, 'tmp', data=ds, dl=dls, lim=lims 
    store_Data, trunk+'_detrended_'+suf, data={x:d.x, y:d.y-ds.y, v:d.v}, dl=dl, lim=lim
    store_data, delete='tmp'
    tplot_names, trunk+'_detrended_'+suf
    
    return
  end

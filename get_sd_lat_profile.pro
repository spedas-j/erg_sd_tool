PRO get_sd_lat_profile, vn, latrng=latrng, lonrng=lonrng, dlat=dlat, maglat=maglat, new_vn=new_vn
  
  ;Currently this procedure can take as an argument:
  ; vlos, vlshell, (vlos|vnorth|veast), _iscat
  
  ;Check the arguments and keywords
  if tnames(vn[0]) eq '' then return
  vn0 = tnames(vn[0])
  vn = vn0
  
  if ~keyword_set(latrng) or ~keyword_set(lonrng) then return
  
  if ~keyword_set(dlat) then dlat = 1.
  
  if n_elements(latrng) ne 2 or n_elements(lonrng) ne 2 then return
  
  latrng = float(latrng)
  dlat = abs(dlat)
  tlatarr = ( latrng[0] + dlat* findgen(ceil( (latrng[1]-latrng[0])/dlat )+1) ) < latrng[1]
 
  ;print, tlatarr
  
  ;Generate the time-lat array
  scan = get_scan_struc_arr(vn) 
  valarr = fltarr( n_elements(scan.x), n_elements(tlatarr)-1 )
  nlat = n_elements(tlatarr)
  latc = (tlatarr[1:(nlat-1)]+tlatarr[0:(nlat-2)])/2.
  ;print, latc
  
  for i=0L, n_elements(tlatarr)-2 do begin
    
    latmin = tlatarr[i] & latmax = tlatarr[i+1]
    latave = get_sd_ave(vn, latrng=[latmin,latmax],lonrng=lonrng,$
                          maglat=maglat )
    valarr[*,i] = latave.y
    
  endfor
  
  ;Store the time-lat arr in a tplot var
  if ~keyword_set(new_vn) then $
    new_vn = vn +'_latpro_lon'+string(lonrng[0],'(I03)')+'-'+$
      string(lonrng[1],'(I03)')
      
  store_data, new_vn, data={x: scan.x, y:valarr, v:latc}, $
    dl={spec:1}, $
    lim={zrange:[-300,300]}
    
  return
end

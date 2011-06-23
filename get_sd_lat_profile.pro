PRO get_sd_lat_profile, vn, latrng=latrng, lonrng=lonrng, dlat=dlat, maglat=maglat, new_vn=new_vn
  
  ;Currently this procedure can take as an argument:
  ; vlos, vlshell, (vlos|vnorth|veast), _iscat
  
  ;Check the arguments and keywords
  if tnames(vn[0]) eq '' then return
  
  if ~keyword_set(dlat) then dlat = 1.
  
  if n_elements(latrng) ne 2 or n_elements(lonrng) ne 2 then return
  
  dlat = abs(dlat)
  tlatarr = ( latrng[0] + dlat* findgen(ceil( (latrng[1]-latrng[0])/dlat )) ) < latrng[1]
 
  
  
  return
end

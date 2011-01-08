FUNCTION get_sphcntr, latarr, lonarr

  ;Check the array size
  if n_elements(latarr) ne n_elements(lonarr) then begin
    print, 'get_sphcntr: Array size does not match!'
    return, [!values.f_nan,!values.f_nan] 
  endif
  
  phiarr = lonarr*!dtor
  thearr = (90.-latarr)*!dtor
  x = sin(thearr)*cos(phiarr)
  y = sin(thearr)*sin(phiarr)
  z = cos(thearr)
  ave_x = total(x) & ave_y = total(y) & ave_z = total(z)
  xyz_to_polar, [ave_x,ave_y,ave_z], phi=lon, theta=lat, $
    /ph_0_360
  
return, [lat,lon]
end

;+
; lat0: latitude of the start point [deg]
; lon0: longitude of the start point [deg]
; dlat: the latitudinal component of the vector to be drawn
; dlon: the longitudinal component of the vector to be drawn
; arclength: length of the vector in deg. dlat and dlon are normalized 
;            by this value 
; 
; ex)   overlay_map_vec, 65., 270., -0.,-3, 18., linethick=1.5
;-
PRO overlay_map_vec, lat0, lon0, dlat, dlon, arclength, $
  linethick=linethick, color=color
  
  ;Check the arguments
  npar = n_params()
  if npar ne 5 then return 
  
  ;Calculate the end point of a vector with given arclength
  the0 = 90. - lat0 & dthe = (-1.)*dlat
  get_end_point_in_sph, the0,lon0,dthe,dlon,arclength, $
    the1,phi1
  lat1 = 90.-the1 & lon1 = phi1
  
  
  ;Plot!
  plots, lon0,lat0, psym=4, symsize=1.2
  plots, [lon0,lon1], [lat0,lat1], thick=linethick





  return

end
PRO overlay_map_sdfov, site=site, force_nhemis=force_nhemis, $
      geo_plot=geo_plot

  ;Set the list of the available sites
  valid_sites = [ 'hok','ksr','sye','sys','bks','rkn','unw','tig' ]
  
  ;Check the site name 
  stns = thm_check_valid_name( sites, valid_sites, /ignore_case, /include_all )
  if strlen(stns[0]) eq 0 then begin
    print, 'No valid radar name in sites!'
    print, 'Data currently available: ',valid_sites
    return
  endif

  ;The loop to draw fovs of multiple stations
  for i=0, n_elements(stns)-1 do begin
    stn = stns[i]
    ptbl_vn = tnames('sd_'+strlowcase(stn)+'_position_tbl_?')
    if strlen(ptbl_vn) lt 10 then continue
    
    
;Load the position table
get_data, ptbl_vn,data=d
glat=reform(d.y[0,*,*,1])  ;--> array size [76,17] 
glon=reform(d.y[0,*,*,0])
alt = glat & alt[*] = 400 ;km

if ~keyword_set(geo_plot) then begin
aacgmconvcoord,glat,glon,alt,mlat,mlon,err,/TO_AACGM
mlon = (mlon + 360.) mod 360.
ts = time_struct(!sdarn.sd_polar.plot_time)
yrsec = long( alt )
yrsec[*] = long((ts.doy-1)*86400L + ts.sod)
yr = yrsec & yr[*] = ts.year

tmlt = aacgmmlt(yr, yrsec, mlon)
tmlt = ( (tmlt + 24. ) mod 24. ) /24.*360.  ;[deg]
endif else begin
  mlat = glat & tmlt = glon
endelse

n_rg = n_elements(mlat[*,0])-1
n_az = n_elements(mlat[0,*])-1

plots,tmlt[0,0:n_az],mlat[0,0:n_az]
plots,tmlt[0:n_rg,n_az],mlat[0:n_rg,n_az]
plots,tmlt[n_rg,0:n_az],mlat[n_rg,0:n_az]
plots,tmlt[0:n_rg,0],mlat[0:n_rg,0]

return
end

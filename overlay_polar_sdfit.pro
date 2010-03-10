;+
; :DESCRIPTION:
;    Describe the procedure.
;
; :PARAMS:
;    datvn
;
; :KEYWORDS:
;    time
;
; :AUTHOR: T. Hori
; :HISTORY:
; 	2010/03/09: Created
;-
pro overlay_polar_sdfit, datvn, time=time

;Check argument and keyword
npar=n_params()
if npar lt 1 then return
if ~keyword_set(time) then begin
  get_timespan, tr
  time = (tr[0]+tr[1])/2.  ; Take the center of the designated time range
endif

;if datvn is the index number for tplot var
if size(datvn[0], /type) eq 2 or size(datvn[0], /type) eq 3 then begin
  datvn = tnames(datvn[0])
endif
;then datvn should a string now
if size(datvn[0], /type) ne 7 then return
if n_elements(datvn) eq 0 then return

;Load the data to be drawn and to be used for drawing on a 2-d map
get_data, datvn, data=d, dl=dl, lim=lim
get_data, 'sd_hok_azim_no_0', data=az
get_data, 'sd_hok_position_tbl_0', data=tbl 
get_data, 'sd_hok_scanstartflag_0', data=stflg
get_data, 'sd_hok_scanno_0', data=scno

;Choose data for the time given by keyword
idx = nn( scno.x, time_double(time) )
bmno = where( scno.y eq scno.y[idx] )
;;for debugging
print, 'time: '+time_string(time)
print, 'time by nn: '+time_string(scno.x[idx])
print, 'scan no: ',scno.y[idx]
print, 'beam no:', bmno
print, 'scan time: '+time_string(min(scno.x[bmno]))+' -- '+time_string(max(scno.x[bmno]))
;;

;Set the range of the plotted values
str_element, lim, 'zrange', val, success=s
if s eq 1 then valrng = val else valrng=[-1000.,1000]

;Set color level for contour
clmax = !d.table_size-1
clmin = 8L
cnum = clmax-clmin


;Set the lat-lon canvas and draw the continents
map_set, 89., 0., /orth, /isotropic, /horizon  
map_continents, /coast

;Draw the data
for i=0L, n_elements(bmno)-1 do begin
  bn = bmno[i]
  valarr = reform(d.y[bn, *, 0])
  rgmax = n_elements(valarr)
  azno = az.y[bn] 
  tblidx = max(where(tbl.x le d.x[bn], cnt)) 
  if tblidx eq -1 then begin
    print, 'beam time does not fall in any time range of the position table!'
    return
  endif
  pos = reform(tbl.y[tblidx,*,azno:(azno+1),*])
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; The routine to convert pos to GEO has been inserted temorarily. 
  ;; This part should be removed as soon as the bug in make_sd_fitacf_cdf_file.pro 
  ;; is fixed. 
  for k=0L, n_elements(pos[*,0,0])-1 do begin
    for l=0L, n_elements(pos[0,*,0])-1 do begin
      ts = time_struct(tbl.x[0])
      yrsec = long(ts.doy*86400. + ts.sod)
      aacgm_load_coef, 2005
      aacgm_conv_coord, (pos[k,l,1]), (pos[k,l,0]+360.) mod 360., $
        400., glat, glon, err, /TO_GEO
      pos[k,l,1] = glat & pos[k,l,0] = glon
    endfor
  endfor
  ;;;;;;;;;;;;;;;;;;;;;;
  
  for j=0, rgmax-1 do begin
    val = valarr[j]
    if ~finite(val) then continue ;Skip drawing for NaN
    
    ;Color level for val
    clvl = clmin + cnum*(val-valrng[0])/(valrng[1]-valrng[0])
    clvl = (clvl > clmin) 
    clvl = (clvl < clmax) ; clmin <= color level <= clmax 
    
    ;Lon and Lat for a square to be filled 
    lon = [ pos[j,0,0], pos[j,1,0], pos[j+1,1,0], pos[j+1,0,0] ]
    lat = [ pos[j,0,1], pos[j,1,1], pos[j+1,1,1], pos[j+1,0,1] ]
    
    ;Draw 
    polyfill, lon, lat, color=clvl
    
  endfor

endfor


;Normal end
return

end


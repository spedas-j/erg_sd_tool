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

npar=n_params()
if npar lt 1 then return

;if datvn is the index number for tplot var
if size(datvn[0], /type) eq 2 or size(datvn[0], /type) eq 3 then begin
  datvn = tnames(datvn[0])
endif
;then datvn should a string now
if size(datvn[0], /type) ne 7 then return
if n_elements(datvn) eq 0 then return

get_data, datvn, data=d, dl=dl, lim=lim
get_data, 'sd_hok_azim_no_0', data=az
get_data, 'sd_hok_position_tbl_0', data=tbl 
get_data, 'sd_hok_scanstartflag_0', data=stflg
get_data, 'sd_hok_scanno_0', data=scno

idx = nn( scno.x, time_double(time) )
bmno = where( scno.y eq scno.y[idx] )
;;for debugging
print, 'time: '+time_string(time)
print, 'time by nn: '+time_string(scno.x[idx])
print, 'scan no: ',scno.y[idx]
print, 'beam no:', bmno
print, 'scan time: '+time_string(min(scno.x[bmno]))+' -- '+time_string(max(scno.x[bmno]))
;;

str_element, lim, 'zrange', val, success=s
if s eq 1 then 



;Normal end
return

end


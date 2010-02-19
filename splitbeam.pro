;+
; :Author: hori
;-
pro splitbeam, tvar 

if strlen(tnames(tvar)) lt 2 then return
if strlen(tnames('sd_hok_azim_no_0')) lt 2 then return

get_data, 'sd_hok_azim_no_0', data=d 
bmidx = uniq( d.y, sort(d.y) )

get_data, tvar, data=dd, dl=dl, lim=lim 

for i=0L, n_elements(bmidx)-1 do begin

  vn = tvar + '_azim' + string(d.y[bmidx[i]], '(I2.2)')
  ;print, vn
  idx = where( d.y eq d.y[bmidx[i]] )
  ;print, n_elements(idx) 
  if idx[0] eq -1 then continue
  ;help, dd.x, dd.y
  store_data, vn, data={x:dd.x[idx], y:dd.y[idx,*,0], v:dd.v }, dl=dl, lim=lim
  
endfor


end


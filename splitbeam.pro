;+
; :Author: hori
;-
pro splitbeam, tvar 

;Does tvar exist?
tvar = tvar[0] ;so far only 1st argument is taken. 
if size(tvar,/type) eq 2 OR size(tvar,/type) eq 3 then tvar=tnames(tvar)
if strlen(tnames(tvar)) lt 2 then return
if strlowcase(strmid(tvar, 0,3)) ne 'sd_' then return

;Generate the tplot var. name for the beam_dir
stn = strmid(tvar, 3,3)
suf = strmid(tvar, 0,1, /reverse )
beamdir_tvar_name = 'sd_'+stn+'_azim_no_'+suf

if strlen(tnames(beamdir_tvar_name)) lt 2 then return

get_data, beamdir_tvar_name, data=d 
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
  options, vn, 'ytitle', strupcase(stn)+' bm'+string(d.y[bmidx[i]], '(I2.2)')
  options, vn, 'ysubtitle', '[range gate]'
  
endfor


end


;+
; ERG_LOAD_SD
; 
; to load SuperDARN fitacf data as tplot vars
;
; written by: T. Hori 
; last updated: 
;-
pro erg_load_sd, sites=sites, cdffn=cdffn, get_support_data=get_support_data

if ~keyword_set(cdffn) then begin
  if ~keyword_set(sites) then sites = 'hok'
  
  source = file_retrieve(/struct)
  source.local_data_dir = root_data_dir()+'GROUND/sd/fitacf/'+sites[0]+'/'
  source.remote_data_dir = 'http://st4a.stelab.nagoya-u.ac.jp/~horit/web_tmp/tmp/data/GROUND/sd/fitacf/'+sites[0]+'/'
  source.min_age_limit = 900
  
  datfileformat = 'YYYY/sd_fitacf_l2_'+sites[0]+'_YYYYMMDD*cdf'
  relfnames = file_dailynames(file_format=datfileformat, trange=trange, times=times)
  
  datfiles = file_retrieve(relfnames, _extra=source)
  if n_elements(datfiles) lt 1 or strlen(datfiles[0]) lt 10 then begin
    print, 'No data was loaded!'
    return
  endif
endif else begin
  datfiles = cdffn 
  if fix(total(file_test(datfiles))) lt 1 then begin
    print, 'Cannot find any of the data file(s): ', cdffn
    return
  endif
endelse

prefix='sd_' + sites[0] + '_'
cdf2tplot,file=datfiles, prefix=prefix, get_support_data=get_support_data

tclip, 'sd_hok_'+ ['pwr','spec','vlos','elev'] +'*', -4000,4000, /over

options,'sd_hok_pwr_0', ytitle='Range gate',ysubtitle='',ztitle='Backscatter power [dB]'
options,'sd_hok_pwr_err_0', ytitle='Range gate',ysubtitle='',ztitle='power err[dB]'
options,'sd_hok_spec_width_0', ytitle='Range gate',ysubtitle='',ztitle='Spec. width [m/s]'
options,'sd_hok_spec_width_err_0', ytitle='Range gate',ysubtitle='',ztitle='Spec. width err[m/s]'
options,'sd_hok_vlos_0', ytitle='Range gate',ysubtitle='',ztitle='Doppler velocity [m/s]'
options,'sd_hok_vlos_err_0', ytitle='Range gate',ysubtitle='',ztitle='Vlos err [m/s]'
options,'sd_hok_elev_angle_0', ytitle='Range gate',ysubtitle='',ztitle='Elev. angle [deg]'
options,'sd_hok_echo_flag_0', ytitle='Range gate',ysubtitle='',ztitle='1: iono. echo'
options,'sd_hok_quality_0', ytitle='Range gate',ysubtitle='',ztitle='quality'
options,'sd_hok_quality_flag_0', ytitle='Range gate',ysubtitle='',ztitle='quality flg'

zlim, 'sd_hok_pwr_0', 0,30
zlim, 'sd_hok_pwr_err_0', 0,30
zlim, 'sd_hok_spec_width_0', 0,200
zlim, 'sd_hok_spec_width_err_0', 0,300
zlim, 'sd_hok_vlos_0', -300,300
zlim, 'sd_hok_vlos_err_0', 0,300

get_data, 'sd_hok_pwr_0', data=d & pwr = d.y
idx = WHERE( ~FINITE(pwr) )

tn='sd_hok_echo_flag_0'
get_data, tn, data=d, dl=dl, lim=lim & val=FLOAT(d.y)
IF idx[0] NE -1 THEN val[idx] = !values.f_nan
store_data, tn, data={x:d.x, y:val, v:d.v}, dl=dl, lim=lim

tn='sd_hok_quality_0'
get_data, tn, data=d, dl=dl, lim=lim & val=FLOAT(d.y)
IF idx[0] NE -1 THEN val[idx] = !values.f_nan
store_data, tn, data={x:d.x, y:val, v:d.v}, dl=dl, lim=lim

tn='sd_hok_quality_flag_0'
get_data, tn, data=d, dl=dl, lim=lim & val=FLOAT(d.y)
IF idx[0] NE -1 THEN val[idx] = !values.f_nan
store_data, tn, data={x:d.x, y:val, v:d.v}, dl=dl, lim=lim

;Load the position table(s) ;;;;;;;;;;;;;;;;;;
tbl_0='' & tbl_1='' & tbl_2=''
time_0='' & time_1='' & time_2=''
tbllist = ['tbl_0', 'tbl_1' , 'tbl_2']
timelist = ['time_0','time_1','time_2']
for i=0L, n_elements(datfiles)-1 do begin
  cdfi = cdf_load_vars( datfiles[i], varformat='*' ) 
  timevn = strfilter( cdfi.vars.name, 'Epoch_?' )
  ptblvn = strfilter( cdfi.vars.name, 'position_tbl_?' )
  ;Error check
  if n_elements(timevn) eq 0 or n_elements(ptblvn) eq 0 or $
    n_elements(timevn) ne n_elements(ptblvn) then begin 
    dprint, 'Epoch_x and position_tbl_x mismatch in CDF!'
    return
  endif
  timevn = timevn[ sort(timevn) ] ;sort the variable names
  ptblvn = ptblvn[ sort(ptblvn) ]
  
  for j=0, n_elements(ptblvn)-1 do begin
    tvn = timevn[j] & pvn = ptblvn[j]
    stblno = strmid(tvn, 0, 1, /reverse)
    tvnidx = (where( strcmp(cdfi.vars.name,tvn ) , nw))[0]
    pvnidx = (where( strcmp(cdfi.vars.name,pvn ) , nw))[0]
    time = *cdfi.vars[tvnidx].dataptr
    tbl  = *cdfi.vars[pvnidx].dataptr
    dim = size( tbl, /dim ) & tbl2 = reform( tbl, 1, dim[0],dim[1],dim[2] )
    rslt=execute('append_array, time_'+stblno+', [time[0],time[n_elements(time)-1]]')
    rslt=execute('append_array, tbl_'+stblno+', [tbl2,tbl2]' )
  endfor
endfor

for i=0, n_elements(tbllist)-1 do begin
  rslt=execute('n=n_elements('+tbllist[i]+')')
  if n lt 2 then continue
  rslt=execute('time='+timelist[i])
  rslt=execute('tbl='+tbllist[i])
  store_data, prefix+'position_'+tbllist[i], $
    data={x:time_double(time,/epoch), y:tbl}
endfor

;Release unused ptrs
tplot_ptrs = ptr_extract(tnames(/dataquant))
unused_ptrs = ptr_extract(cdfi,except=tplot_ptrs)
ptr_free,unused_ptrs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Normal end
return
end


;+
; ERG_LOAD_SD
; 
; to load SuperDARN fitacf data as tplot vars
;
; written by: T. Hori 
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

cdf2tplot,file=datfiles, prefix='sd_hok_', get_support_data=get_support_data

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





return
end

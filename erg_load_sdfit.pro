;+
; erg_load_sdfit
;
; :DESCRIPTION:
;    Load fitacf CDF files of SuperDARN as tplot variables.
;
;
;
; :KEYWORDS:
;    sites
;    cdffn
;    get_support_data
;
; :AUTHOR: T. Hori
; :HISTORY:
;   2010/03/09: Created as a draft version
;-
PRO erg_load_sdfit, sites=sites, cdffn=cdffn, get_support_data=get_support_data

  IF ~KEYWORD_SET(cdffn) THEN BEGIN
    IF ~KEYWORD_SET(sites) THEN sites = 'hok'
    
    source = file_retrieve(/struct)
    source.local_data_dir = root_data_dir()+'GROUND/sd/fitacf/'+sites[0]+'/'
    source.remote_data_dir = 'http://st4a.stelab.nagoya-u.ac.jp/~horit/web_tmp/tmp/data/GROUND/sd/fitacf/'+sites[0]+'/'
    source.remote_data_dir = 'http://gemsissc.stelab.nagoya-u.ac.jp/data/ergsc/ground/radar/sd/fitacf/'+sites[0]+'/'
    source.min_age_limit = 900
    
    datfileformat = 'YYYY/sd_fitacf_l2_'+sites[0]+'_YYYYMMDD*cdf'
    relfnames = file_dailynames(file_format=datfileformat, trange=trange, times=times)
    
    datfiles = file_retrieve(relfnames, _extra=source)
    IF N_ELEMENTS(datfiles) LT 1 OR STRLEN(datfiles[0]) LT 10 THEN BEGIN
      PRINT, 'No data was loaded!'
      RETURN
    ENDIF
  ENDIF ELSE BEGIN
    datfiles = cdffn
    IF FIX(TOTAL(FILE_TEST(datfiles))) LT 1 THEN BEGIN
      PRINT, 'Cannot find any of the data file(s): ', cdffn
      RETURN
    ENDIF
    sites=''
  ENDELSE
  
  ;Station name, to be improved in future for loading data for multiple stations
  stn = sites[0]
  
  prefix='sd_' + stn + '_'
  cdf2tplot,file=datfiles, prefix=prefix, get_support_data=get_support_data
  
  if n_elements(tnames(prefix+'*') ) lt 2 then begin
    print, 'No tplot var loaded.'
    return
  endif
  
  ;Set data values to NaN if abs(data) > 4000
  tclip, prefix+['pwr','spec','vlos','elev'] +'*', -4000,4000, /over
  
  ;For the case of a CDF including multiple range gate data
  suf = strmid( tnames(prefix+'pwr_?'), 0, 1, /reverse )
  for i=0, n_elements(suf)-1 do begin
  
    options,prefix+'pwr_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='Backscatter power [dB]'
    options,prefix+'pwr_err_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='power err[dB]'
    options,prefix+'spec_width_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='Spec. width [m/s]'
    options,prefix+'spec_width_err_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='Spec. width err[m/s]'
    options,prefix+'vlos_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='Doppler velocity [m/s]'
    options,prefix+'vlos_err_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='Vlos err [m/s]'
    options,prefix+'elev_angle_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='Elev. angle [deg]'
    options,prefix+'echo_flag_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='1: iono. echo'
    options,prefix+'quality_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='quality'
    options,prefix+'quality_flag_'+suf[i], ytitle=strupcase(stn)+'!CRange gate',ysubtitle='',ztitle='quality flg'
    
    zlim, prefix+'pwr_'+suf[i], 0,30
    zlim, prefix+'pwr_err_'+suf[i], 0,30
    zlim, prefix+'spec_width_'+suf[i], 0,200
    zlim, prefix+'spec_width_err_'+suf[i], 0,300
    zlim, prefix+'vlos_'+suf[i], -300,300
    zlim, prefix+'vlos_err_'+suf[i], 0,300
    
    get_data, prefix+'pwr_'+suf[i], data=d & pwr = d.y
    idx = WHERE( ~FINITE(pwr) )
    
    tn=prefix+'echo_flag_'+suf[i]
    get_data, tn, data=d, dl=dl, lim=lim & val=FLOAT(d.y)
    IF idx[0] NE -1 THEN val[idx] = !values.f_nan
    store_data, tn, data={x:d.x, y:val, v:d.v}, dl=dl, lim=lim
    
    tn=prefix+'quality_'+suf[i]
    get_data, tn, data=d, dl=dl, lim=lim & val=FLOAT(d.y)
    IF idx[0] NE -1 THEN val[idx] = !values.f_nan
    store_data, tn, data={x:d.x, y:val, v:d.v}, dl=dl, lim=lim
    
    tn=prefix+'quality_flag_'+suf[i]
    get_data, tn, data=d, dl=dl, lim=lim & val=FLOAT(d.y)
    IF idx[0] NE -1 THEN val[idx] = !values.f_nan
    store_data, tn, data={x:d.x, y:val, v:d.v}, dl=dl, lim=lim
  
  endfor
  
  
  ;Load the position table(s) ;;;;;;;;;;;;;;;;;;
  tbl_0='' & tbl_1='' & tbl_2=''
  time_0='' & time_1='' & time_2=''
  tbllist = ['tbl_0', 'tbl_1' , 'tbl_2']
  timelist = ['time_0','time_1','time_2']
  FOR i=0L, N_ELEMENTS(datfiles)-1 DO BEGIN
    cdfi = cdf_load_vars( datfiles[i], varformat='*' )
    timevn = strfilter( cdfi.vars.name, 'Epoch_?' )
    ptblvn = strfilter( cdfi.vars.name, 'position_tbl_?' )
    ;Error check
    IF N_ELEMENTS(timevn) EQ 0 OR N_ELEMENTS(ptblvn) EQ 0 OR $
      N_ELEMENTS(timevn) NE N_ELEMENTS(ptblvn) THEN BEGIN
      dprint, 'Epoch_x and position_tbl_x mismatch in CDF!'
      RETURN
    ENDIF
    timevn = timevn[ SORT(timevn) ] ;sort the variable names
    ptblvn = ptblvn[ SORT(ptblvn) ]
    
    FOR j=0, N_ELEMENTS(ptblvn)-1 DO BEGIN
      tvn = timevn[j] & pvn = ptblvn[j]
      stblno = STRMID(tvn, 0, 1, /reverse)
      tvnidx = (WHERE( STRCMP(cdfi.vars.name,tvn ) , nw))[0]
      pvnidx = (WHERE( STRCMP(cdfi.vars.name,pvn ) , nw))[0]
      time = *cdfi.vars[tvnidx].dataptr
      tbl  = *cdfi.vars[pvnidx].dataptr
      dim = SIZE( tbl, /dim ) & tbl2 = REFORM( tbl, 1, dim[0],dim[1],dim[2] )
      rslt=EXECUTE('append_array, time_'+stblno+', [time[0],time[n_elements(time)-1]]')
      rslt=EXECUTE('append_array, tbl_'+stblno+', [tbl2,tbl2]' )
    ENDFOR
  ENDFOR
  
  FOR i=0, N_ELEMENTS(tbllist)-1 DO BEGIN
    rslt=EXECUTE('n=n_elements('+tbllist[i]+')')
    IF n LT 2 THEN CONTINUE
    rslt=EXECUTE('time='+timelist[i])
    rslt=EXECUTE('tbl='+tbllist[i])
    store_data, prefix+'position_'+tbllist[i], $
      data={x:time_double(time,/epoch), y:tbl}
  ENDFOR
  
  ;Release unused ptrs
  tplot_ptrs = ptr_extract(tnames(/dataquant))
  unused_ptrs = ptr_extract(cdfi,except=tplot_ptrs)
  PTR_FREE,unused_ptrs
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;Normal end
  RETURN
END


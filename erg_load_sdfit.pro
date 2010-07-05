;+
; erg_load_sdfit
;
; :DESCRIPTION:
;    Load fitacf CDF files of SuperDARN as tplot variables.
;
;
;
; :KEYWORDS:
;    sites: 3-letter code of SD radar name. Currently only 'hok' works for loading.
;    cdffn: File path of a CDF file if given explicitly. 
;    get_support_data: Turn this on to load the supporting data 
;    trange: time range for which data are loaded. 
;            e.g., ['2008-10-01/00:00:00','2008-10-02/00:00:00'] 
;
; :AUTHOR: T. Hori
; :HISTORY:
;   2010/03/09: Created as a draft version
;
; $LastChangedBy:$
; $LastChangedDate:$
; $LastChangedRevision:$
; $URL:$
;-
PRO erg_load_sdfit, sites=sites, cdffn=cdffn, $
  get_support_data=get_support_data, $
  noacknowledgment=noacknowledgment, trange=trange

  ;Initialize the TDAS environment
  thm_init

  ;If a CDF file path is not given explicitly
  IF ~KEYWORD_SET(cdffn) THEN BEGIN

    ;Currently only 'hok' is put in array "sites". 
    ;This part should be implemented in future to take multiple sites 
    ;with thm_valid_names() and a for loop of stn. 
    IF ~KEYWORD_SET(sites) THEN sites = 'hok'
   
    stn = sites[0] 
 
    source = file_retrieve(/struct)
    source.local_data_dir = root_data_dir()+'ergsc/ground/radar/sd/fitacf/'+stn+'/'
    source.remote_data_dir = 'http://gemsissc.stelab.nagoya-u.ac.jp/data/ergsc/ground/radar/sd/fitacf/'+stn+'/'
    source.min_age_limit = 900
   
    ;Currently only the first element of array "sites" is adjusted. 
    ;to be implemented in future for loading data of multiple stations 
    datfileformat = 'YYYY/sd_fitacf_l2_'+stn+'_YYYYMMDD*cdf'
    relfnames = file_dailynames(file_format=datfileformat, trange=trange, times=times)
    
    datfiles = file_retrieve(relfnames, _extra=source)
    IF N_ELEMENTS(datfiles) LT 1 OR STRLEN(datfiles[0]) LT 10 THEN BEGIN
      PRINT, 'No data was loaded!'
      RETURN
    ENDIF
  ;If a CDF file path is given
  ENDIF ELSE BEGIN
    datfiles = cdffn
    IF FIX(TOTAL(FILE_TEST(datfiles))) LT 1 THEN BEGIN
      PRINT, 'Cannot find any of the data file(s): ', cdffn
      RETURN
    ENDIF
    ;;sites=''
  ENDELSE
  
  ;Read CDF files and create tplot variables
  prefix='sd_' + stn + '_'
  cdf2tplot,file=datfiles, prefix=prefix, $
    get_support_data=get_support_data, $
    /convert_int1_to_int2
 
  ;Quit if no data have been loaded 
  if n_elements(tnames(prefix+'*') ) lt 1 then begin
    print, 'No tplot var loaded.'
    return
  endif
  
  ;Set data values to NaN if abs(data) > 9000
  tclip, prefix+['pwr','spec','vlos','elev'] +'*', -9000,9000, /over
  
  ;For the case of a CDF including multiple range gate data
  suf = strmid( tnames(prefix+'pwr_?'), 0, 1, /reverse )
  for i=0, n_elements(suf)-1 do begin
  
    ;Set labels for some tplot variables
    options,prefix+'pwr_'+suf[i], ysubtitle='[range gate]',ztitle='Backscatter power [dB]'
    options,prefix+'pwr_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'pwr_err_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='power err [dB]'
    options,prefix+'pwr_err_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'spec_width_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='Spec. width [m/s]'
    options,prefix+'spec_width_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'spec_width_err_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='Spec. width err [m/s]'
    options,prefix+'spec_width_err_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'vlos_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='Doppler velocity [m/s]'
    options,prefix+'vlos_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'vlos_err_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='Vlos err [m/s]'
    options,prefix+'vlos_err_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'elev_angle_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='Elev. angle [deg]'
    options,prefix+'elev_angle_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'echo_flag_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='1: iono. echo'
    options,prefix+'echo_flag_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'quality_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='quality'
    options,prefix+'quality_'+suf[i], 'ytitle',strupcase(stn)+' all beams'
    options,prefix+'quality_flag_'+suf[i], ytitle=strupcase(stn)+' all beams',ysubtitle='[range gate]',ztitle='quality flg'
    options,prefix+'quality_flag_'+suf[i], 'ytitle',strupcase(stn)+' all beams'

    ;Split vlos_? tplot variable into 3 components
    get_data, prefix+'vlos_'+suf[i], data=d, dl=dl, lim=lim
    store_data, prefix+'vlos_'+suf[i], data={x:d.x, $
      y:d.y[*,*,2],v:d.v},dl=dl,lim=lim
    options,prefix+'vlos_'+suf[i],ztitle='LOS Doppler vel. [m/s]'
    store_data, prefix+'vnorth_'+suf[i], data={x:d.x, y:d.y[*,*,0],v:d.v},dl=dl,lim=lim
    options,prefix+'vnorth_'+suf[i],ztitle='GEO Northward comp. of Doppler vel. [m/s]'
    store_data, prefix+'veast_'+suf[i], data={x:d.x, y:d.y[*,*,1],v:d.v},dl=dl,lim=lim
    options,prefix+'veast_'+suf[i],ztitle='GEO Eastward comp. of Doppler vel. [m/s]'
   
    ;Set the z range explicitly for some tplot variables 
    zlim, prefix+'pwr_'+suf[i], 0,30
    zlim, prefix+'pwr_err_'+suf[i], 0,30
    zlim, prefix+'spec_width_'+suf[i], 0,200
    zlim, prefix+'spec_width_err_'+suf[i], 0,300
    zlim, prefix+'vlos_'+suf[i], -400,400
    zlim, prefix+'vnorth_'+suf[i], -400,400
    zlim, prefix+'veast_'+suf[i], -400,400
    zlim, prefix+'vlos_err_'+suf[i], 0,300
   
    ;Fill values --> NaN 
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

    ;Apply tclip the vlos data temporarily for demo 
    ;tclip, prefix+'vlos_'+suf[i] , -500.,500., /over

  endfor
  
  
  ;Load the position table(s) ;;;;;;;;;;;;;;;;;;
  tbl_0='' & tbl_1='' & tbl_2=''
  time_0='' & time_1='' & time_2=''
  tbllist = ['tbl_0', 'tbl_1' , 'tbl_2']
  timelist = ['time_0','time_1','time_2']
  FOR i=0L, N_ELEMENTS(datfiles)-1 DO BEGIN
    cdfi = cdf_load_vars( datfiles[i], varformat='*',/convert_int1_to_int2 )
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
  
  ;Show the rules of the road 
  ;unless keyword noacknowledgement is defined.
  if ~keyword_set(noacknowledgement) then begin
    vstr = tnames(prefix+'pwr_?')
    if strlen(vstr[0]) gt 5 then begin
      get_data, vstr[0], data=d, dl=dl
      print, '############## RULES OF THE ROAD ################'
      print, dl.cdf.gatt.rules_of_use
      print, '############## RULES OF THE ROAD ################'
    endif
  endif
  
  
  ;Normal end
  RETURN
END


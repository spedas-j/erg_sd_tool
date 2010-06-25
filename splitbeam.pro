;+
; :DESCRIPTION:
;    Divide tplot variables including all beams into that for each beam.
;
; :EXAMPLE:
;    splitbeam, 'sd_hok_vlos_0'
;
; :Author: hori
; :HISTORY:
;    2010/03/02: Created
;
; $LastChangedBy:$
; $LastChangedDate:$
; $LastChangedRevision:$
; $URL:$
;-
PRO splitbeam, tvars

  FOR n = 0, N_ELEMENTS(tvars)-1 DO BEGIN
  
    ;Does tvar exist?
    tvar = tvars[n] ;so far only 1st argument is taken.
    IF SIZE(tvar,/type) EQ 2 OR SIZE(tvar,/type) EQ 3 THEN tvar=tnames(tvar)
    IF STRLEN(tnames(tvar)) LT 2 THEN CONTINUE
    IF STRLOWCASE(STRMID(tvar, 0,3)) NE 'sd_' THEN CONTINUE
    
    ;Generate the tplot var. name for the beam_dir
    stn = STRMID(tvar, 3,3)
    suf = STRMID(tvar, 0,1, /reverse )
    beamdir_tvar_name = 'sd_'+stn+'_azim_no_'+suf
    
    IF STRLEN(tnames(beamdir_tvar_name)) LT 2 THEN CONTINUE
    
    get_data, beamdir_tvar_name, data=d
    bmidx = uniq( d.y, SORT(d.y) )
    
    get_data, tvar, data=dd, dl=dl, lim=lim
    
    FOR i=0L, N_ELEMENTS(bmidx)-1 DO BEGIN
    
      vn = tvar + '_azim' + STRING(d.y[bmidx[i]], '(I2.2)')
      ;print, vn
      idx = WHERE( d.y EQ d.y[bmidx[i]] )
      ;print, n_elements(idx)
      IF idx[0] EQ -1 THEN CONTINUE
      ;help, dd.x, dd.y
      store_data, vn, data={x:dd.x[idx], y:dd.y[idx,*,*], v:dd.v }, dl=dl, lim=lim
      options, vn, 'ytitle', STRUPCASE(stn)+' bm'+STRING(d.y[bmidx[i]], '(I2.2)')
      options, vn, 'ysubtitle', '[range gate]'
      
    ENDFOR
    
  ENDFOR
  
  
END


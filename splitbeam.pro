;+
; :DESCRIPTION:
;    Divide tplot variables including all beams into that for each beam.
;
; :EXAMPLE:
;    splitbeam, 'sd_hok_vlos_0'
;
; :Author:
;    Tomoaki Hori (E-mail: horit@isee.nagoya-u.ac.jp)
; :HISTORY:
;    2010/03/02: Created
;
; $LastChangedBy:$
; $LastChangedDate:$
; $LastChangedRevision:$
; $URL:$
;-
PRO splitbeam, tvars, ilnscan=ilnscan

  tvars = tnames(tvars)
  if strlen(tvars[0]) lt 6 then return

  
  FOR n=0, N_ELEMENTS(tvars)-1 DO BEGIN
    
                                ;Does tvar exist?
    tvar = tvars[n]
    IF SIZE(tvar, /type) EQ 2 OR SIZE(tvar, /type) EQ 3 THEN tvar = tnames(tvar)
    IF STRLEN(tnames(tvar)) LT 2 THEN CONTINUE               ;Skip if tplot var not found
    IF STRLOWCASE(STRMID(tvar, 0, 3)) NE 'sd_' THEN CONTINUE ;Skip if given non-SD data
    
                                ;Generate the tplot var. name for the beam_dir
    stn = STRMID(tvar, 3, 3)
    suf = STRMID(tvar, 0, 1, /reverse )
    beamdir_tvar_name = 'sd_'+stn+'_azim_no_'+suf
    
    IF STRLEN(tnames(beamdir_tvar_name)) LT 2 THEN CONTINUE
    
    get_data, beamdir_tvar_name, data=d
    bmidx = uniq( d.y, SORT(d.y) )
    
    get_data, tvar, data=data, dl=dl, lim=lim
    
    if ~keyword_set(ilnscan) then begin

      FOR i=0L, N_ELEMENTS(bmidx)-1 DO BEGIN
        
        azim_suf = '_azim' + STRING(d.y[bmidx[i]], '(I2.2)')
        vn = tvar + azim_suf
                                ;print, vn
        idx = WHERE( d.y EQ d.y[bmidx[i]] )
                                ;print, n_elements(idx)
        IF idx[0] EQ -1 THEN CONTINUE
                                ;help, dd.x, dd.y
        if is_struct(data) then begin
          dd = data
          store_data, vn, data={x:dd.x[idx], y:dd.y[idx, *], v:dd.v }, dl=dl, lim=lim
        endif else begin   ;;Cases of multi-tplot var containing both iono. and ground scatter data 
          vn_iono = data[ ( where( strpos(data, 'iscat') ge 0 ) )[0] ] ;& help, vn_iono
          vn_gscat = data[ ( where( strpos(data, 'gscat') ge 0 ) )[0] ] ;& help, vn_gscat
          get_data, vn_iono, data=dd, dl=dl, lim=lim
          get_data, vn_gscat, data=ddg, dl=dlg, lim=limg
          store_data, vn_iono+azim_suf, data={x:dd.x[idx], y:dd.y[idx, *], v:dd.v}, dl=dl,lim=lim
          store_data, vn_gscat+azim_suf, data={x:ddg.x[idx], y:ddg.y[idx, *],v:ddg.v},dl=dlg,lim=limg
          options, vn_iono+azim_suf, 'ytitle', STRUPCASE(stn)+'!Cbm'+STRING(d.y[bmidx[i]], '(I2.2)')
          options, vn_iono+azim_suf, 'ysubtitle', '[range gate]'
          store_data, vn, data=[ vn_iono+azim_suf, vn_gscat+azim_suf ]
        endelse
      
        options, vn, 'ytitle', STRUPCASE(stn)+'!Cbm'+STRING(d.y[bmidx[i]], '(I2.2)')
        options, vn, 'ysubtitle', '[range gate]'
        maxrg = max(dd.v,/nan)+1
        ylim, vn, [0,maxrg]
      
      ENDFOR
      
    ;; for interleaved normal scan
    endif else begin

      vn_cpid = 'sd_'+stn+'_cpid_'+suf
      get_data, vn_cpid, data=d & cpid = d.y
      get_data, beamdir_tvar_name, data=d & bmdir0 = d.y
      get_data, tvar, data=data0, dl=dl, lim=lim
      get_data, 'sd_'+stn+'_scanno_'+suf, data=d & scanno0 = d.y
      
      idx = where( abs(cpid) ge 190 and abs(cpid) lt 200, num )
      if num eq 0 then begin
        print, 'No interleaved normal scan data is found in ' + tvar + '!'
        continue
      endif
      t_dat = data0.x[idx]
      dat = data0.y[idx, *]
      v_dat =  data0.v[idx, *]
      data = {x:t_dat, y:dat, v:v_dat}
      bmdir = bmdir0[idx]
      scanno = scanno0[idx]

      idx = where( scanno eq scanno[2], num )
      bmlist = spd_uniq(bmdir[idx])
      nbm = n_elements(bmlist)
      bmpair = intarr( nbm/2, 2 )
      bmpair[*, 0] = bmlist[ 2*indgen( nbm/2  ) ]
      bmpair[*, 1] = bmlist[ 2*indgen( nbm/2  ) + 1 ]
      print,  'bmlist = ', bmlist
      print, 'bmpair: '
      print, bmpair
      
      for i=0L, n_elements(bmpair[*, 0])-1 do begin

        ilbm = reform( bmpair[i, *] )
        
        azim_suf = '_ilazim' + STRING(ilbm, '(2(I2.2))')
        vn = tvar + azim_suf
                                ;print, vn
        idx = WHERE( bmdir eq ilbm[0] or bmdir eq ilbm[1], num )
                                ;print, n_elements(idx)
        IF num EQ 0 THEN CONTINUE
                                ;help, dd.x, dd.y
        if is_struct(data) then begin
          if ndimen(v_dat) eq 1 then v = v_dat else v = v_dat[idx, *]
          store_data, vn, data={x:t_dat[idx], y:dat[idx, *], v:v }, dl=dl, lim=lim
        endif else begin                                                ;;Cases of multi-tplot var containing both iono. and ground scatter data 
          vn_iono = data[ ( where( strpos(data, 'iscat') ge 0 ) )[0] ]  ;& help, vn_iono
          vn_gscat = data[ ( where( strpos(data, 'gscat') ge 0 ) )[0] ] ;& help, vn_gscat
          get_data, vn_iono, data=dd, dl=dl, lim=lim
          get_data, vn_gscat, data=ddg, dl=dlg, lim=limg
          store_data, vn_iono+azim_suf, data={x:dd.x[idx], y:dd.y[idx, *], v:dd.v}, dl=dl,lim=lim
          store_data, vn_gscat+azim_suf, data={x:ddg.x[idx], y:ddg.y[idx, *],v:ddg.v},dl=dlg,lim=limg
          options, vn_iono+azim_suf, 'ytitle', STRUPCASE(stn)+'!Cbm'+STRING(d.y[bmidx[i]], '(I2.2)')
          options, vn_iono+azim_suf, 'ysubtitle', '[range gate]'
          store_data, vn, data=[ vn_iono+azim_suf, vn_gscat+azim_suf ]
        endelse
      
        options, vn, 'ytitle', STRUPCASE(stn)+'!CINscan!Cbm'+STRING(ilbm, '(I2.2,",",I2.2)')
        ;;options, vn, 'ysubtitle', '[range gate]'
        ;;maxrg = max(dd.v, /nan)+1
        ;;ylim, vn, [0, maxrg]
        
      endfor

    endelse
    
    
    
    
  ENDFOR
  
  
END


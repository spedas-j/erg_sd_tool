;+
; set_coords.pro
; 
; :Description:
; Transform the coordinate system of RTI-plot-type tplot variables
; 
; 
; :Author: T. Hori (horit@stelab.nagoya-u.acjp)
;
; :History: 2010/11/18
;
;-
pro set_coords, tplot_vars, coord

  npar = n_params()
  if npar eq 0 then begin
    print, 'Usage:'
    print, "       set_coords, ['var1','var2'...], {'gate'|'mlat'|'mlt'}"
    print, "e.g., set_coords, 'sd_hok_vlos_1', 'mlat'"
    return
  endif
  
  if ~keyword_set(coord) then coord = 'mlat'
  coord = strlowcase(coord)
  
  tplot_vars = tnames(tplot_vars)
  if tplot_vars[0] eq '' then return
  
  for i=0L, n_elements(tplot_vars)-1 do begin
    
    vn = tplot_vars[i]
    vnstr = strsplit(vn,'_')
    
    if vnstr[0] ne 'sd_' then continue ;non sd-tplot var
    
    ;get the radar name and the suffix
    stn = vnstr[1] ; radar name code
    prefix = 'sd_'+stn+'_'
    n = min( where( stregex(vnstr, '^((0|1|2|3|4|5|6|7|8|9)+)$') ge 0 ) )
    if n gt 1 then suf = vnstr[n] else begin
      print, 'Cannot find the RG suffix... skip!   ',vn  
      continue
    endelse
    is_azimvar = ( stregex(vn, '_azim(0|1|2|3|4|5|6|7|8|9){2}') ge 0 )
    
    ;Load the data to be drawn and to be used for drawing on a 2-d map
    get_data, vn, data=d, dl=dl, lim=lim
    get_data, prefix+'azim_no_'+suf, data=az
    get_data, prefix+'position_tbl_'+suf, data=tbl
    
    ;;;;;;;
    CASE (coord) OF
      'gate': BEGIN
        
        gateno = indgen( n_elements(d.v) )
        d.v = gateno
        store_data, vn, data=d, dl=dl, lim=lim
        options, vn, ysubtitle='[range gate]'
      END
      'mlat': begin
        
        if is_azimvar then begin
          azimno = fix( strmid( stregex(vn, 'azim(0|1|2|3|4|5|6|7|8|9){2}', /ext), 4,2 ) )
          
        endif else begin ;Cases of tplot vars containing all (0-15) beams
        
        endelse
        
      end
      ELSE: BEGIN
        
      END
    ENDCASE

    
  endfor
  
end


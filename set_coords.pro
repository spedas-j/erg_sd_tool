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
    print, "       set_coords, ['var1','var2'...], {'gate'|'mlat'}"
    print, "e.g., set_coords, 'sd_hok_vlos_1', 'mlat'"
    return
  endif
  
  if ~keyword_set(coord) then coord = 'mlat'
  coord = strlowcase(coord)
  
  tplot_vars = tnames(tplot_vars)
  if tplot_vars[0] eq '' then return
  
  for i=0L, n_elements(tplot_vars)-1 do begin
    
    vn = tplot_vars[i]
    vnstr = strsplit(vn,'_', /extract)
    
    if strmid(vn,0,3) ne 'sd_' then continue ;non sd-tplot var
    
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
    
    ;Get dimensions of the data/table arrays
    rgmax = n_elements(tbl.y[0,*,0,0])-1
    azmmax= n_elements(tbl.y[0,0,*,0])-1
    glonarr = ( reform( tbl.y[0, 0:(rgmax-1), *, 0]) + 360. ) mod 360. ;Array [rgmax, 17]
    glatarr = ( reform( tbl.y[0, 0:(rgmax-1), *, 1]) + 360. ) mod 360.
     
    ;Cases of Multi-tplot var --> call this pro recursively
    if (size(d))[2] eq 7 then begin
      for n=0, n_elements(d)-1 do begin
        set_coords, d[n], coord
        ;print, d[n], coord
      endfor
      get_data, d[0], lim=lim
      yr = minmax(lim.yrange)
      ylim, vn, yr[0],yr[1]
      CASE (coord) OF
        'gate': BEGIN
          options, vn, 'ysubtitle','[range gate]'
        END
        'mlat': BEGIN
          options, vn, 'ysubtitle','[Mag. Lat]'
        END
      ENDCASE

      return
    endif
    
    ;;;;;;;
    CASE (coord) OF
    
      'gate': BEGIN
        
        gateno = indgen( rgmax )
        
        str_element, d, 'v', gateno, /add_replace
        store_data, vn, data=d, dl=dl, lim=lim
        
        options, vn, 'yrange', minmax(gateno)
        options, vn, 'ystyle', 1
        options, vn, 'ysubtitle','[range gate]'
        
      END
      
      'mlat': begin
        
        if is_azimvar then begin
          azimno = fix( strmid( stregex(vn, 'azim(0|1|2|3|4|5|6|7|8|9){2}', /ext), 4,2 ) )
          glat = total( reform( glatarr[ *, azimno:(azimno+1)] ), 2) /2. ;simple average
          glon = total( reform( glonarr[ *, azimno:(azimno+1)] ), 2) /2. ;simple average
          ;GEO --> AACGM, assuming 400km
          aacgmconvcoord, glat, glon, replicate(400.,rgmax), mlat,mlon,err,/TO_AACGM
          
          str_element, d, 'v', mlat, /add_replace
          store_data, vn, data=d, dl=dl, lim=lim
          
        endif else begin ;Cases of tplot vars containing all (0-15) beams
          glat = reform( glatarr[*,0:(azmmax-1)] + glatarr[*,1:azmmax] ) /2.
          glon = reform( glonarr[*,0:(azmmax-1)] + glonarr[*,1:azmmax] ) /2.
          alt = glat & alt[*,*] = 400. ;km
          aacgmconvcoord, glat,glon,alt, mlat,mlon,err,/TO_AACGM
          if (size(mlat))[0] eq 0 then begin ; For Unix ver. AACGM DLM bug 
            mlat = reform(mlat,rgmax,azmmax) & mlon = reform(mlon,rgmax,azmmax)
          endif
          
          newv = d.y & newv[*,*] = !values.f_nan ;Create an array of the same dimension and initialize it 
          for n=0, azmmax-1 do begin
            idx = where( az.y eq n ) & if idx[0] eq -1 then continue
            newv[idx,*] = replicate(1.,n_elements(idx)) # transpose(mlat[*,n])
          endfor
          
          str_element, d, 'v', newv, /add_replace
          store_data, vn, data=d, dl=dl, lim=lim
          
        endelse
        
        yr = minmax(d.v)
        ylim, vn, yr[0],yr[1] 
        options, vn, 'ystyle', 1
        options, vn, ysubtitle='[Mag. Lat]'
      end
      
      ELSE: BEGIN
        print, "Only 'gate' and 'mlat' are available for keyword COORD"
        return
      END
      
    ENDCASE

    
  endfor
  
end


;+
; FUNCTION aacgmmlt 
; 
; :PURPOSE:
; A wrapper procedure to choose AACGM DLM or IDL-native routines 
; to convert a AACGM longitude to AACGM MLT.   
; 
; The wrapper procedures/functions check !sdarn.aacgm_dlm_exists 
; (if not defined, then define it by sd_init) to select appropriate 
; AACGM routines (DLM, or IDL native ones attached to TDAS). 
; 
; :AUTHOR: 
;   Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
; 
; :Params:
;   yr:     4-digit year for which MLT is calculated.
;   t0:     Second of year for which MLT is calculated.
;   mlon:   AACGM longitude to be converted to MLT.
; 
; :Examples:
;   mlt = aacgmmlt( yr, t0, mlon )
;   
; :HISTORY:
;   2011/10/04: created and got through the initial bug fixes
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-


function aacgmmlt, yr, t0, mlon

;Initialize !sdarn if not defined
help, name='!sdarn',out=out
if out eq '' then sd_init

if !sdarn.aacgm_dlm_exists then begin 
  ;print, 'using AACGM_DLM'
  return, aacgm_mlt(yr,t0,mlon)
endif else begin
  mlt=mlon
  mlt[*]=0.
  for i=0L, n_elements(mlon)-1 do begin
    tmlt = calc_mlt(yr[i],t0[i],mlon[i])
    mlt[i]=tmlt
    ;print, 'cnv_aacgm was executed'
    ;print, glat[i],glon[i],alt[i],'   ',mlat[i],mlon[i],r,err
  endfor
  return, mlt
endelse

return, !values.f_nan ;error 
end


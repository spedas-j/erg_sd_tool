;+
; PROCEDURE sd_aacgmlib 
; 
; :DESCRIPTION:
; A dummy procedure to compile the AACGM wrapper procedure/function 
; defined below by just type "sd_aacgmlib" 
; 
; The wrapper procedures/functions check !sdarn.aacgm_dlm_exists 
; (if not defined, then define it by sd_init) to select appropriate 
; AACGM routines (DLM, or IDL native ones attached to TDAS). 
; 
; :AUTHOR: 
;   Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
;   
; :HISTORY:
;   2010/12/02: created and got through the initial bug fixes
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-

pro aacgmconvcoord, glat,glon,alt,mlat,mlon,err, TO_AACGM=TO_AACGM, TO_GEO=TO_GEO

;Initialize !sdarn if not defined
help, name='!sdarn',out=out
if out eq '' then sd_init

glon = (glon + 360.) mod 360.

if !sdarn.aacgm_dlm_exists then begin 
  ;print, 'using AACGM_DLM'
  aacgm_conv_coord, glat,glon,alt,mlat,mlon,err,$
    TO_AACGM=TO_AACGM, TO_GEO=TO_GEO
endif else begin
  mlat=glat & mlon=glon
  mlat[*]=0. & mlon[*]=0.
  for i=0L, n_elements(glat)-1 do begin
    cnv_aacgm,glat[i],glon[i],alt[i],tmlat,tmlon,r,err,geo=TO_GEO
    mlat[i]=tmlat & mlon[i]=tmlon
    ;print, 'cnv_aacgm was executed'
    ;print, glat[i],glon[i],alt[i],'   ',mlat[i],mlon[i],r,err
  endfor
endelse

mlon = (mlon + 360.) mod 360.

return
end



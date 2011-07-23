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
pro aacgmloadcoef, year

;Initialize !sdarn if not defined
help, name='!sdarn',out=out
if out eq '' then sd_init

;Only AACGM DLM has a subroutine to load the S-H coefficients for given year
if !sdarn.aacgm_dlm_exists then begin 
  ;print, 'using AACGM_DLM'
  aacgm_load_coef, year
endif 

return
end

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

function aacgmmlong, yr, t0, mlt

;Initialize !sdarn if not defined
help, name='!sdarn',out=out
if out eq '' then sd_init

if !sdarn.aacgm_dlm_exists then begin 
  ;print, 'using AACGM_DLM'
  tmlon = aacgm_mlong(yr,t0,mlt)
  tmlon = ( tmlon + 360. ) mod 360.
  return, tmlon
endif else begin
  cnv_aacgm,0.0,0.0,0.001,mlat0,mlon0,r,err,/geo
  mlon0 = ( mlon0 + 360. ) mod 360.
  mlon=mlt
  mlon[*]=0.
  for i=0L, n_elements(mlon)-1 do begin
    tmlt = ( mlt[i] + 24. ) mod 24.
    mlt0 = calc_mlt(yr[i],t0[i],mlon0)
    mlt0 = ( mlt0 + 24. ) mod 24.
    tmlon = ( (tmlt - mlt0 +24.) mod 24. )/24.*360.
    mlon[i]=tmlon
    ;print, 'cnv_aacgm was executed'
    ;print, glat[i],glon[i],alt[i],'   ',mlat[i],mlon[i],r,err
  endfor
  return, mlon
endelse

return, !values.f_nan ;error 
end

;This is just to compile the above pro/fun by executing. 
pro sd_aacgmlib, quiet=quiet

if ~keyword_set(quiet) then print, 'SD_AACGMLIB compiled.'

return
end


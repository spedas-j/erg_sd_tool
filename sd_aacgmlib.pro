pro aacgmconvcoord, glat,glon,alt,mlat,mlon,err, TO_AACGM=TO_AACGM, TO_GEO=TO_GEO

;Initialize !sdarn if not defined
help, name='!sdarn',out=out
if out eq '' then sd_init

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



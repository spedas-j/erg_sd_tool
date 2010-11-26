pro aacgmconvcoord, glat,glon,alt,mlat,mlon,err, TO_AACGM=TO_AACGM, TO_GEO=TO_GEO

;Initialize !sdarn if not defined
help, name='!sdarn',out=out
if out eq '' then sd_init

if !sdarn.aacgm_dlm_exists then begin 
  aacgm_conv_coord, glat,glon,alt,mlat,mlon,err,



return
end



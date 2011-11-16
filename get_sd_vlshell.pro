PRO get_sd_vlshell, stn, angle_var=angle_var, exclude_angle=exclude_angle, glatp=glatp,glonp=glonp,glatm=glatm,glonm=glonm
  
  ;Check the argument
  npar = n_params()
  if npar ne 1 then return
  vlos_vn = tnames('sd_'+strlowcase(stn)+'_vlos_?')
  if strlen(tnames(vlos_vn)) lt 6 then begin
    print, 'Cannnot find the vlos variable: '+vlos_vn
    return
  endif

  ;Inisitalize the AACGM lib
  sd_init
  aacgmloadcoef, 2005
  
  ;Variable names
  prefix = 'sd_'+strlowcase(stn)+'_'
  suf = strmid(vlos_vn, 0,1,/reverse)
  vlos_vn = prefix+'vlos_'+suf
  azmno_vn = prefix+'azim_no_'+suf
  ptbl_vn = prefix+ 'position_tbl_'+suf
  ctbl_vn = prefix+ 'positioncnt_tbl_'+suf
  
  ;Get variable values
  get_timespan, tr ;Time range
  get_data, vlos_vn, data=d, lim=vlos_lim
  vlos = d.y & vlos_v = d.v &  vlos_time = d.x 
  get_data, azmno_vn, data=d
  azmno = d.y
  get_data, ptbl_vn, data= d 
  ptbl = reform(d.y[0,*,*,*])
  get_data, ctbl_vn, data= d 
  ctbl = reform(d.y[0,*,*,*])
  
  ;Number of range gate and beam 
  nrang = n_elements(ctbl[*,0,0])
  azmmax = n_elements(ctbl[0,*,0] )
  
  posm = fltarr( nrang, azmmax, 2)
  posp = fltarr( nrang, azmmax, 2)
  
  for i=0L, nrang-1 do begin
    for j=0L, azmmax-1 do begin
    
      pos = get_sphcntr( [ ptbl[i,j,1],ptbl[i,j+1,1] ], [ ptbl[i,j,0],ptbl[i,j+1,0] ] )
      posm[i,j,0] = pos[1] & posm[i,j,1] = pos[0]
      pos = get_sphcntr( [ ptbl[i+1,j,1],ptbl[i+1,j+1,1] ], [ ptbl[i+1,j,0],ptbl[i+1,j+1,0] ] )
      posp[i,j,0] = pos[1] & posp[i,j,1] = pos[0]
      
    endfor
  endfor
  
  ;height
  if ~keyword_set(alt) then alt = 400. ;[km]
  earth_radii = 6371.2 ;km
  r = (alt + earth_radii)/earth_radii ;[Re]
  
;;;Beam vector for each pixel in GEO
  xm = r*cos(posm[*,*,1]*!dtor)*cos(posm[*,*,0]*!dtor)
  ym = r*cos(posm[*,*,1]*!dtor)*sin(posm[*,*,0]*!dtor)
  zm = r*sin(posm[*,*,1]*!dtor)
  xp = r*cos(posp[*,*,1]*!dtor)*cos(posp[*,*,0]*!dtor)
  yp = r*cos(posp[*,*,1]*!dtor)*sin(posp[*,*,0]*!dtor)
  zp = r*sin(posp[*,*,1]*!dtor)
  beam_dx_geo = xp-xm   ;Beam dir at the center of each pixel in GEO
  beam_dy_geo = yp-ym
  beam_dz_geo = zp-zm
  t = sqrt(beam_dx_geo^2+beam_dy_geo^2+beam_dz_geo^2)
  beam_dx_geo /= t & beam_dy_geo /= t & beam_dz_geo /= t ;normalized
  
;;;Get unit vectors of the "AACGM MLT direction" in GEO
   
  glat = ctbl[*,*,1] & glon = ctbl[*,*,0]
  altarr = glat & altarr[*,*] = alt
  aacgmconvcoord, glat,glon,altarr, mlat,mlon,err,/TO_AACGM
  mlon = ( mlon + 360. ) mod 360.
  mlonp = mlon + 0.15 & mlonp = ( mlonp + 360. ) mod 360. 
  mlonm = mlon - 0.15 & mlonm = ( mlonm + 360. ) mod 360. 
  aacgmconvcoord, mlat,mlonp,altarr,glatp,glonp,err, /TO_GEO
  aacgmconvcoord, mlat,mlonm,altarr,glatm,glonm,err, /TO_GEO
  ;return
  xp = r*cos(glatp*!dtor)*cos(glonp*!dtor)
  yp = r*cos(glatp*!dtor)*sin(glonp*!dtor)
  zp = r*sin(glatp*!dtor)
  xm = r*cos(glatm*!dtor)*cos(glonm*!dtor)
  ym = r*cos(glatm*!dtor)*sin(glonm*!dtor)
  zm = r*sin(glatm*!dtor)
  mltdir_dx_geo = xp-xm   ;AACGM MLT dir at the center of each pixel in GEO
  mltdir_dy_geo = yp-ym   ;Positive is eastward
  mltdir_dz_geo = zp-zm
  t = sqrt(mltdir_dx_geo^2+mltdir_dy_geo^2+mltdir_dz_geo^2)
  mltdir_dx_geo /= t & mltdir_dy_geo /= t & mltdir_dz_geo /= t ;normalized
  
;;;Get an angle between VLOS and MLT dir
  cos_vlos_mltdir = beam_dx_geo*mltdir_dx_geo + beam_dy_geo*mltdir_dy_geo $
    + beam_dz_geo*mltdir_dz_geo   ;as a 2-D array in [nrang,azmax] 
  
;;;Get V_Lshell values
  cos_vlos_mltdir_arr = (transpose(cos_vlos_mltdir))[ azmno, * ]
  vlshell = -vlos / cos_vlos_mltdir_arr
  ;help, azmno, cos_vlos_mltdir, cos_vlos_mltdir_arr, vlos
  
  ;Exclude Vlshell values with mltdir-bmdir angle satisfying the 
  ;exclude angle range given by the keyword "exclude_angle"
  if keyword_set(exclude_angle) then begin
    if n_elements(size(exclude_angle)) ne 4 then begin
      print, 'exclude_angle is invalid!'
      exclude_angle=0
    endif
  endif
  if keyword_set(exclude_angle) then begin
    angrng = exclude_angle
    sz = size(angrng)
    if sz[1] eq 2 and sz[3] eq 2 then begin
      angrng = minmax(angrng)
      angarr = acos(cos_vlos_mltdir_arr)*!radeg
      idx = where( angarr ge angrng[0] and angarr le angrng[1] )
      if idx[0] ne -1 then vlshell[idx] = !values.f_nan    
    endif else begin
      print, 'exclude_angle is invalid!'
      help, angrng
      print, '...ignored'
    endelse
  endif
  
;;;Store into a tplot variable
  vlshell_vn = prefix+'vlshell_'+suf
  if strlen(tnames(vlshell_vn)) gt 6 then $
    store_data, delete=vlshell_vn
  store_data, vlshell_vn, $
    data={x: vlos_time, y: vlshell, v: vlos_v}, $
    dl={spec:1}, $
    lim={ytitle:vlos_lim.ytitle, ysubtitle:vlos_lim.ysubtitle, $
      ztitle:'V_Lshell [m/s]', zrange:[-200.,200.] }
  
  if keyword_set(angle_var) then begin
    angle_var_vn = prefix+'mltdir-bmdir_angle_'+suf
    if strlen(tnames(angle_var_vn)) gt 6 then store_data,del=angle_var_vn
    store_data, angle_var_vn, $
      data={x: vlos_time, y: acos(cos_vlos_mltdir_arr)*!radeg, $
        v: vlos_v}, $
      dl={spec:1}, $
      lim={ytitle:vlos_lim.ytitle, ysubtitle:vlos_lim.ysubtitle, $
      ztitle:'MLTdir-bmdir!Cangle [deg]', zrange:[0.,180.] }
  endif
  
  return
end
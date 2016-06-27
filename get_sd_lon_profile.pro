;+
; FUNCTION get_sd_lon_profile
;
; :Description:
;   Obtain a tplot variable storing a time series of the values
;   averaged over the given latitude/longitude range, to be plotted as a RTI-type plot.
;   This procedure calls get_sd_ave() internally. 
;
; :PARAMTERS:
; vn : a tplot variable the values in which are to be averaged
;
; :KEYWORD:
; lonrng: the geographical latitude range for which the given values are averaged
; dlon:   longitudinal width of each longitudinal bin for which the average values are obtained.  
; latrng: the geographical latiitude range for averaging
; maglat: Set this keyword if you give the latrng in magnetic latitude, not in geographical latitude. 
; maglon: Set this keyword if  you give the lonrng in magnetic longitude. 
; new_vn: Set a string to create a new tplot variable containing the averaged values
;
; :EXAMPLES:
;   erg_load_sdfit, site='hok',/get
;   get_sd_lon_profile, 'sd_hok_vlos_1', lonrng=[140,170], dlon=1., latrng=[55,57], /maglat
;
; :Author:
;   Tomo Hori (E-mail: horit@isee.nagoya-u.ac.jp)
;
; :HISTORY:
;   2016/06/20: Created
;
; $LastChangedBy:$
; $LastChangedDate:$
; $LastChangedRevision:$
; $URL:$
;-
PRO get_sd_lon_profile, vn, latrng=latrng, lonrng=lonrng, dlon=dlon, maglat=maglat, maglon=maglon, new_vn=new_vn
  
  ;Currently this procedure can take as an argument:
  ; vlos, vlshell, (vlos|vnorth|veast), _iscat
  
  ;Check the arguments and keywords
  if tnames(vn[0]) eq '' then return
  vn0 = tnames(vn[0])
  vn = vn0
  
  if ~keyword_set(latrng) or ~keyword_set(lonrng) then return
  
  if ~keyword_set(dlon) then dlon = 2.
  
  if n_elements(latrng) ne 2 or n_elements(lonrng) ne 2 then return
  
  lonrng = minmax(float(lonrng))
  dlon = abs(dlon)
  tlonarr = ( lonrng[0] + dlon* findgen(ceil( (lonrng[1]-lonrng[0])/dlon )+1) ) < lonrng[1]
 
  ;print, tlatarr
  
  ;Generate the time-lat array
  scan = get_scan_struc_arr(vn) 
  valarr = fltarr( n_elements(scan.x), n_elements(tlonarr)-1 )
  nlon = n_elements(tlonarr)
  lonc = (tlonarr[1:(nlon-1)]+tlonarr[0:(nlon-2)])/2.
  ;print, latc
  
  for i=0L, n_elements(tlonarr)-2 do begin
    
    lonmin = tlonarr[i] & lonmax = tlonarr[i+1]
    lonave = get_sd_ave(vn, lonrng=[lonmin,lonmax],latrng=latrng,$
                          maglat=maglat, maglon=maglon )
    valarr[*,i] = lonave.y
    
  endfor
  
  ;Store the time-lat arr in a tplot var
  if ~keyword_set(new_vn) then $
    new_vn = vn +'_lonpro_lat'+string(latrng[0],'(I03)')+'-'+$
      string(latrng[1],'(I03)')
      
  store_data, new_vn, data={x: scan.x, y:valarr, v:lonc}, $
    dl={spec:1}, $
    lim={zrange:[-300,300]}
    
  return
end

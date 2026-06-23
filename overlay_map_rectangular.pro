;+
; PRO overlay_map_rectangular
;   to draw a rectangular bounded by a set of Mlats and Mlons on 
;   map grids defined by sd_map_set or map2d_set
;
; Usage: 
;  overlay_map_rectangular, mlatrng=[63., 64.], mlonrng=[305., 340.]  ; for FHW
; 
;-
pro overlay_map_rectangular, $
   mlatrng=mlatrng, mlonrng=mlonrng, $
   linethick=linethick, $
   debug=debug


  if undefined(linethick) then linethick = 3
  
  ts = time_struct( !map2d.time )
  yr0 = ts.year
  ys = long( !map2d.time - time_double( string(yr0, '(i04)') + '-01-01/00:00') )

  ;;mlatrng = [ 67.5, 68.5 ]
  ;;mlonrng = [ 285., 335. ]
  nlon = 81
  mlongrid = mlonrng[0] + (mlonrng[1]-mlonrng[0])*findgen(nlon)/(nlon-1)
  ysarr = replicate( ys, nlon )
  yrarr = replicate( yr0, nlon )
  
  for k=0, n_elements(mlatrng)-1 do begin
    mlatgrid0 = mlongrid & mlatgrid1 = mlongrid
    mlatgrid0[*] = mlatrng[0] & mlatgrid1[*] = mlatrng[1]
    alt = mlongrid & alt[*] = 0.001
    
    mltsm = aacgmmlt(yrarr, ysarr, mlongrid ) /24*360
    plots, mltsm, mlatgrid0, linestyle=0, thick=linethick
    plots, mltsm, mlatgrid1, linestyle=0, thick=linethick
    plots, [mltsm[0], mltsm[0]], [mlatgrid0[0], mlatgrid1[0]], linestyle=0, thick=linethick
    plots, [mltsm[nlon-1], mltsm[nlon-1]], [mlatgrid0[nlon-1], mlatgrid1[nlon-1]], linestyle=0, thick=linethick
    
    ;;xyouts, mltsm[0]-7, mlatgrid[0]+0.5, 'MLAT!C57-58', $
    ;;        orien=10., charthick=2., charsize=2.2
    
  endfor


end

;+
;PROCEDURE specplot,x,y,z
;NAME:
;   specplot
;PURPOSE:
;   Creates a spectrogram plot.
;   All plot limits and plot positions are handled by the keyword LIMITS.
;INPUT:
;   x:  xaxis values:  dimension N.
;   y:  yaxis values:  dimension M.  (Future update will allow (N,M))
;   Z:  color axis values:  dimension (N,M).
;
;   All options are passed in through a single structure.
;KEYWORDS:
;   LIMITS:  A structure that may contain any combination of the following
;       elements:
;       X_NO_INTERP:   Prevents interpolation along the x-axis.
;       Y_NO_INTERP:   Prevents interpolation along the y-axis.
;       NO_INTERP:     Prevents interpolation along either axis
;       NO_COLOR_SCALE: Prevents drawing of color bar scale.
;       BOTTOM, TOP:   Sets the bottom and top colors for byte-scaling
;       ALL plot keywords such as:
;       XLOG,   YLOG,   ZLOG,
;       XRANGE, YRANGE, ZRANGE,
;       XTITLE, YTITLE,
;       TITLE, POSITION, REGION  etc. (see IDL documentation for a description)
;         The following elements can be included in LIMITS to effect DRAW_COLOR_SCALE:
;       ZTICKS, ZRANGE, ZTITLE, ZPOSITION, ZOFFSET
;   DATA:  A structure that provides an alternate means of supplying
;       the data and options.  This is the method used by "TPLOT".
;   X_NO_INTERP:   Prevents interpolation along the x-axis.
;   Y_NO_INTERP:   Prevents interpolation along the y-axis.
;   OVERPLOT:      If non-zero then data is plotted over last plot.
;   OVERLAY:       If non-zero then data is plotted on top of data from last
;        last plot.
;   PS_RESOLUTION: Post Script resolution.  Default is 150.
;   NO_INTERP:     If set, do no x or y interpolation.
;   IGNORE_NAN:    If nonzero, ignore data points that are not finite.
;   DX_GAP_SIZE = Maximum time gap over which to interpolate the plot. Use this
;     keyword when overlaying spectra plots, allowing the underlying spectra to
;     be shown in the data gaps of the overlying spectra.  Overrides value set
;     by DATAGAP in dlimits.  Note: if either DX_GAP_SIZE or DATAGAP is set to
;     less than zero, then the 20 times the smallest delta x is used.
;
;Notes:
;  - The arrays x and y MUST be monotonic!  (increasing or decreasing)
;  - The default is to interpolate in both the x and y dimensions.
;  - Data gaps can be included by setting the z values to NAN  (!values.f_nan).
;  - If ZLOG is set then non-positive zvalues are treated as missing data.
;
;See Also:  "XLIM", "YLIM", "ZLIM",  "OPTIONS",  "TPLOT", "DRAW_COLOR_SCALE"
;Author:  Davin Larson,  Space Sciences Lab
; $LastChangedBy: bckerr $
; $LastChangedDate: 2009-06-16 16:46:31 -0700 (Tue, 16 Jun 2009) $
; $LastChangedRevision: 6230 $
; $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/ssl_general/tags/tdas_5_20/tplot/specplot.pro $
;-
pro specplot,x,y,z,limits=lim,data=data,overplot=overplot,overlay=overlay,$
    ps_resolution=ps_res,x_no_interp=x_no_interp,y_no_interp=y_no_interp, $
        no_interp=no_interp, ignore_nan=ignore_nan, $
        dx_gap_size=dx_gap_size

opt = {xrange:[0.,0.],yrange:[0.,0.],zrange:[1.,1.]}

if keyword_set(dx_gap_size) then dg=dx_gap_size else str_element,lim,'datagap',dg

if keyword_set(data) then begin
  x = struct_value(data,'x')
  y = struct_value(data,'v')
  z = struct_value(data,'y')
  if not keyword_set( y ) then begin
       y = struct_value(data,'v2')    ;bp
       z = total(z,2)
  endif
  extract_tags,opt,data,except=['x','y','v']
  if keyword_set(dx_gap_size) then dg=dx_gap_size else str_element,lim,'datagap',dg
  ;str_element,lim,'datagap',dg ; old way
  ;if keyword_set(dg) then makegap,dg,x,z,v=y   ;bpif keyword_set(dg)
endif

if keyword_set(no_interp) then begin
   x_no_interp=1
   y_no_interp=1
endif

; Find where gaps are
if keyword_set(dg) then begin

   ;dg = max_gap_interp
   ;dt = median(x[1:*]-x)
   tdif = [x[1:*]-x[0:n_elements(x)-2]]
   
   ; set minimum gap interp to twice median sampling rate in current trange
   ;if dg lt 2*dt then dg = 2*dt
   
   ; set dg to 20 times the smallest dx if datagap and/or dx_gap_size is negative
   if dg lt 0 then begin
      posindx = where(tdif gt 0,poscnt)
      dg = 20d*min(tdif(posindx))
   endif
   
   dprint,verbose=verbose,'No plot interpolation for data gaps longer than ', $
          strcompress(dg,/remove_all),' seconds.'
   
   gapindx = where(tdif gt dg, gapcnt)
   
   if gapcnt gt 0 then begin
      ; create separate vars
      seg0 = lonarr(gapcnt+1) ; index numbers of start of each data segment 
      seg1 = seg0             ; index numbers of end of each data segment
      seg1[gapcnt] =  n_elements(x)-1
      for i=0L,gapcnt-1 do begin
      ;TODO: Need to account for "consecutive gaps" to reduce # of segments 
      ;      (gapcnt) and speed up the main loop for time windows with lots of
      ;      data with sample intervals greater than DATAGAP flag and/or
      ;      DX_GAP_SIZE.
         seg0[i+1] = gapindx[i]+1
         seg1[i] = gapindx[i] 
      endfor
 
   endif else begin
      ; prepare for only single iteration in for loop
      seg0 = 0L
      seg1 = n_elements(x)-1
   endelse
   
endif else begin
   ; prepare for only single iteration in for loop
   gapcnt = 0
   seg0 = 0L
   seg1 = n_elements(x)-1
endelse

;copy to temp variable
xtemp = x
ytemp = y
ztemp = z

ydim = size(y,/n_dim)

for j=0L,gapcnt do begin

   x = xtemp[seg0[j]:seg1[j]]
   if ydim eq 1 then y=ytemp else y=ytemp[seg0[j]:seg1[j],*]
   z = ztemp[seg0[j]:seg1[j],*]

   if n_params() eq 1 then begin
     dim = dimen(x)
     specplot,findgen(dim(0)),findgen(dim(1)),x,limits=lim,overplot=overplot,$
          overlay=overlay,ps_resolution=ps_res, $
          x_no_interp=x_no_interp,y_no_interp=y_no_interp
     return
   endif
   
   extract_tags,opt,lim
   
   if opt.xrange(0) eq opt.xrange(1) then opt.xrange = minmax(x)
   if opt.yrange(0) eq opt.yrange(1) then opt.yrange = minmax(y)
   
   ;str_element,opt,'ytype',value=ylog   ; obsolete keywords
   ;str_element,opt,'xtype',value=xlog
   ;str_element,opt,'ztype',value=zlog
   
   str_element,opt,'xlog',value=xlog
   str_element,opt,'ylog',value=ylog
   str_element,opt,'zlog',value=zlog
   
   str_element,opt,'gifplot',value=gifplot
   if keyword_set(gifplot) then begin
     x_no_interp = 1
     y_no_interp = 1
     no_color_scale = 1
   endif
   
   str_element,opt,'x_no_interp',value=x_no_interp
   str_element,opt,'y_no_interp',value=y_no_interp
   str_element,opt,'no_interp',value=no_interp
   if keyword_set(no_interp) then begin
      x_no_interp=1
      y_no_interp=1
   endif
   
   
   str_element,opt,'max_value',value=mx
   str_element,opt,'min_value',value=mn
   
   ;if keyword_set(mx) then print,'max_value= ', mx
   
   str_element,opt,'ztitle',value=ztitle
   str_element,opt,'bottom',value=bottom
   str_element,opt,'top',   value=top
   
   if not keyword_set(overplot) then box,opt     ; Sets plot parameters.
   
   
   zrange = opt.zrange
   y1 = y
   if keyword_set(ylog) then begin
     bad = where( finite(y1) eq 0, c)
     if c ne 0 then y1(bad) = 0.
     bad = where(y1 le 0,c)
     if c ne 0 then y1(bad) = !values.f_nan
     y1 = alog10(y1)
   endif
   
   if keyword_set(xlog) then x1 = alog10(x) else x1 = x
   
   str_element,opt,'minzlog',value=minzlog
   z1 = z
   if keyword_set(zlog) then begin
      bad = where( finite(z1) eq 0, cbad)
      if cbad ne 0 then z1[bad] = !values.f_nan
      neg = where(z1 le 0,cneg)
      if keyword_set(minzlog) then begin
          posrange = minmax(z1,/pos)
          negvals = posrange[0]/10.
      endif else negvals = !values.f_nan
      if cneg ne 0 then z1[neg] = negvals
      z1 = alog10(z1)
      zrange = alog10(zrange)
      if keyword_set(mn) then mn = alog10(mn)
      if keyword_set(mx) then mx = alog10(mx)
   endif
   
   xwindow=!x.window
   ywindow=!y.window
   xcrange=!x.crange
   ycrange=!y.crange
   
   ;str_element,opt,'overlay',value=overlay
   overlay = struct_value(opt,'overlay',default=1)
   
   ; need to be in overlay mode if stitching multiple segments together
   if gapcnt gt 0 then overlay=1
   
   if keyword_set(overlay) then begin
      winpos = convert_coord(minmax(x),minmax(y),/data,/to_norm)
      xwr = minmax(winpos(0,*))
      ywr = minmax(winpos(1,*))
   ;   xwindow(0) = xwindow(0) > xwr(0)
   ;   xwindow(1) = xwindow(1) < xwr(1)
      xwindow = xwindow > xwr[0]
      xwindow = xwindow < xwr[1]
      ywindow(0) = ywindow(0) > ywr(0)
      ywindow(1) = ywindow(1) < ywr(1)
      datpos = convert_coord(xwindow,ywindow,/norm,/to_data)
      xcrange = reform(datpos(0,*))
      ycrange = reform(datpos(1,*))
      if !x.type then xcrange = alog10(xcrange)
      if !y.type then ycrange = alog10(ycrange)
   endif
   
   
   pixpos = round(convert_coord(xwindow,ywindow,/norm,/to_device))
   npx = pixpos(0,1)-pixpos(0,0)+1
   npy = pixpos(1,1)-pixpos(1,0)+1
   xposition = pixpos(0,0)
   yposition = pixpos(1,0)
   
   if npx gt 0 and npy gt 0 then begin
   
      str_element,opt,'ignore_nan',ignore_nan
      if keyword_set(ignore_nan) then begin
         wg = where(finite(total(z1,2)),c)
         if c gt 0 then begin
           z1 = z1[wg,*]
           y1 = y1[wg,*]
           x1 = x1[wg]
         endif
      endif
      
      if !d.flags and 1 then begin   ; scalable pixels (postscript)
         if keyword_set(ps_res) then ps_resolution=ps_res else  ps_resolution = 150.  ; Postscript defaults to 150 dpi
         str_element,opt,'ps_resolution',value=ps_resolution
         dprint,dlevel=4,ps_resolution
         scale = ps_resolution/!d.x_px_cm/2.54
      endif else scale = 1.
      
      yd = ndimen(y1)
      if yd eq 1 then begin            ; Typical, y does not vary with time
        nypix = round(scale*npy)
        ny = n_elements(y1)
        yp = findgen(nypix)*(ycrange(1)-ycrange(0))/(nypix-1) + ycrange(0)
        ys = interp(findgen(ny),y1,yp)
        if keyword_set(y_no_interp) then  ys = round(ys)
      
        nxpix = round(scale*npx)
        if nxpix eq 0 then begin
           dprint, verbose=verbose, dlevel=2,'WARNING: Data segment ',strcompress(j,/remove_all),' is too small along the  x-axis',string(13B),$
           '   for the given time window or is not within the given window.  Nothing will be',string(13B),$
           '   plotted.  Try making the x-axis window smaller, or if creating a postscript',string(13B),$
           "   file, try increasing the 'ps_resolution' value using the OPTIONS command."
           no_color_scale=1
           continue
        endif
        nx = n_elements(x1)
        xp = findgen(nxpix)*(xcrange(1)-xcrange(0))/(nxpix-1) + xcrange(0)
        xs = interp(findgen(nx),x1,xp )
        if keyword_set(x_no_interp) then  xs = round(xs)
        image = interpolate(float(z1),xs,ys,missing = !values.f_nan,/grid)  ; using float( ) to fix IDL bug.
      
      ;  str_element,opt,'roi',roi
      ;  if keyword_set(roi) then begin
      ;     xp_ = xp # replicate(1.,nypix)
      ;     yp_ = replicate(1.,nxpix) # yp
      ;     roi_x = keyword_set(xlog) ? alog10(roi[*,0]) : roi[*,0]
      ;     roi_y = keyword_set(ylog) ? alog10(roi[*,1]) : roi[*,1]
      ;     dummy = enclosed(xp_,yp_,roi_x,roi_y,ncircs=ncirc)
      ;     image[where(ncirc eq 0)] = !values.f_nan
      ;  endif
      
      endif else begin
      ;  starttime = systime(1)
      ;  message,'y is 2 dimensional.  Please be patient...',/info
      
        nypix = round(scale*npy)
        ny = dimen2(y1)
        yp = findgen(nypix)*(ycrange(1)-ycrange(0))/(nypix-1) + ycrange(0)
        nxpix = round(scale*npx)
        if nxpix eq 0 then begin
           dprint, verbose=verbose, dlevel=2,'WARNING: Data segment ',strcompress(j,/remove_all),' is too small along the  x-axis',string(13B),$
           '   for the given time window or is not within the given window.  Nothing will be',string(13B),$
           '   plotted.  Try making the x-axis window smaller, or if creating a postscript',string(13B),$
           "   file, try increasing the 'ps_resolution' value using the OPTIONS command."
           no_color_scale=1
           continue
        endif
        nx = n_elements(x1)
        xp = findgen(nxpix)*(xcrange(1)-xcrange(0))/(nxpix-1) + xcrange(0)
        xs = interp(findgen(nx),x1,xp)
        xs = xs # replicate(1.,nypix)
        bad = where(finite(xs) eq 0,c)
        if c ne 0 then xs(bad)=-1
        if keyword_set(x_no_interp) then  xs = round(xs)
      
        ys = replicate(-1.,nxpix,nypix)
        ny1 = dimen1(y1)
        y_ind = findgen(ny)
        xi = round(xs)
        for i=0l,nxpix-1 do begin
          m = (xi(i) > 0) < (ny1-1)
          yt1 = reform(y1(m,*))
          ys(i,*) = interp(y_ind,yt1,yp)
        endfor
      ;dtime = systime(1)-starttime
      ;message,string(dtime)+' seconds.',/info
      
        bad = where(finite(ys) eq 0,c)
        if c ne 0 then ys(bad)=-1
        if keyword_set(y_no_interp) then  ys = round(ys)
        image = interpolate(float(z1),xs,ys,missing = !values.f_nan)
      
      endelse
      
      
      if not keyword_set(gifplot) then begin
      
        if zrange(0) eq zrange(1) then $
           zrange = minmax(image,max=mx,min=mn)
        image = bytescale(image,bottom=bottom,top=top,range=zrange)
      
      endif
      
      ;if fill_color defined, fill all pixels with the same color specified by fill_color
      str_element,opt,'fill_color',value=fill_color
      if ~keyword_set(fill_color) then fill_color = -1
      if fill_color ge 0 then begin
        idx = where(image lt 255) & if idx[0] ne -1 then image[idx]=fill_color
        no_color_scale = 1
      endif
      
      ;printdat,image,xposition,yposition
      if xposition ge 0 and yposition ge 0 and xposition lt !d.x_size and yposition lt !d.y_size then begin
        if fill_color lt 0 then begin
          tv,image,xposition,yposition,xsize=npx,ysize=npy
        endif else begin
          idx = where( image eq fill_color )
          if idx[0] ne -1 then begin
            for i=0L, n_elements(idx)-1 do begin
              ind = array_indices(image, idx[i] )
              polyfill, xposition+ round( (ind[0]+[0,1,1,0])/scale ), $
                         yposition+ round( (ind[1]+[0,0,1,1])/scale ), color=fill_color, /device
            endfor
          endif
        endelse
      endif
      ;help,image
      ;print, xposition,yposition,npx,npy
      ;print, 'scale= ',scale
      
      ;redraw the axes
      str_element,/add,opt,'noerase',1
      str_element,/add,opt,'overplot',/delete
      str_element,/add,opt,'ytitle',/delete
      str_element,/add,opt,'position',reform(transpose([[!x.window],[!y.window]]),4)
      ;help,opt,/st
      box,opt

;      ; keep track of min/max zrange for all data segments
;      if j eq 0 then begin
;        zrange_min = zrange[0]
;        zrange_max = zrange[1]
;      endif else begin
;        zrange_min = min(zrange[0], zrange_min)
;        zrange_max = max(zrange[1], zrange_max)   
;      endelse
   
   endif else dprint,dlevel=0,'Out of range error'

endfor ; loop over data segments

;zrange = [zrange_min, zrange_max]

if keyword_set(zlog) then zrange = 10.^zrange

charsize=!p.charsize
str_element,opt,'charsize',value=charsize
if not keyword_set(charsize) then charsize = 1.

str_element,opt,'no_color_scale',value=no_color_scale
str_element,opt,'zticks',zticks
str_element,opt,'zposition',zposition
str_element,opt,'zoffset',zoffset
if not keyword_set(no_color_scale) then $
  draw_color_scale,brange=[bottom,top],range=zrange,log=zlog,title=ztitle, $
    charsize=charsize,yticks=zticks,position=zposition,offset=zoffset

;copy from temp variable back to input variables
x = xtemp
y = ytemp
z = ztemp

end

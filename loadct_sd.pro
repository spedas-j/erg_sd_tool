;+
;PROCEDURE loadct_sd
; Basically this procedure is the same as loadct2.pro except for
; yellow (color=5) replaced with grey
;
; This slightly modified color table is to used for plotting SuperDARN data
;-

PRO cut_col_tab

  ; Number of colours for current device
  ncol=(!D.N_COLORS<256)-1
  
  red  =INTARR(ncol)
  green=INTARR(ncol)
  blue =INTARR(ncol)
  
  ; Define internal colour table
  ;IF col_table EQ -1 THEN BEGIN
  
  colour_table=INTARR(4,256)
  
  ; Check whether colour or grey scale should be used
  ; If grey scale then reverse for postscript
  
  ;IF (format AND 4) EQ 0 THEN BEGIN
  stack = SCOPE_TRACEBACK(/structure)
  filename = stack[SCOPE_LEVEL()-1].filename
  dir = FILE_DIRNAME(filename)
  fname_cut_coltbl = dir+'/col_tbl/cut_col_tab.dat'
  OPENR,colorfile,fname_cut_coltbl,/GET_LUN
  READF,colorfile,colour_table
  FREE_LUN,colorfile
  
  ; Stretch this colour scale coz it's no good at the ends
  colour_stretch=INTARR(4,256)
  scale_start=65
  scale_end=240
  skip=(scale_end-scale_start)/256.0
  FOR col=0,255 DO BEGIN
    colour_stretch(*,col)=colour_table(*,FIX(scale_start+skip*col))
  ENDFOR
  colour_table=colour_stretch
  
  ;    ENDIF ELSE BEGIN
  ;      grey_base=0.1
  ;      IF !D.NAME NE 'PS' THEN BEGIN
  ;        colour_table(1,*)=FIX(INDGEN(256)*(1-grey_base)+256*grey_base)
  ;        colour_table(2,*)=FIX(INDGEN(256)*(1-grey_base)+256*grey_base)
  ;        colour_table(3,*)=FIX(INDGEN(256)*(1-grey_base)+256*grey_base)
  ;      ENDIF ELSE BEGIN
  ;        colour_table(1,*)=255-FIX(INDGEN(256)*(1-grey_base)+256*grey_base)
  ;        colour_table(2,*)=255-FIX(INDGEN(256)*(1-grey_base)+256*grey_base)
  ;        colour_table(3,*)=255-FIX(INDGEN(256)*(1-grey_base)+256*grey_base)
  ;      ENDELSE
  ;    ENDELSE
  
  indx=1.0
  skip=255.0/(ncol-1)
  FOR col=1,ncol-1 DO BEGIN
    red(col)  =colour_table(1,FIX(indx))
    green(col)=colour_table(3,FIX(indx))
    blue(col) =colour_table(2,FIX(indx))
    indx=indx+skip
  ENDFOR
  
  ; Swap colour bar if necessary
  red_swap  =red
  blue_swap =blue
  green_swap=green
  FOR col=1,ncol-1 DO BEGIN
    red(ncol-col)  =red_swap(col)
    blue(ncol-col) =blue_swap(col)
    green(ncol-col)=green_swap(col)
  ENDFOR
  
  ;ENDIF
  
  ; Black and white
  ;  red(0)       =0
  ;  blue(0)      =0
  ;  green(0)     =0
  ;  red(ncol-1)  =255
  ;  blue(ncol-1) =255
  ;  green(ncol-1)=255
  
  ; Ground scatter colour (grey)
  ;  red(ncol-2)  =grey_level*16
  ;  blue(ncol-2) =grey_level*16
  ;  green(ncol-2)=grey_level*16
  
  IF !D.NAME NE 'NULL' AND !d.name NE 'HP'THEN BEGIN
  
    TVLCT,red,green,blue
    
  ; Load external colour table if selected
  ;IF col_table NE -1 THEN set_colour_table,col_table
    
  ENDIF
  
END

;-----------------------------------------------------------------------

PRO loadct_sd,ct,invert=invert,reverse=revrse,file=file,previous_ct=previous_ct
  COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
  @colors_com
  
  deffile = GETENV('IDL_CT_FILE')
  IF NOT KEYWORD_SET(deffile) THEN BEGIN  ; looks for color table file in same directory
    stack = SCOPE_TRACEBACK(/structure)
    filename = stack[SCOPE_LEVEL()-1].filename
    dir = FILE_DIRNAME(filename)
    deffile = FILE_SEARCH(dir+'/col_tbl/'+'colors*.tbl',count=nf)
    IF nf GT 0 THEN deffile=deffile[nf-1]              ; Use last one found
  ;dprint,'Using color table: ',deffile,dlevel=3
  ENDIF
  IF NOT KEYWORD_SET(file) AND KEYWORD_SET(deffile) THEN file=deffile
  
  black = 0
  magenta=1
  blue = 2
  cyan = 3
  green = 4
  grey = 5
  red = 6
  bottom_c = 7
  
  IF ~KEYWORD_SET(ct) THEN ct = 43 ;FAST-Special
  
  IF N_ELEMENTS(color_table) EQ 0 THEN color_table=ct
  previous_ct =  color_table
  IF !d.name EQ 'NULL' OR !d.name EQ 'HP' THEN BEGIN   ; NULL device and HP device do not support loadct
    dprint,'Device ',!d.name,' does not support color tables. Command Ignored'
    RETURN
  ENDIF
  
  IF ct NE 44 THEN BEGIN
    loadct,ct,bottom=bottom_c,file=file,/silent
    PRINT, '% Loading table SD-Special'
  ENDIF ELSE BEGIN
    cut_col_tab
    print, '% Loading table Cutlass color bar for SD' 
  ENDELSE
  color_table = ct
  
  top_c = !d.table_size-2
  white =top_c+1
  cols = [black,magenta,blue,cyan,green,grey,red,white]
  primary = cols[1:6]
  
  
  TVLCT,r,g,b,/get
  
  IF KEYWORD_SET(revrse) THEN BEGIN
    r[bottom_c:top_c] = reverse(r[bottom_c:top_c])
    g[bottom_c:top_c] = reverse(g[bottom_c:top_c])
    b[bottom_c:top_c] = reverse(b[bottom_c:top_c])
  ENDIF
  
  r[cols] = BYTE([0,1,0,0,0,0.353,1,1]*255)
  g[cols] = BYTE([0,0,0,1,1,0.353,0,1]*255)
  b[cols] = BYTE([0,1,1,1,0,0.353,0,1]*255)
  TVLCT,r,g,b
  
  r_curr = r  ;Important!  Update the colors common block.
  g_curr = g
  b_curr = b
  
  
END



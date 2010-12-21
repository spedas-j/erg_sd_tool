;+
; PROCEDURE loadct_sd
; 
; :DESCRIPTION:
; Basically this procedure is the same as loadct2.pro except for
; yellow (color=5) replaced with grey. In addition, if you run 
; this with an argument of 44 (e.g., loadct_sd, 44), then it 
; loads the Cutlass color table usually used for SuperDARN data. 
; 
; :AUTHOR:
;   Tomo Hori (E-mail: horit@stelab.nagoya-u.ac.jp)
; :HISTORY:
;   2010/11/20: created 
; 
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-

;To define the cutlass color table. The RGB values are loaded 
;from cut_col_tab.dat which should be placed in the same directory.
PRO cut_col_tab

  ; Number of colours for current device
  ncol=(!D.N_COLORS<256)-1
  
  red  =INTARR(ncol)
  green=INTARR(ncol)
  blue =INTARR(ncol)
    
  colour_table=INTARR(4,256)
  
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
  
  indx=1.0
  skip=255.0/(ncol-1)
  FOR col=1,ncol-1 DO BEGIN
    red(col)  =colour_table(1,FIX(indx))
    green(col)=colour_table(3,FIX(indx))
    blue(col) =colour_table(2,FIX(indx))
    indx=indx+skip
  ENDFOR
  
  ; Swap colour bar so that color goes red -> yellow -> green -> blue 
  red_swap  =red
  blue_swap =blue
  green_swap=green
  FOR col=1,ncol-1 DO BEGIN
    red(ncol-col)  =red_swap(col)
    blue(ncol-col) =blue_swap(col)
    green(ncol-col)=green_swap(col)
  ENDFOR
  
  IF !D.NAME NE 'NULL' AND !d.name NE 'HP'THEN BEGIN
  
    TVLCT,red,green,blue
    
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
  
  r[cols] = BYTE([0,1,0,0,0,0.553,1,1]*255)
  g[cols] = BYTE([0,0,0,1,1,0.553,0,1]*255)
  b[cols] = BYTE([0,1,1,1,0,0.553,0,1]*255)
  TVLCT,r,g,b
  
  r_curr = r  ;Important!  Update the colors common block.
  g_curr = g
  b_curr = b
  
  
END



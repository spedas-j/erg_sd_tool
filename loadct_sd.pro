;+
;PROCEDURE loadct_sd
; Basically this procedure is the same as loadct2.pro except for 
; yellow (color=5) replaced with grey 
; 
; This slightly modified color table is to used for plotting SuperDARN data
;-


pro loadct_sd,ct,invert=invert,reverse=revrse,file=file,previous_ct=previous_ct
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
@colors_com

deffile = getenv('IDL_CT_FILE')
if not keyword_set(deffile) then begin  ; looks for color table file in same directory
    stack = scope_traceback(/structure)
    filename = stack[scope_level()-1].filename
    dir = file_dirname(filename)
    deffile = file_search(dir+'/'+'colors*.tbl',count=nf)
    if nf gt 0 then deffile=deffile[nf-1]              ; Use last one found
    ;dprint,'Using color table: ',deffile,dlevel=3
endif
if not keyword_set(file) and keyword_set(deffile) then file=deffile

black = 0
magenta=1
blue = 2
cyan = 3
green = 4
grey = 5
red = 6
bottom_c = 7

if ~keyword_set(ct) then ct = 43 ;FAST-Special

if n_elements(color_table) eq 0 then color_table=ct
previous_ct =  color_table
if !d.name eq 'NULL' or !d.name eq 'HP' then begin   ; NULL device and HP device do not support loadct
   dprint,'Device ',!d.name,' does not support color tables. Command Ignored'
   return
endif
print, '% Loading table SD-Special'
loadct,ct,bottom=bottom_c,file=file,/silent
color_table = ct

top_c = !d.table_size-2
white =top_c+1
cols = [black,magenta,blue,cyan,green,grey,red,white]
primary = cols[1:6]


tvlct,r,g,b,/get

if keyword_set(revrse) then begin
  r[bottom_c:top_c] = reverse(r[bottom_c:top_c])
  g[bottom_c:top_c] = reverse(g[bottom_c:top_c])
  b[bottom_c:top_c] = reverse(b[bottom_c:top_c])
endif

r[cols] = byte([0,1,0,0,0,0.353,1,1]*255)
g[cols] = byte([0,0,0,1,1,0.353,0,1]*255)
b[cols] = byte([0,1,1,1,0,0.353,0,1]*255)
tvlct,r,g,b

r_curr = r  ;Important!  Update the colors common block.
g_curr = g
b_curr = b

  ;force end colors  0 is black max is white
;tvlct,r,g,b,/get
;n = n_elements(r)
;lc = n-1
;black = 0
;white = 255
;if keyword_set(revrse) then begin
;  r = reverse(r)
;  g = reverse(g)
;  b = reverse(b)
;endif
;if keyword_set(invert) then begin
;  black = 255
;  white = 0
;endif
;r(0) = black & g(0)=black  & b(0)=black
;r(lc)=white  & g(lc)=white & b(lc)=white
;tvlct,r,g,b

end



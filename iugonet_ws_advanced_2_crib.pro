;; iugonet_ws_advanced_2_crib.pro 

; === slide 15 ===
timespan, '2013-09-23'
erg_load_sdfit, site='pyk', /get_support_data
tplot_names


; === slide 17 ===
tplot, 'sd_pyk_pwr_2' 
tplot, ['sd_pyk_pwr_2', 'sd_pyk_vlos_2','sd_pyk_spec_width_2'] 


; === slide 18 ===
splitbeam, 'sd_pyk_vlos_2'
tplot_names
tplot, ['sd_pyk_vlos_2_azim05','sd_pyk_vlos_2_azim07','sd_pyk_vlos_2_azim09'] 


; === slide 19 ===
zlim,  'sd_pyk_vlos_2_azim??', -800, 800 
tplot 


; === slide 20 === 
tlimit 
tlimit, /last 
tlimit, full 
tlimit, '2013-09-23/20:30', '2013-09-24/00:00' 


; === slide 21 ===
splitbeam, 'sd_pyk_vlos_bothscat_2' 
zlim, 'sd_pyk_vlos_*scat*', -800,800 
tplot, 'sd_pyk_vlos_iscat_2_azim05'
loadct_sd, 43
tplot, 'sd_pyk_vlos_bothscat_2_azim05'

; === slide 22 === 
set_coords, ['sd_pyk_vlos_2_azim??'], 'glat' 
tplot,['sd_pyk_vlos_2_azim05','sd_pyk_vlos_2_azim09'] 
set_coords, ['sd_pyk_vlos_2_azim??'], 'mlat' 
tplot,['sd_pyk_vlos_2_azim05','sd_pyk_vlos_2_azim09']


; === slide 23 ===
set_coords, 'sd_pyk_vlos_2_azim??', 'gate' 
get_fixed_pixel_graph, 'sd_pyk_vlos_2', beam=7, range_gate=18 
tplot, ['sd_pyk_vlos_2_azim07', ' sd_pyk_vlos_2_bm07rg018 ' ] 


; === slide 24 === 
loadct_sd, 44
tplot, 'sd_pyk_vlos_2_azim07'
loadct_sd, 45, /center
tplot, 'sd_pyk_vlos_2_azim07'


; === slide 25 === 
asciidump_scan, 'sd_pyk_vlos_2'


; === slide 28 ===
map2d_init
timespan, '2013-09-23'
map2d_time, 2200
map2d_set, /erase 
overlay_map_coast
plots, [345., 15], [65,78] 
polyfill, [15., 45., 45., 15.] , [70., 70., 75., 75.], color = 240


; === slide 29 === 
map2d_coord, 'aacgm'
map2d_set, /erase, /mltlabel 
overlay_map_coast
plots,  [0., 9] /24.*360.,   [70., 80.] 
polyfill, [17., 19., 19., 17.]  /24.*360., [60., 60., 70., 70.], color = 150


; === slide 31 === 
map2d_init 
map2d_time, 2210 
plot_map_sdfit, 'sd_pyk_vlos_2'
plot_map_sdfit, 'sd_pyk_vlos_2', /coast 
erg_load_sdfit, site=�ehan', /get_support
zlim, 'sd_han_vlos_2', -800,800
plot_map_sdfit, ['sd_pyk_vlos_2', 'sd_han_vlos_2'], /coast
map2d_coord, 'geo' 
plot_map_sdfit, ['sd_pyk_vlos_2', 'sd_han_vlos_2'], /coast
overlay_map_sdfov, site='pyk han'


; === slide 32 === 
map2d_coord, 'aacgm'
plot_map_sdfit, ['sd_pyk_vlos_2','sd_han_vlos_2'], /coast, /clip, center_glat=75, center_glon=0
plot_map_sdfit, ['sd_pyk_vlos_2', 'sd_han_vlos_2'], /coast, /clip, center_glat=75, center_glon=0, colorscalepos=[0.05, 0.65, 0.08, 0.95] 
plot_map_sdfit, ['sd_pyk_vlos_2','sd_han_vlos_2'], /coast, /clip, center_glat=75, center_glon=0, colorscalepos=[0.05, 0.65, 0.08, 0.95], /mltlabel


; === slide 33 ===
sd_time, 2155
plot_map_sdfit, ['sd_pyk_vlos_2','sd_han_vlos_2'],/clip, /coast, center_glat=75, center_glon=0, position=[0.0,0.5,0.5,1.0] , /nocolorscale
sd_time, 2200
plot_map_sdfit, ['sd_pyk_vlos_2','sd_han_vlos_2'],/clip, /coast, center_glat=75, center_glon=0, position=[0.5,0.5,1.0,1.0],  /noerase , /nocolorscale
sd_time, 2205
plot_map_sdfit, ['sd_pyk_vlos_2','sd_han_vlos_2'],/clip, /coast, center_glat=75, center_glon=0, position=[0.0,0.0,0.5,0.5],  /noerase, /nocolorscale
sd_time, 2210
plot_map_sdfit, ['sd_pyk_vlos_2','sd_han_vlos_2'],/clip, /coast, center_glat=75, center_glon=0,  position=[0.5,0.0,1.0,0.5],  /noerase, colorscalepos=[0.05, 0.65, 0.08, 0.95]


; === slide 34 ===
!p.position = [0,0,1,1] 
make_fanplot_pictures, ['sd_pyk_vlos_2','sd_han_vlos_2'], 2155, 2210, /clip, /coast, center_glat=75, center_glon=0, prefix='pngdir/sd_pykhan_'


; === slide 36 === 
map2d_init
map2d_time, '2013-09-23/22:10'
map2d_coord, 'aacgm'
map2d_set, glatc=75,glonc=0,  scale=35e+6, /mltlabel, /erase
overlay_map_sdfit, ['sd_pyk_vlos_2','sd_han_vlos_2'], colorscalepos=[0.05, 0.65, 0.08, 0.95] 
overlay_map_coast


; === slide 38 ====
timespan, '2012-03-27/23:00:00',1,/hour
erg_load_sdfit, site='pyk', /get
iug_load_asi_nipr, site='hus'
map2_time, 2304
map2d_set, /erase, scale=17e+6, glatc=70, glonc=346
loadct, 0
overlay_map_asi_nipr, 'nipr_asi_hus_0000', /nocolor, colorr=[20,140]
loadct_sd, 44 & overlay_map_sdfit, 'sd_pyk_vlos_2', pixel=0.5
overlay_map_coast, col=40


;+
; :DESCRIPTION:
;    A crib sheet to demonstrate how procedures/functions for 
;    SuperDARN data work.
;
;    You can run this crib sheet by copying&pasting each commands 
;    below into a IDL window. 
;    Or just compile and run through all by the following command: 
;    .run erg_crib_superdarn.pro
;
; :AUTHOR: T. Hori
; :HISTORY:
;    2010/06/24: Created
;
; $LastChangedBy: $
; $LastChangedDate: $
; $LastChangedRevision: $
; $URL: $
;-

;Initialize 
thm_init 

;Set the date and time for loading data
timespan, '2007-06-21', 1, /day 

;Load the data
erg_load_sdfit, site='hok' 

;View the loaded data names
tplot_names

;Plot some data
window, 0, xsize=600, ysize=750
tplot, ['sd_hok_pwr_1', 'sd_hok_vlos_1', 'sd_hok_spec_width_1']

;Change the time range of the plot
tlimit, ['2007-06-21/13:00','2007-06-21/15:00']

;Divide the data into those for separate beams
splitbeam, ['sd_hok_pwr_1', 'sd_hok_vlos_1', 'sd_hok_spec_width_1']

;Plot data for a specific beam
tplot, ['sd_hok_pwr_1_azim04', 'sd_hok_vlos_1_azim04', $
        'sd_hok_spec_width_1_azim04']

;Plot the northward and eastward components in the geographical coordinates 
;of the line-of-sight Doppler velocity 
splitbeam, ['sd_hok_vnorth_1','sd_hok_veast_1']
tplot, ['sd_hok_vnorth_1_azim04','sd_hok_veast_1_azim04']

;Plot some parameters relating to the radar operation
timespan, '2007-06-21', 1, /day    ;again set the time range to be 1 day
erg_load_sdfit, site='hok', /get_support_data  ;Load data with the supporting data
splitbeam, 'sd_hok_elev_angle_1'
ylim, 'sd_hok_noise_1', 0,0, 1  ;auto range with log scale
tplot, 'sd_hok_'+['elev_angle_1_azim04','scanno_1','smsep_1',$
	'tfreq_1','noise_1', 'num_ave_1']


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Commands for 2-dimensional plotting on the ground map are 
; still being worked on, comming up soon!
;

end


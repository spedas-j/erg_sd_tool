;------------------------------------------------------------------------------
; AACGM library
;
; a collection of IDL routines intended to fully exploit the functionality of
; the AACGM coordinates [Shepherd, 2014] including use of the AACGM
; coefficients and field line tracing
;
; 20140402 SGS v0.0  a simple modification to the existing AACGM IDL software,
;                    but includes additional features: linear interpolation and
;                    fieldline tracing.
; 20140410 SGS v0.1  modified to do interpolation in time; combined all
;                    functions into a single library.
; 20140618 SGS v0.2  more error checking and comments added; common blocks
;                    changed to use leo_ prefix
; 20140701 SGS v0.3  bug fix: fyear was integer instead of float.
;                    fixed common blocks and logic for when time and height
;                    interpolations must occur.
; 20140709 SGS v0.4  changed longitude output of traced results to -180 to 180
;                    in order to be consistent with other outputs
; 20140826 SGS v0.5  change to requiring data/time to be set using external
;                    functions (same as C version), rather than using keywords.
;                    moved coefficient loading and time interpolation into new
;                    function, AACGM_v2_TimeInterp.
; 20140918 SGS v1.0  change function names to _v2 for wider distribution
;
; Functions:
;
; AACGM_v2_Convert
; AACGM_v2_LoadCoef
; AACGM_v2_Sgn
; AACGM_v2_Rylm
; AACGM_v2_Alt2CGM
; AACGM_v2_CGM2Alt
; AACGM_v2_ConvertGeoCoord
; AACGM_v2_Newval
; AACGM_v2_RK45
; AACGM_v2_Dayno
; AACGM_v2_Trace
; AACGM_v2_Trace_inv
; AACGM_v2_TimeInterp
;
;------------------------------------------------------------------------------
;

; define this common block right here
common AACGM_v2_Com, coef_v2,coefs_v2, cint_v2, $
											height_old_v2, order_v2,kmax_v2, $
											myear_v2, fyear_v2, myear_old_v2, fyear_old_v2, $
											aacgm_v2_datetime

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; AACGM routines, non fieldline tracing
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Common blocks for the AACGM algorithm
;
; coefs_v2 holds both full sets of coefficients that bound the time
; coef_v2  holds the linearly interpolated set of coefficients
; cint_v2  holds the coefficients evalated at the altitude of interest
;
;+-----------------------------------------------------------------------------
;

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_LoadCoef
;
; PURPOSE:
;
;       Load two sets of AACGM coefficients that bracket the desired date.
;       Interpolation in time is performed at a later stage.
;
; CALLING SEQUENCE:
;       s = AACGM_v2_LoadCoef(units, order=ord)
;
;     Input Arguments:  
;       units         - logical unit numbers of coefficient files already open
;
;     Output Arguments:  
;       s             - return -1 if IO fails for whatever reason
;
;     Keywords:
;       order         - default is 10th order but allow for other orders
;
; HISTORY:
;
; added ability to interpolate in time with two sets of coefficients.
;     
;+-----------------------------------------------------------------------------
;

function AACGM_v2_LoadCoef, units, order=ord

common AACGM_v2_Com

	on_ioerror, iofail

	if keyword_set(ord) then order_v2 = ord else order_v2 = 10	; default order
	kmax_v2 = (order_v2+1)*(order_v2+1)

	ncoord = 3		; xyz
	nquart = 5		; quartic altitude fit coefficients
	nflag  = 2		; 0: GEO->AACGM; 1: AACGM->GEO

	; interpolation in time, so need two sets of coefficients....
	coefs_v2 = fltarr(2,kmax_v2,ncoord,nquart,nflag)

	temp_coef = fltarr(kmax_v2,ncoord,nquart,nflag)
	readf, units[0],temp_coef
	coefs_v2[0,*,*,*,*] = temp_coef

	readf, units[1],temp_coef
	coefs_v2[1,*,*,*,*] = temp_coef

	coef_v2 = fltarr(kmax_v2,ncoord,nquart,nflag)
	cint_v2 = fltarr(kmax_v2,ncoord,nflag)
;	leo_first_coeff_old = -1.

;	sol_dec_old = 0				; what the hell are these used for?!
;	told = 1e12
;	mslon1=0
;	mslon2=0

	return, 0

iofail:
	return, -1

end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_Sgn
;
; PURPOSE:
;       return the signed quantity of a variable where the magnitude is given
;       by the first argument and the sign is given by the second argument.
;
; CALLING SEQUENCE:
;       AACGM_v2_Sgn, a, b
;     
;     Input Arguments:  
;       a             - magnitude
;       b             - sign
;
;     Return Value:
;       signed quantity
;
;+-----------------------------------------------------------------------------
;

function AACGM_v2_Sgn, a, b
	if (a ge 0) then x = a else x = -a
	if (b ge 0) then return, x
	return, -x
end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_Rylm
;
; PURPOSE:
;       Computes an array of real spherical harmonic function values
;       Y_lm(phi,theta) for a given colatitiude (phi) and longitude (theta)
;       for all the values up to l = order, which is typically 10. The
;       values are stored in a 1D array of dimension (order+1)^2. The
;       indexing scheme used is:
;
;        l    0  1  1  1  2  2  2  2  2  3  3  3  3  3  3  3  4  4  4  4  4 ...
;        m    0 -1  0  1 -2 -1  0  1  2 -3 -2 -1  0  1  2  3 -4 -3 -2 -1  0 ...
;C & IDL j    0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 ...
;FORTRAN j    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 ...
; 
; CALLING SEQUENCE:
;       AACGM_v2_Rylm, colat,lon,order, ylmval
;
;     Input Arguments:  
;       colat         - The colatitude of the point for which the spherical
;                       harmonic Y_lm is to be calculated
;
;       lon           - The longitude of the point for which the spherical
;                       harmonic Y_lm is to be calculated
;
;       order         - The order of the spherical harmonic function expansion.
;                       The total number of terms computed will be (order+1)^2
;
;     Output Argument:
;       ylmval        - 1D array of spherical harmonic functions at the point
;                       (colat,lon)
;
; HISTORY:
;
; Revision 1.1  94/10/12  15:24:21  15:24:21  baker (Kile Baker S1G)
; Initial revision
;
; subsequent revisions, porting to C and IDL by baker, wing and barnes.
;
; NOTES by SGS:
;
; It is likely that the original version was taken from FORTRAN and used array
; indexing that begins with 1. Indexing is somewhat more natural using the
; zeros-based indexing of C/IDL. Indices have thus been changed from the
; original version.
;
; It appears that the original version used unnormalized spherical harmonic
; functions. I suspect this might be better, but realized it too late. The
; coefficients I derived are for orthonormal spherical harmonic functions
; which then require the same for evaluation. I believe that the original
; authors used orthogonal spherical harmonic functions which eliminate the
; need for computing the normalization factors. I suspect this is just fine,
; but have not tested it.
; 
;+-----------------------------------------------------------------------------
;

pro AACGM_v2_Rylm, colat,lon,order, ylmval

	; Note: the recurrence relations are for the associated legendre polynomials.
	;       SGS has added the normalization factor for the spherical harmonic
	;       functions given by (6.8.2). See note about this above.

	cos_theta = cos(colat)
	sin_theta = sin(colat)

	cos_lon = cos(lon)
	sin_lon = sin(lon)

				d1    = -sin_theta
	z2    = complex(cos_lon,sin_lon)
	z1    = d1*z2
	q_fac = z1

	; Generate Zonal Harmonics (P_l^(m=0) for l = 1,order) using recursion
	; relation (6.8.7), p. 252, Numerical Recipes in C, 2nd. ed., Press. W.
	; et al. Cambridge University Press, 1992) for case where m = 0.
	;
	; l Pl = cos(theta) (2l-1) Pl-1 - (l-1) Pl-2          (6.8.7)
	;
	; where Pl = P_l^(m=0) are the associated Legendre polynomials

	ylmval[0] = 1.					; l = 0, m = 0
	ylmval[2] = cos_theta		; l = 1, m = 0

	for l=2,order do begin
		; indices for previous two values: k = l * (l+1) + m with m=0
		ia = (l-2)*(l-1)
		ib = (l-1)*l
		ic = l * (l+1)

		ylmval[ic] = (cos_theta * (2*l-1) * ylmval[ib] - (l-1)*ylmval[ia])/l
	endfor

	; Generate P_l^l for l = 1 to (order+1)^2 using algorithm based upon (6.8.8)
	; in Press et al., but incorporate longitude dependence, i.e., sin/cos (phi)
	;
	; Pll = (-1)^l (2l-1)!! (sin^2(theta))^(l/2)
	;
	; where Plm = P_l^m are the associated Legendre polynomials

	q_val = q_fac
	ylmval[3] = float(q_val)				; l = 1, m = +1
	ylmval[1] = -imaginary(q_val)		; l = 1, m = -1
	for l=2,order do begin
		d1    = l*2 - 1.
		z2    = d1*q_fac
		z1    = z2*q_val
		q_val = z1

		; indices for previous two values: k = l * (l+1) + m
		ia = l*(l+2)		; m = +l
		ib = l*l				; m = -l

		ylmval[ia] = float(q_val)
		ylmval[ib] = -imaginary(q_val)
	endfor

	; Generate P_l,l-1 to P_(order+1)^2,l-1 using algorithm based upon (6.8.9)
	; in Press et al., but incorporate longitude dependence, i.e., sin/cos (phi)
	;
	; Pl,l-1 = cos(theta) (2l-1) Pl-1,l-1

	for l=2,order do begin
		l2 = l*l
		tl = 2*l
		; indices for Pl,l-1; Pl-1,l-1; Pl,-(l-1); Pl-1,-(l-1)
		ia = l2 - 1
		ib = l2 - tl + 1
		ic = l2 + tl - 1
		id = l2 + 1

		fac = tl - 1
		ylmval[ic] = fac * cos_theta * ylmval[ia]			; Pl,l-1
		ylmval[id] = fac * cos_theta * ylmval[ib]			; Pl,-(l-1)
	endfor

	; Generate remaining P_l+2,m to P_(order+1)^2,m for each m = 1 to order-2
	; using algorithm based upon (6.8.7) in Press et al., but incorporate
	; longitude dependence, i.e., sin/cos (phi).
	;
	; for each m value 1 to order-2 we have P_mm and P_m+1,m so we can compute
	; P_m+2,m; P_m+3,m; etc.

	for m=1,order-2 do begin
		for l=m+2,order do begin
			ca = float(2.*l-1)/(l-m)
			cb = float(l+m-1.)/(l-m)

			l2 = l*l
			ic = l2 + l + m
			ib = l2 - l + m
			ia = l2 - l - l - l + 2 + m
			; positive m
			ylmval[ic] = ca * cos_theta * ylmval[ib] - cb * ylmval[ia]

			ic -= (m+m)
			ib -= (m+m)
			ia -= (m+m)
			; negative m
			ylmval[ic] = ca * cos_theta * ylmval[ib] - cb * ylmval[ia]
		endfor
	endfor

	; Normalization added here (SGS)
	;
	; Note that this is NOT the standard spherical harmonic normalization factors
	;
	; The recursive algorithms above treat positive and negative values of m in
	; the same manner. In order to use these algorithms the normalization must
	; also be modified to reflect the symmetry.
	;
	; Output values have been checked against those obtained using the internal
	; IDL legendre() function to obtain the various associated legendre
	; polynomials.
	;
	; As stated above, I think that this normalization may be unnecessary. The
	; important thing is that the various spherical harmonics are orthogonal,
	; rather than orthonormal.

	fact = factorial(indgen(2*order+1))
	ffff = fltarr((order+1)*(order+1))
	for l=0,order do begin
		for m=0,l do begin
			k = l * (l+1) + m					; 1D index for l,m
			ffff[k] = sqrt((2*l+1)/(4*!dpi) * fact[l-m]/fact[l+m])
		endfor
		for m=-l,-1 do begin
			k = l * (l+1) + m					; 1D index for l,m
			kk = l * (l+1) - m
			ffff[k] = ffff[kk] * (-1)^(-m mod 2)
		endfor
	endfor

	ylmval *= ffff

	return
end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_Alt2CGM
;
; PURPOSE:
;       Transformation from so-called 'at-altitude' coordinates to AACGM.
;       The purpose of this function is to scale the latitudes in such a
;       way so that there is no gap. The problem is that for non-zero
;       altitudes (h) are range of latitudes near the equator lie on dipole
;       field lines that near reach the altitude h, and are therefore not
;       accessible. This is the inverse transformation.
;
;       cos (lat_aacgm) = sqrt( Re/(Re + h) ) cos (lat_at-alt)
;       
;
; CALLING SEQUENCE:
;       AACGM_v2_Alt2CGM,r_height_in,r_lat_alt,r_lat_adj
;
;     Input Arguments:  
;       r_height_in   - The altitude (h)
;       r_lat_alt     - The 'at-altitude' latitude
;
;     Output Arguments:  
;       r_lat_adj     - The corrected latitude, i.e., AACGM latitude
;
; HISTORY:
;
; This function is unchanged from its original version (Baker ?)
;     
;+-----------------------------------------------------------------------------
;

pro AACGM_v2_Alt2CGM, r_height_in, r_lat_alt, r_lat_adj

	; convert from at-altitude to AACGM coordinates
	eradius = 6371.2
	eps     = 1e-9
	unim    = 0.9999999

	r1 = cos(!pi*r_lat_alt/180.)
	ra = r1*r1
	if (ra lt eps) then ra = eps

	r0 = (r_height_in/eradius + 1.) / ra
	if (r0 lt unim) then r0 = unim
  
	r1 = acos(sqrt(1./r0))
	r_lat_adj= AACGM_v2_Sgn(r1, r_lat_alt)*180.0/!pi

  return
end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_CGM2Alt
;
; PURPOSE:
;       Transformation from AACGM to so-called 'at-altitude' coordinates.
;       The purpose of this function is to scale the latitudes in such a
;       way so that there is no gap. The problem is that for non-zero
;       altitudes (h) are range of latitudes near the equator lie on dipole
;       field lines that near reach the altitude h, and are therefore not
;       accessible. This mapping closes the gap.
;
;       cos (lat_at-alt) = sqrt( (Re + h)/Re ) cos (lat_aacgm)
;       
;
; CALLING SEQUENCE:
;       AACGM_v2_CGM2Alt,r_height_in,r_lat_in,r_lat_adj, error
;
;     Input Arguments:  
;       r_height_in   - The altitude (h)
;       r_lat_in      - The AACGM latitude
;
;     Output Arguments:  
;       r_lat_adj     - The 'at-altitude' latitude
;       error         - variable is set if latitude is below the value that
;                       is mapped to the origin
;
; HISTORY:
;
; This function is unchanged from its original version (Baker ?)
;     
;+-----------------------------------------------------------------------------
;

pro AACGM_v2_CGM2Alt, r_height_in,r_lat_in, r_lat_adj, error
		
	; convert from AACGM to at-altitude coordinates
	eradius = 6371.2
	unim    = 1
	error   = 0

	r1 = cos(!pi*r_lat_in/180.0)
	ra = (r_height_in/eradius + 1)*(r1*r1)
	if (ra gt unim) then begin
		ra = unim
		error = 1
	endif

	r1 = acos(sqrt(ra))
	r_lat_adj = AACGM_v2_Sgn(r1,r_lat_in)*180.0/!pi

	return
end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_ConvertGeoCoord
;
; PURPOSE:
;
;       Convert to and from AACGM and Geographic with numerous options, such as
;       whether to use the coefficients or do fieldline tracing. The altitude
;       dependence is determined here. The default is to perform the
;       transformation from Geographic to AACGM coordinates using the AACGM
;       coefficients. Several keywords can be used to modify the behavior.
;
; CALLING SEQUENCE:
;				AACGM_v2_ConvertGeoCoord, lat_in,lon_in,height_in, lat_out,lon_out, $
;																	error, geo=geo, trace=trace, $
;                                 allow_trace=allow_trace, bad_idea=bad_idea, $
;                                 at_alt=at_alt, verbose=verbose, debug=debug
;
;     Input Arguments:  
;       lat_in        - input latitude (degrees)
;       lon_in        - input longitude (degrees)
;       height_in     - input altitude (km)
;
;     Output Arguments:  
;       lat_out       - transformed latitude (degrees)
;       lon_out       - transformed longitude (degrees)
;       error         - non-zero integer code used to indicate failure.
;
;     Keywords:
;       geo           - perform inverse transformation: AACGM -> GEO
;       trace         - perform fieldline tracing instead of using coefficients
;       allow_trace   - allow tracing above 2000 km altitude. The default is
;                       to not allow altitudes above 2000 km.
;       bad_idea      - use coefficients to extrapolate for altitudes above
;                       2000 km. This really is a bad idea...
;
;       at_alt        - use the intermediate transformation to at-altitude
;                       dipole coordinates. Note: only used for testing in
;                       the forward direction. Should disable this feature
;                       for release. Use requires that coefficients be derived
;                       from intermediate coordinates, which is not the case.
;       verbose       - debugging for fieldline tracing of inverse
;                       transformation. Should disable this feature for release
;       debug         - set keyword to use built-in IDL Legendre functions
;                       instead of Rylm procedure. Included for debugging of
;                       Rylm procedure. Remove for release.
;
; HISTORY:
;
; Fieldline tracing and modified altitude dependence added to original version.
; 20140827 SGS removed myear and fyear as arguments
;     
;+-----------------------------------------------------------------------------
;

pro AACGM_v2_ConvertGeoCoord, lat_in,lon_in,height_in, lat_out,lon_out, $
																error, geo=geo, trace=trace, $
																bad_idea=bad_idea, allow_trace=allow_trace, $
																eps=eps, at_alt=at_alt, verbose=verbose, $
																debug=debug

common AACGM_v2_Com

	if (height_in lt 0) then begin
		print, $
			'ERROR: coordinate tranformations are not defined for altitudes < 0 km', $
			height_in
		error = -2
		return
	endif

	if height_in gt 2000 and $
			not keyword_set(trace) and not keyword_set(bad_idea) and $
			not keyword_set(allow_trace) then begin
		; the user is not using fieldline tracing or indicated that they know
		; what they are doing by setting the 'bad_idea' keyword.
		print, 'ERROR: coefficients are not valid for altitudes above 2000 km.'
		print, '       Use fieldline tracing option [/trace] to perform fieldline'
		print, '       tracing at all altitudes, [/allow_trace] to perform'
		print, '       fieldline tracing only above 2000 km or indicate that you'
		print, '       want to use the coefficients for extrapolation and are aware'
		print, '       that the results can be nonsensical by setting the bad_idea'
		print, '       keyword.'
		error = -4
		return
	endif

	if (abs(lat_in) gt 90.) then begin
		print, 'ERROR: latitudes must be in the range -90 to +90 degrees'
		error = -8
		return
	endif

	; SGS - better checking of inputs needed here
	if lon_in lt 0 then lon_in += 360
	if ((lon_in lt 0) or (lon_in gt 360)) then begin
		print, 'ERROR: longitudes must be in the range 0 to 360 degrees'
		error = -16
		return
	endif

	; field line tracing
	if keyword_set(trace) or (height_in gt 2000 and keyword_set(allow_trace)) $
	then begin
		if keyword_set(geo) then begin
			AACGM_v2_Trace_inv, lat_in,lon_in,height_in, tmp_lat,tmp_lon, error, $
										fixed=fixed, ds=ds, eps=eps, max_ds=max_ds, $
										verbose=verbose
		endif else begin
			AACGM_v2_Trace, lat_in,lon_in,height_in, tmp_lat,tmp_lon, error, $
										fixed=fixed, ds=ds, eps=eps, max_ds=max_ds
		endelse
		lat_out = tmp_lat
		lon_out = tmp_lon
		return
	endif

	; 20140827 SGS moved coefficient loading to Date/Time setting functions
	; 20140827 SGS moved time interpolation to Date/Time setting functions

	flag = keyword_set(geo)

	; determine the altitude dependence of the coefficients
	if (height_in ne height_old_v2[flag]) then begin
;		print, '*** HEIGHT INTERPOLATION ***'
		alt_var    = height_in/2000.0		; make this scaling height a variable?
		alt_var_sq = alt_var*alt_var
		alt_var_cu = alt_var*alt_var_sq
		alt_var_qu = alt_var*alt_var_cu

		for i=0,2 do begin		; should this be variable, i.e. we only use x and y
			for j=0,kmax_v2-1 do begin 
				cint_v2[j,i,flag] =	coef_v2[j,i,0,flag] + $
													coef_v2[j,i,1,flag] * alt_var + $
													coef_v2[j,i,2,flag] * alt_var_sq + $
													coef_v2[j,i,3,flag] * alt_var_cu + $
													coef_v2[j,i,4,flag] * alt_var_qu 
			endfor
		endfor
		height_old_v2[flag] = height_in
	endif

	x = double(0)
	y = double(0)
	z = double(0)

	lon_input = lon_in*!pi/180.0 

	; Intermediate coordinate. Only used for inverse transmformation
;	if not keyword_set(at_alt) or (flag eq 0) then begin
	if not keyword_set(at_alt) and (flag eq 0) then begin
		colat_input = (90.-lat_in)*!pi/180.0
	endif else begin
		; convert from AACGM to at-altitude coordinates
		error = -64
		AACGM_v2_CGM2Alt, height_in,lat_in, lat_adj, errflg
		if (errflg ne 0) then return
		colat_input = (90. - lat_adj)*!pi/180.0
	endelse

	; SGS - remove this feature after timing tests
	if keyword_set(debug) then begin
		; use the built-in legendre function to compute values of the spherical
		; harmonic functions
		fact = factorial(indgen(2*order_v2+1))

		ylmval = fltarr(kmax_v2)
		for l=0,order_v2 do begin

			; m < 0 terms
			for m=-l,-1 do begin
				plm = legendre(cos(colat_input), l, m, /double)
				ylm = sqrt((2*l+1)/(4*!dpi) * fact[l-m]/fact[l+m])*plm*sin(m*lon_input)
				k = l * (l+1) + m					; 1D index for l,m
				ylmval[k] = ylm
				x += cint_v2[k,0,flag] * ylm
				y += cint_v2[k,1,flag] * ylm
				z += cint_v2[k,2,flag] * ylm
			endfor

			; m = 0 term
			plm = legendre(cos(colat_input), l, 0, /double)
			ylm = sqrt((2*l+1)/(4*!dpi)) * plm
			k = l * (l+1)								; 1D index for l,m
			ylmval[k] = ylm
			x += cint_v2[k,0,flag] * ylm
			y += cint_v2[k,1,flag] * ylm
			z += cint_v2[k,2,flag] * ylm

			; m > 0 terms
			for m=1,l do begin
				plm = legendre(cos(colat_input), l, m, /double)
				ylm = sqrt((2*l+1)/(4*!dpi) * fact[l-m]/fact[l+m])*plm*cos(m*lon_input)
				k = l * (l+1) + m					; 1D index for l,m
				ylmval[k] = ylm
				x += cint_v2[k,0,flag] * ylm
				y += cint_v2[k,1,flag] * ylm
				z += cint_v2[k,2,flag] * ylm
			endfor

		endfor
		;print, 'legendre()'
		;print, ylmval
	endif else begin
		; use the Rylm function (adapted to orthonormal functions; SGS) to compute
		; values of the spherical harmonic functions

		ylmval = fltarr(kmax_v2)
		AACGM_v2_Rylm, colat_input,lon_input, order_v2,ylmval

		for l = 0,order_v2 do begin
			for m = -l,l do begin
				k = l * (l+1) + m

				x += cint_v2[k,0,flag]*ylmval[k]
				y += cint_v2[k,1,flag]*ylmval[k]
				z += cint_v2[k,2,flag]*ylmval[k]		; SGS - need this for sign...
			endfor
		endfor
	;print, 'Rylm'
	;print, ylmval
	endelse

	; COMMENT: SGS
	; 
	; This answers one of my questions about how the coordinates for AACGM are
	; guaranteed to be on the unit sphere. Here they compute xyz indpendently
	; using the SH coefficients for each coordinate. They reject anything that
	; is +/- .1 Re from the surface of the Earth. They then scale each xyz
	; coordinate by the computed radial distance. This is a TERRIBLE way to do
	; things... but necessary for the inverse transformation.

	; SGS - new method that ensures position is on unit sphere and results in a
	;       much better fit. Uses z coordinate only for sign, i.e., hemisphere.
	if (flag eq 0) then begin
		fac = x*x + y*y
		if fac gt 1. then begin
			; we are in the forbidden region and the solution is undefined
			lat_out = !values.f_nan
			lon_out = !values.f_nan
			error = -64
			return
		endif

		ztmp = sqrt(1. - fac)
		if z lt 0 then z = -ztmp else z = ztmp

		colat_temp = acos(z)

	endif else begin
	; SGS - for inverse the old normalization produces lower overall errors...
		r = sqrt(x*x + y*y + z*z)
		if ((r lt 0.9) or (r gt 1.1)) then begin
			; too much variation in the radial component
			lat_out = !values.f_nan
			lon_out = !values.f_nan
			error = -32
			return
		endif
 
		z /= r
		x /= r
		y /= r

		; SGS - this is for cases where abs(z) > 1.
		if (z ge 1.) then colat_temp = 0 $ 
		else if (z lt -1.) then colat_temp = !pi $
		else colat_temp = acos(z)
 	endelse 

	; SGS - check these values
	if ((abs(x) lt 1e-8) and (abs(y) lt 1e-8)) then $
		lon_temp = 0 $
	else $
		lon_temp = atan(y,x)

	lon_output = lon_temp

	if keyword_set(at_alt) and (flag eq 0) then begin
		lat_alt = 90 - colat_temp*180.0/!pi
		AACGM_v2_Alt2CGM, height_in,lat_alt, lat_adj
		colat_output = (90. - lat_adj) * !pi/180.0
	endif else $
		colat_output = colat_temp

	lat_out = 90. - colat_output*180.0/!pi
	lon_out = lon_output*180.0/!pi

	error = 0  
	return
end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_Convert
;
; PURPOSE:
;
;       High-level routine that loops over each input array element or just
;       calls the transformation function for the input scalars. Several
;       keywords can be used to modify the behavior and are passed through to
;       subsequent functions.
;
; CALLING SEQUENCE:
;       s = AACGM_v2_Convert(in_lat, in_lon, height, out_lat, out_lon, r, $
;                             geo=geo, trace=trace)
;
;     Input Arguments:  
;       in_lat        - input latitude (degrees)
;       in_lon        - input longitude (degrees)
;       height        - input altitude (km)
;
;     Output Arguments:  
;       out_lat       - transformed latitude (degrees)
;       out_lon       - transformed longitude (degrees)
;       r             - distance from origin (Re)
;       s             - non-zero integer code used to indicate failure.
;
;     Keywords:
;       geo           - perform inverse transformation: AACGM -> GEO
;       trace         - perform fieldline tracing instead of using coefficients
;       verbose       - debugging for fieldline tracing of inverse
;                       transformation. Should disable this feature for release
;       debug         - set keyword to use built-in IDL Legendre functions
;                       instead of Rylm procedure. Included for debugging of
;                       Rylm procedure. Remove for release.
;
; HISTORY:
;
; added keywords to pass through, otherwise unmodified from original.
; 20140827 SGS removed myear and fyear as arguments
;     
;+-----------------------------------------------------------------------------
;

function AACGM_v2_Convert, in_lat,in_lon,height, out_lat,out_lon,r, geo=geo, $
														trace=trace, bad_idea=bad_idea, $
														eps=eps, allow_trace=allow_trace, $
														verbose=verbose, debug=debug

	geo = keyword_set(geo)
	if n_elements(in_lat) ne 1 then begin
		n       = n_elements(in_lat)
		sze     = size(in_lat)
		out_lat = dblarr(sze[1:sze[0]])
		out_lon = dblarr(sze[1:sze[0]])
		r       = dblarr(sze[1:sze[0]])
		tmp_lat = 0.
		tmp_lon = 0.
		for i=0, n-1 do begin 
			AACGM_v2_ConvertGeoCoord, in_lat[i],in_lon[i],height[i], $
																tmp_lat,tmp_lon, error, geo=geo, $
																trace=trace, bad_idea=bad_idea, $
																allow_trace=allow_trace, eps=eps, $
																verbose=verbose, debug=debug
			out_lat[i] = tmp_lat
			out_lon[i] = tmp_lon
			r[i]       = 1.0
		endfor
	endif else begin
		out_lat = 0.
		out_lon = 0.
		AACGM_v2_ConvertGeoCoord, in_lat,in_lon,height, $
															out_lat,out_lon, error, geo=geo, $
															trace=trace, bad_idea=bad_idea, $
															allow_trace=allow_trace, eps=eps, $
															verbose=verbose, debug=debug
		r = 1.0
endelse

	return, error
end

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
; fieldline tracing routines
;
; REQUIRES that geopack be installed
;
; internal function used for fieldline tracing IGRF model
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_Newval
;
; PURPOSE:
;       Computes the vector of rate functions using the IGRF magnetic field at
;       the position given by the variable pxyz. The direction is given by
;       the signed variable idir and the stepsize is given by the variable ds
;   
; CALLING SEQUENCE:
;       AACGM_v2_Newval, pxyz, idir,ds
;
;     Input Arguments:  
;       pxyz          - 3-element array of the current position in Cartesian
;                       coordinates: pxyz = [x,y,z].
;       idir          - Direction along the field to move: +1 North, -1 South
;       ds            - Step size to move
;
;     Return Value:
;       knew          - array of rate function values for [x,y,z] coordinates
;
; HISTORY:
;
; Revision 1.0  14/06/10 SGS initial version
; 
; NOTES:
;
; geopack is required to be installed for this function to work
; 
;+-----------------------------------------------------------------------------
;

function AACGM_v2_Newval, pxyz, idir,ds

  geopack_sphcar, pxyz[0],pxyz[1],pxyz[2], r,theta,phi, /to_sphere
  geopack_igrf_geo, r,theta,phi, br,btheta,bphi
  geopack_bspcar, theta,phi, br,btheta,bphi, bx,by,bz

  bmag = sqrt(bx*bx + by*by + bz*bz)

  knew = ds*idir*[bx,by,bz]/bmag
  return, knew
end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_RK45
;
; PURPOSE:
;       Compute a single step along the magnetic field line using either a
;       fixed stepsize RK4 method or a Runge-Kutta-Fehlberg adaptive stepsize
;       ODE solver.
;   
; CALLING SEQUENCE:
;       AACGM_v2_RK45, x,y,z, idir, ds, eps, $
;                       fixed=fixed, max_ds=max_ds, RRds=RRds
;
;     Input Arguments:  
;       x,y,z         - Cartesian position of current location in Re
;       idir          - Direction along the field to move: +1 North, -1 South
;       ds            - Step size in units of Re
;       eps           - Global truncation error we wish to maintain, in units
;                       of Re
;
;     Keywords:
;       fixed         - set this keyword to do RK4 method with stepsize ds
;       max_ds        - maximum stepsize that is allowed, in units of Re
;       RRds          - set to use a maximum stepsize that is proportional
;                       to cube of the distance from the origin.
;
;     Return Value:
;       [x,y,z]       - Cartesian position of the next position
;
; HISTORY:
;
; Revision 1.0  14/06/10 SGS initial version
; 
; NOTES:
;
; geopack is required to be installed for this function to work
; 
;+-----------------------------------------------------------------------------
;

function AACGM_v2_RK45, x,y,z, idir, ds, eps, fixed=fixed, $
													max_ds=max_ds, RRds=RRds, verbose=verbose

	RE   =  6371.2		; magnetic reference spherical radius from IGRF

	; convert position to spherical coords
	geopack_sphcar, x,y,z, r,theta,phi, /to_sphere

	; compute IGRF field in spherical coords
	geopack_igrf_geo, r,theta,phi, br,btheta,bphi

	; convert field from spherical coords to Cartesian
	geopack_bspcar, theta,phi, br,btheta,bphi, bx,by,bz

	; magnitude of field to normalize vector
	bmag = sqrt(bx*bx + by*by + bz*bz)

	if keyword_set(fixed) then begin
		; RK4 Method
		k1 = ds*idir*[bx,by,bz]/bmag
		k2 = AACGM_v2_Newval([x,y,z] + .5*k1, idir,ds)
		k3 = AACGM_v2_Newval([x,y,z] + .5*k2, idir,ds)
		k4 = AACGM_v2_Newval([x,y,z] + k3, idir,ds)

		tmp = (k1 + 2*k2 + 2*k3 + k4)/6.
		x += tmp[0]
		y += tmp[1]
		z += tmp[2]
	endif else begin
		; Adaptive RK45 method
		RR = eps+1	; just to get into the loop
		while (RR gt eps) do begin
			k1 = ds*idir*[bx,by,bz]/bmag

			k2 = AACGM_v2_Newval([x,y,z] + k1/4., idir,ds)
			k3 = AACGM_v2_Newval([x,y,z] + 3.*k1/32. + 9.*k2/32., idir,ds)
			k4 = AACGM_v2_Newval([x,y,z] + 1932.*k1/2197. - 7200.*k2/2197. + $
														7296.*k3/2197., idir,ds)
			k5 = AACGM_v2_Newval([x,y,z] + 439.*k1/216. - 8.*k2 + 3680.*k3/513. - $
														845.*k4/4104., idir,ds)
			k6 = AACGM_v2_Newval([x,y,z] - 8.*k1/27. + 2.*k2 - 3544.*k3/2565. + $
														1859.*k4/4104. - 11.*k5/40., idir,ds)

			w1 = [x,y,z] + 25.*k1/216. + 1408.*k3/2565. + 2197.*k4/4104. - k5/5.
			w2 = [x,y,z] + 16.*k1/135. + 6656.*k3/12825. + 28561.*k4/56430. - $
											9.*k5/50. + 2.*k6/55.

			RR = abs(w1 - w2)/ds
			RR = sqrt(total(RR*RR))
			if keyword_set(verbose) then print, 'diff: ', RR
			if RR eq 0 then begin
				; it is possible for the two solutions to give the same answer, which
				; would correspond to an infinitely large stepsize
				if keyword_set(max_ds) then ds = max_ds		; limit the stepsize or
																									; just leave it alone
			endif else begin
				delt = 0.84 * (eps / RR)^0.25		; this formula relates the difference
                                        ; in the local trucation errors to the
                                        ; global error of the solution. There
                                        ; are lots of assumptions here...

				if keyword_set(verbose) then print, 'delt: ', delt
				newds = ds * delt
				ds = newds

				; maximum stepsize is fixed to max_ds in units of Re
				if keyword_set(max_ds) then	ds = min([max_ds,ds])

				; Setting this keyword uses a maximum stepsize that is proportional to
				; the distance from the origin, i.e., the further away from the origin
				; the larger the stepsize you can take to maintain the same accuracy.
				; Note that this is the maximum stepsize, if the algorithm says it
				; should be smaller, it will use the smaller.
				; Maximum stepsize is r^3 * 50km, where r is in units of Re
				if keyword_set(RRds) then		ds = min([50*r*r*r/RE, ds])
			endelse
		endwhile

		; we use the RK4 solution
		x = w1[0]
		y = w1[1]
		z = w1[2]
		; You might think that using the higher order RK5 method would be better,
		; but there is the suggestion that using the RK4 solution _guarantees_
		; accuracy while the RK5 does not. Apparently some texts are now
		; suggesting using the RK5 solution...
	endelse

	return, [x,y,z]
end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_Trace
;
; PURPOSE:
;
;       Perform field line tracing to determine AACGM coordinates
;
; CALLING SEQUENCE:
;       AACGM_v2_Trace, lat_in,lon_in,height_in, lat_out,lon_out, error
;
;     Input Arguments:  
;       lat_in        - latitude specified in geographic degrees
;       lon_in        - longitude specified in geographic degrees
;       height_in     - altitude above surface of Earth in kilometers
;
;     Output Arguments:  
;       lat_out       - latitude specified in AACGM degrees
;       lon_out       - longitude specified in AACGM degrees
;       error         - non-zero error code
;
;     Keywords:
;       ds            - starting stepsize in km
;       fixed         - set this keyword to do RK4 method with stepsize ds
;       eps           - global error in units of RE; CAREFUL using too small
;       max_ds        - maximum stepsize that is allowed, in units of Re
;
; HISTORY:
;
; new function
;     
;+-----------------------------------------------------------------------------
;

pro AACGM_v2_Trace, lat_in,lon_in,height_in, lat_out,lon_out, error, $
										fixed=fixed, ds=ds, eps=eps, max_ds=max_ds

	RE    = 6371.2							; magnetic reference spherical radius from IGRF
	if not keyword_set(ds) then $
		ds  = 1.									; 1 km default starting stepsize
	dsRE  = ds/RE
	dsRE0 = dsRE
	if not keyword_set(eps) then $
		eps = 1e-3/RE							; global error (RE)

	; if user wants to fix maximum step size then let them by turning off
	; radial step size dependence that is default
	if keyword_set(max_ds) then RRds = 0 else RRds = 1

	r     = (RE + height_in)/RE	; 1.0 is surface of Earth
	theta = (90.-lat_in)*!dtor	; colatitude in radians
	phi   = lon_in*!dtor				; longitude  in radians

	; convert position to Cartesian coords
	geopack_sphcar, r,theta,phi, x,y,z, /to_rect

	; convert to magnetic Dipole coordinates
	geopack_conv_coord, x,y,z, xx,yy,zz, /from_geo, /to_mag

	if zz gt 0 then idir = -1 else idir = 1		; N or S hemisphere

	dsRE = dsRE0

	; trace to magnetic equator
	;
	; Note that there is the possibility that the magnetic equator lies
	; at an altitude above the surface of the Earth but below the starting
	; altitude. I am not certain of the definition of CGM, but these
	; fieldlines map to very different locations than the solutions that
	; lie above the starting altitude. I am considering the solution for
	; this set of fieldlines as undefined; just like those that lie below
	; the surface of the Earth.

	while idir * zz lt 0 do begin

		xprev = x		; save for interpolation
		yprev = y
		zprev = z

		newpos = AACGM_v2_RK45(x,y,z, idir, dsRE, eps, $
																	fixed=fixed, max_ds=max_ds, RRds=RRds)
		; x,y,z are passed by reference and modified here...

		; convert to magnetic Dipole coordinates
		geopack_conv_coord, x,y,z, xx,yy,zz, /from_geo, /to_mag

	endwhile

	; now bisect stepsize (fixed) to land on magnetic equator w/in 1 meter
	xc = xprev
	yc = yprev
	zc = zprev

	while dsRE gt 1e-3/RE do begin
		dsRE *= .5
		xprev = xc
		yprev = yc
		zprev = zc
		newpos = AACGM_v2_RK45(xc,yc,zc, idir, dsRE, eps, /fixed)
		geopack_conv_coord, xc,yc,zc, xx,yy,zz, /from_geo, /to_mag

		; Is it possible that resetting here causes a doubling of the tol?
		if idir * zz gt 0 then begin
			xc = xprev
			yc = yprev
			zc = zprev
		endif
	endwhile

	; 'trace' back to surface along Dipole field lines
	Lshell = sqrt(xc*xc + yc*yc + zc*zc)
	if Lshell lt (RE+height_in)/RE then begin	; Magnetic equator is below your...
		lat_out = !values.f_nan
		lon_out = !values.f_nan
		error   = 1
	endif else begin
		geopack_conv_coord, xc,yc,zc, xx,yy,zz, /from_geo, /to_mag
		geopack_sphcar, xx,yy,zz, mr,mtheta,mphi, /to_sphere

		lat_out = -idir*acos(sqrt(1./Lshell))/!dtor
		lon_out = mphi/!dtor
		if lon_out gt 180 then lon_out -= 360		; SGS - make consistent with output
																						; from coefficient functions
		error   = 0
	endelse

end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_Trace_inv
;
; PURPOSE:
;
;       Perform field line tracing to determine geographic coordinates from
;       AACGM coordinates
;
; CALLING SEQUENCE:
;       AACGM_v2_Trace_inv, lat_in,lon_in,height_in, lat_out,lon_out, error
;
;     Input Arguments:  
;       lat_in        - latitude specified in AACGM degrees
;       lon_in        - longitude specified in AACGM degrees
;       height_in     - altitude above surface of Earth in kilometers
;
;     Output Arguments:  
;       lat_out       - latitude specified in geographic degrees
;       lon_out       - longitude specified in geographic degrees
;       error         - non-zero error code
;
;     Keywords:
;       ds            - starting stepsize in km
;       fixed         - set this keyword to do RK4 method with stepsize ds
;       eps           - global error in units of RE; CAREFUL using too small
;       max_ds        - maximum stepsize that is allowed, in units of Re
;
; HISTORY:
;
; new function
;     
;+-----------------------------------------------------------------------------
;

pro AACGM_v2_Trace_inv, lat_in,lon_in,height_in, lat_out,lon_out, error, $
										fixed=fixed, ds=ds, eps=eps, max_ds=max_ds, verbose=verbose

	RE    = 6371.2							; magnetic reference spherical radius from IGRF
	if not keyword_set(ds) then $
		ds  = 1.									; 1 km default starting stepsize
	dsRE  = ds/RE
	dsRE0 = dsRE
	if not keyword_set(eps) then $
;		eps   = 1e-4/RE						; global error (RE)
		eps   = 1e-2/RE						; global error (RE)

	; if user wants to fix maximum step size then let them by turning off
	; radial step size dependence that is default
	if keyword_set(max_ds) then RRds = 0 else RRds = 1

	; INV: for inverse we must first map AACGM to magnetic equator along
	;      the dipole field line that passes through the Earth at lat/lon
	if (abs(lat_in - 90.d) lt 1e-6) then lat_in = 90-1e-6
	Lshell = 1.d/(cos(lat_in*!dtor)*cos(lat_in*!dtor))

	if keyword_set(verbose) then print, Lshell

	if Lshell lt (RE + height_in)/RE then begin
		; solution does not exist, the starting position at the magnetic
		; equator is below the altitude of interest
		lat_out = !values.f_nan
		lon_out = !values.f_nan
		error   = 1
	endif else begin
		phim = lon_in

		; magnetic Cartesian coordinates of fieldline trace starting point
		xm = Lshell*cos(phim*!dtor)
		ym = Lshell*sin(phim*!dtor)
		zm = 0.d

		; geographic Cartesian coordinates of starting point
		geopack_conv_coord, xm,ym,zm, x,y,z, /from_mag, /to_geo

		; geographic spherical coordinates of starting point
		geopack_sphcar, x,y,z, r,theta,phi, /to_sphere

		; direction of trace is determined by the starting hemisphere?
		if lat_in gt 0 then idir = 1 else idir = -1   ; N or S hemisphere

		dsRE = dsRE0

		; trace back to altitude above Earth
		while r gt (RE + height_in)/RE do begin

			xprev = x		; save for interpolation
			yprev = y
			zprev = z

			if keyword_set(verbose) then print, 'xyz: ', x,y,z, dsRE
			newpos = AACGM_v2_RK45(x,y,z, idir, dsRE, eps, $
															fixed=fixed, max_ds=max_ds, RRds=RRds, $
															verbose=verbose)
			if keyword_set(verbose) then print, 'xyz: ', x,y,z, dsRE

			geopack_sphcar, x,y,z, r,theta,phi, /to_sphere

;			if keyword_set(verbose) then stop
		endwhile

		; now bisect stepsize (fixed) to land on magnetic equator w/in 1 meter
		xc = xprev
		yc = yprev
		zc = zprev

		while dsRE gt 1e-3/RE do begin
			dsRE *= .5
			xprev = xc
			yprev = yc
			zprev = zc
			newpos = AACGM_v2_RK45(xc,yc,zc, idir, dsRE, eps, /fixed)

			geopack_sphcar, xc,yc,zc, r,theta,phi, /to_sphere

			if r lt (RE + height_in)/RE then begin
				xc = xprev
				yc = yprev
				zc = zprev
			endif
		endwhile

		; record lat/lon and xyz
		lat_out = 90. - theta/!dtor
		lon_out = phi/!dtor
		error   = 0
	endelse

end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_Dayno
;
; PURPOSE:
;       Determine the day number of the given date.
;   
; CALLING SEQUENCE:
;       AACGM_v2_Dayno, yr,mo,dy, days=days
;
;     Input Arguments:  
;       yr            - 4-digit year
;       mo            - Month: 1-January, 2-February, etc.
;       dy            - Day of month, starting at 1
;
;       date inputs can be an array, but must be the same size and it is
;       assumed that each day is from the same year.
;
;     Keywords:
;       days          - set to a variable that will contain the total number of
;                       days in the given year.
;
;     Return Value:
;       dayno         - day number of the current year.
;
; HISTORY:
;
; Revision 1.0  14/06/10 SGS initial version
; 
;+-----------------------------------------------------------------------------
;

function AACGM_v2_Dayno, yr,mo,dy, days=days
	; works on an array. assume that all from same day
; WHAT IS THE POINT OF AN ARRAY OF THE SAME DAY?!

	mdays=[0,31,28,31,30,31,30,31,31,30,31,30,31]

	nelem = n_elements(yr)
	if (yr[0] ne yr[nelem-1]) or $
		(mo[0] ne mo[nelem-1]) or $
		(dy[0] ne dy[nelem-1]) then begin
			print, ''
			print, 'Not same day in AACGM_v2_Dayno'
			print, ''
			exit
	endif

	tyr = yr[0]
	; leap year calculation
	if tyr mod 4 ne 0 then inc=0 $
	else if tyr mod 400 eq 0 then inc=1 $
	else if tyr mod 100 eq 0 then inc=0 $
	else inc=1
	mdays[2]=mdays[2]+inc

	if keyword_set(days) then days = fix(total(mdays))

	if nelem eq 1 then $
		doy = total(mdays[0:mo[0]-1])+dy[0] $
	else $
		doy = intarr(nelem) + total(mdays[0:mo[0]-1])+dy[0]

	return, fix(doy)
end

;------------------------------------------------------------------------------
;
; NAME:
;       AACGM_v2_TimeInterp
;
; PURPOSE:
;       Determine whether coefficients need to be loaded and whether
;       interpolation of the newly loaded or existing coefficients need to
;       occur.
;   
; CALLING SEQUENCE:
;       AACGM_v2_TimeInterp
;
; HISTORY:
;
; Revision 1.0  14/08/27 SGS initial version
; 
;+-----------------------------------------------------------------------------
;
function AACGM_v2_TimeInterp

	common AACGM_v2_Com

	; set 5-year epoch based on the current year
	myear_v2 = fix(aacgm_v2_datetime.year/5)*5
	; set the floating-point year based on the current date and time
	fyear_v2 = aacgm_v2_datetime.year + ((aacgm_v2_datetime.dayno-1) + $ 
					(aacgm_v2_datetime.hour + (aacgm_v2_datetime.minute + $
					aacgm_v2_datetime.second/60.)/60.)/24.) / $
					aacgm_v2_datetime.daysinyear

	must_load = 0
	if (isa(myear_v2_old) eq 0) then begin
		; first time, so need to load new coefficients
		must_load = 1
	endif else if (myear_v2_old ne myear_v2) then begin
		; 5-year epoch year has changed, so need to load new coefficients
		must_load = 1
	endif

	; load the sets of coefficients
	if (must_load ne 0) then begin
		modyr  = myear_v2
		prefix = getenv('AACGM_v2_DAT_PREFIX')
		if (strlen(prefix) eq 0) then begin
			; prefix does not exist...
			print, 'Environment variable AACGM_v2_DAT_PREFIX is not set.'
			print, 'You must set this variable to the location and prefix, e.g., '
			print, "'aacgm_coeffs-11-', of the coefficient files on your system."
			return, -1
		endif

		;   prefix = 'aacgm_coeffs-leo-11-'
		print, 'Loading new coefficients: ', modyr, modyr+5
		fnamea = prefix+string(format='(i4.4)',modyr)+'.asc'
		fnameb = prefix+string(format='(i4.4)',modyr+5)+'.asc'
		openr, ua, fnamea, /get_lun,/stdio
		openr, ub, fnameb, /get_lun,/stdio
		s      = AACGM_v2_LoadCoef([ua,ub])
		free_lun, ua,ub

		; new coefficients so force both interpolations
		myear_v2_old = myear_v2
		fyear_v2_old = -1					; force time interpolation
		height_old_v2 = [-1.,-1.]	; force height interpolation
	endif

	if (fyear_v2 ne fyear_v2_old) then begin
		; here is the linear interpolation of the two bounding sets of coefficients
		coef_v2 = reform(coefs_v2[0,*,*,*,*]) + (fyear_v2 - myear_v2) * $
								reform(coefs_v2[1,*,*,*,*]-coefs_v2[0,*,*,*,*])/5

		height_old_v2[0] = -1.				; force height interpolation because coeffs
		height_old_v2[1] = -1.				; have changed

		fyear_v2_old = fyear_v2
	endif

	return, 0
end

pro aacgmlib_v2 
  
  aacgm_v2
  
end


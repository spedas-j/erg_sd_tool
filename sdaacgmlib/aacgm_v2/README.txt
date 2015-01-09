
Instructions:

1. Download the coefficients and put them in a convenient directory

2. Set the environment variable AACGM_v2_DAT_PREFIX to the directory that
   you are storing the coefficients in AND include the prefix of the
   coefficient files, i.e., aacgm_coeffs-11-

   e.g.,

   AACGM_v2_DAT_PREFIX=/mnt/thayerfs/shepherd/AACGM/idl/coeffs/aacgm_coeffs-11-

   Note that if you used the old AACGM software from JHU/APL you should have
   a similar variable that is already set.

3. Untar the contents of the .tar file into a directory and test it by running:

   idl test.idl

   from the command line. The output should be self-explanitory. Any errors
   should be reported to simon.shepherd@dartmouth.edu.

   Looking at test.idl will give you an idea of how to use the software. The
   main function, cnvcoord_v2(), is intended to be a direct replacement
   for the equivalent function, cnvcoord(), but with more options. The
   following is a description of the function cnvcoord_v2() taken from
   aacgm_v2.pro:

; CALLING SEQUENCE:
;       pos = cnvcoord_v2(inpos,[inlong],[height], [/GEO], $
																			[/TRACE], [/BAD_IDEA)
;
;     Note on input arguments:
;      the routine can be called either with a 3-element floating
;      point array giving the input latitude, longitude and height
;      or it can be called with 3 separate floating point values
;      giving the same inputs.
;      The input array can also be given in the form inpos(3,d1,d2,...)
;
;     Keywords:
;       geo           - set this keyword to convert from AACGM to geographic
;                       coordinates. The default is from geographic to AACGM
;       trace         - perform the slower but more accurate field line tracing
;                       to determine the coordinate conversion at all
;                       altitudes.  This feature is new and still in beta form.
;       allow_trace   - perform field line tracing for altitudes above 2000 km.
;                       The default is to throw an error if the altitude is
;                       above 2000 km. Because the tracing requires geopack
;                       to be installed, it is not acceptable to just load
;                       the required geopack functions.
;       bad_idea      - field line tracing is forced above 2000 km unless this
;                       keyword is set, in which case the coefficients will be
;                       used to extrapolate above the maximum altitude that
;                       is intended. Note that results can be nonsensical when
;                       using this option and you are acknowledging it by
;                       setting this keyword.
;
;     Output:  
;       pos           - a vector of the same form as the input with
;                       latitude and longitude specified in degrees and
;                       the distance from the origin in Re



This package include the following files:

AACGM IDL software:

aacgm_v2.pro          ; user functions
aacgmlib_v2.pro       ; internal library functions
test.idl              ; idl driver script


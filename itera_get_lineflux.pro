;+
; NAME:
;
;   ITERA_get_lineflux
;    
; PURPOSE:
;   
;   given a model series from ITERA, with 'a' values of the 1st
;   parameter (param1) and 'b' values of the 2nd parameter (param2), it
;   searches through each model of the series adding up all the fluxes
;   of the chosen lines, returning the summed line flux of these lines
;   for each model of the series in a [a,b] array.
;
; CALLING SEQUENCE:
;
;   linefluxes=itera_get_lineflux(lines,model_series)
;
; INPUTS
;
;   LINES: an array (can be single valued) of indices for the chosen
;          lines whose fluxes are to be summed (indices are
;          from the 'linelist.txt' file).
;
;   MODEL_SERIES: a model_series struct (can be one individual model)
;          from which the line fluxes are to be extracted.The
;          structure of the struct is defined in 'read_iteramodelseries.pro'
;
; OUTPUTS
;
;   Returns lineflux, an [a,b] sized array of summed line fluxes, where 'a' and
;   'b' are the number of 1st parameter (param1) and 2nd parameter
;   (param2) respectively.
;
; REQUIREMENTS
; 
;   The model series input here must come from the
;   read_iteramodelseries function.
;
; MODIFICATION HISTORY:
;   Brent Groves, July 2009
;
;##############################################################################
;
; LICENSE
;
;  Copyright (C) 2009 Brent Groves
;
;  This file is part of ITERA.
;
;  ITERA is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 3 of the licence, or
;  (at your option) any later version.
;
;  ITERA is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with ITERA (gpl.txt).  If not, see
;  http://www.gnu.org/licenses/. 
;
;##############################################################################
;
FUNCTION ITERA_get_lineflux, lines, model_series

 Catch, theError
 IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   Print, ''
   Help, /Last_Message, Output=theErrorMessage
   FOR j=0,N_Elements(theErrorMessage)-1 DO BEGIN
      Print, theErrorMessage[j]
   ENDFOR
   RETURN,-1
 ENDIF

 IF size(model_series,/TYPE) NE 8 THEN $
      MESSAGE,'Input Model Series not in structure form.'
 IF n_elements(lines) EQ 0 THEN MESSAGE,'No Lines were input'
; 
; Have to allow for absent models
;
 n_param2=n_elements(model_series.param2)
 maxparam1=0
 FOR i=0,n_param2-1 DO BEGIN
   n_param1=n_elements((*(model_series.models)[i]).hbeta)
   IF n_param1 GT maxparam1 THEN BEGIN 
     maxparam1=n_param1
     Param1vals=(*(model_series.models)[i]).(5)
   ENDIF
 ENDFOR
 n_param1=maxparam1
 lineflux=fltarr(n_param2,n_param1)
;
;  There must be an easier way of doing this, but I cant think of it
;  at the moment, so currently use triple loop structure to find all
;  relevant lines in the model data.
;
 FOR lind=0L,n_elements(lines)-1 DO BEGIN ; loop over the lines to be added
   FOR i=0L,n_param2-1 DO BEGIN ; loop over parameter2 (ie B)
     model=model_series.models[i]
     FOR j=0L,n_param1-1 DO BEGIN ; loop over parameter1 (ie velocity or U)
       mind=WHERE((*model).(5) EQ Param1vals[j])
       IF mind NE -1 THEN BEGIN
         lineindex=WHERE((*model).line_ind[mind,*] EQ lines[lind],n_ind)
         IF n_ind NE 0 THEN $
           lineflux[i,j] = lineflux[i,j] + $
              (*model).line_flux[mind,lineindex]
       ENDIF
     ENDFOR 
   ENDFOR  
  ENDFOR
 RETURN,lineflux
END

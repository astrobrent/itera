;+
; FUNCTION NAME:
;    
;    READ_LINELIST
;
; PURPOSE:
; 
;    Reads in the linelist.txt file return full list of all
;    MAPPINGS III spectral lines.
;
; KEYWORDS:
;
;    FILEPATH: the full file path for the line list file. If
;              not present assumes './linelist.txt'
;
; OUTPUTS:
;
;    LINELIST: A stucture containing:  
;                   - line index (LONG)
;                   - Element (STRING)
;                   - Ion (STRING)
;                   - Freq_eV (FLOAT): frequency in electron Volts
;                   - Wave_A (FLOAT): Line wavelength in Angstrom
;
; EXAMPLE:
;
;    linelist=read_linelist(FILEPATH='../linelist.txt')
;
; AUTHOR: 
;  
;    Brent Groves
;    Email - brent@strw.leidenuniv.nl
;
; EXTERNAL: 
;   
;    Uses the idl routine READ_ASCII
;
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
FUNCTION read_linelist, filepath=file
 Catch, theError
 IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   Print, ''
   Print, 'READ_LINELIST error: ' + !Error_State.Msg
   RETURN,-1
 ENDIF
 
 line_template={VERSION:1.00000,$
                DATASTART:1L,$
                DELIMITER:32B,$
                MISSINGVALUE:!VALUES.F_NAN,$
                COMMENTSYMBOL:'',$
                FIELDCOUNT:5L,$
                FIELDTYPES:[3L,7L,7L,5L,5L],$
                FIELDNAMES: ['INDEX','ELEMENT','ION','FREQ_EV','WAVE_A'],$
                FIELDLOCATIONS:[4L,8L,14L,21L,35L],$
                FIELDGROUPS:[0L, 1L, 2L, 3L, 4L] }
 
 IF n_elements(file) EQ 0 THEN file='linelist.txt'
 IF ~FILE_TEST(file) THEN BEGIN 
   print,'File ',file,' not found. Returning...'
   RETURN,-1
 END
 linelist=READ_ASCII(file,template=line_template)
;
; sort line list, just incase
;
 sorted_linelist=linelist
 sort_index=SORT(linelist.wave_a)
 FOR i=0,N_tags(linelist)-1 DO sorted_linelist.(i)=linelist.(i)[sort_index]

 RETURN,sorted_linelist

END

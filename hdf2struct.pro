;+
; FUNCTION NAME: 
;
;   HDF2STRUCT
;
; PURPOSE:
;
;   Reads in a HDF_SD file and converts the data to a struct
;   See the associated program STRUCT2HDF
;
; INPUT: 
;
;   file: The name of the hdf file to be read in and converted
;
; KEYWORD PARAMETERS:
;
;   VERBOSE: If set, function outputs details of the HDF file being
;   read in, such as number of tags and dimensions
;
; OUTPUT:
;
;   Stucture containing HDF data, with tag names from hdf list
;
; EXAMPLE:
;
;   data_struct=hdf2struct('data_file.hdf')
;
; AUTHOR: 
;
;   Brent Groves
;   Email - brent@strw.leidenuniv.nl
;
;ACKNOWLEDGEMENTS:
;
;    Based upon the codes of Dr. Jarle Brinchmann and the
;    HDF_SAVE_STRUCT code of Liam Gumley
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
FUNCTION hdf2struct, file,verbose=verbose_key

 Catch, theError
 IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   Print, ''
   Print, 'HDF2STRUCT error: ' + !Error_State.Msg
   RETURN,-1
 ENDIF
;
; Read in the data in a HDF file and put in a struct.
;
 IF (~FILE_TEST(file)) THEN BEGIN
   print,'HDF file: ',file,' doesnt exist'
   RETURN, -1
 ENDIF 

 sd_id = HDF_SD_START(file, /READ)
 IF (sd_id EQ -1) THEN BEGIN
    print, 'Opening of '+file+' failed!'
    RETURN, -1
 ENDIF

 HDF_SD_FILEINFO, sd_id, n_datasets, n_attributes

;
; Read the HDF file in reverse order so that struct is returned in
; original form
 FOR i=n_datasets-1,0,-1 DO BEGIN

   sds_id = HDF_SD_SELECT(sd_id, i)
    
   HDF_SD_GETINFO, sds_id, name=name, dims=dims, ndims=ndims, type=type
   IF n_elements(verbose_key) NE 0 THEN $
     print,'sds_id: ',sds_id,'Name: ',name,$
           ', of idltype ',type,' and dims ,',dims  
       
   HDF_SD_GETDATA, sds_id, data
   IF (type EQ 'BYTE') THEN BEGIN
     IF (HDF_SD_ATTRFIND(sds_id,'STRING') NE -1) THEN $
       data = string(data)
   ENDIF

   IF (n_elements(str) EQ 0) THEN $
     str = create_struct(name, data) $
   ELSE $ 
     str = create_struct(name, data, str)

   HDF_SD_ENDACCESS, sds_id
 ENDFOR

;----------------------------------------------------------------
; Finally add the attributes - Notice that the way of
; reading/writing these means that the order of elements in the
; final struct will be different!
;----------------------------------------------------------------
 FOR i=0, n_attributes-1 DO BEGIN
   hdf_sd_attrinfo, sd_id, i, data=data, name=name

   ;; Scalarify.
   IF (n_elements(data) EQ 1) THEN data = data[0]
   IF (n_elements(str) EQ 0) THEN $
    str = create_struct(name, data) $
   ELSE $
  str = create_struct(name, data, str)
 ENDFOR

 HDF_SD_END, sd_id
 IF (n_elements(str) EQ 0) THEN str = -1
 RETURN, str
END



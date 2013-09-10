;+
; NAME: 
;
;    struct2hdf
;
; PURPOSE:
;
;    Converts a given structuture (strct) to a HDF_SD (Hierachical
;    Data Format - Scientific Data) file, whose name
;    is given by the keyword FILENAME, with the default 'idl.hdf'.
;    See the associated program HDF2STRUCT to read in the resulting file
;
; CALLING SEQUENCE:
;
;    struct2hdf(strct, [FILENAME=, DIR=])
;
; INPUTS:
;    
;    strct: The structure file to be converted into a HDF-SD
;      file
;
; KEYWORD PARAMETERS: 
;
;    FILENAME: The file which the HDF-SD output is to be written
;      to. If no name is given, the file will be written to "idl.hdf".
;
;    DIR: The directory which the output file is written to. If no
;    directory is given it is assumed to be the current directory.  
;
; AUTHOR: 
;    
;    Brent Groves
;    Email - brent@strw.leidenuniv.nl
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
PRO struct2hdf, strct, FILENAME=outfile, DIRECTORY=dir

 Catch, theError
 IF theError NE 0 THEN BEGIN
   Catch, /Cancel
   Print, ''
   Print, 'STRUCT2HDF error: ' + !Error_State.Msg
   RETURN
 ENDIF

 IF (n_elements(strct) EQ 0) THEN BEGIN
   print, "Usage as follows:"
   print, "STRUCT2HDF,structure,FILENAME=output_file,DIRECTORY=dir"
   print, "where structure must be of type structure, and"
   print, "the FILENAME and DIRECORY are path strings"
   RETURN
 ENDIF
 IF (n_elements(outfile) EQ 0) THEN outfile ='idl.hdf'
 IF (n_elements(dir) EQ 0) THEN dir=''

 datanames=tag_names(strct)
 n_data=n_elements(datanames)

 sd_id = HDF_SD_START(DIR+outfile, /CREATE)
 FOR i=0L,n_data-1 DO BEGIN
   info = size(strct.(i))
   ndim = info[0]
   IF ndim GT 0 THEN datasize = [info[1:ndim]] ELSE datasize = [1]
   type = info[ndim + 1]
   nele = info[ndim + 2]
   IF type EQ 7 THEN BEGIN 
      datasize=[max(strlen(strct.(i))),datasize]
      data=byte(strct.(i))
   ENDIF ELSE data=strct.(i)

; Create the variable if supported by HDF
   saveflag = 1B
   CASE type OF
     1  : sds_id = HDF_SD_CREATE(sd_id, datanames[i], datasize, /byte)
     2  : sds_id = HDF_SD_CREATE(sd_id, datanames[i], datasize, /int)
     3  : sds_id = HDF_SD_CREATE(sd_id, datanames[i], datasize, /long)
     4  : sds_id = HDF_SD_CREATE(sd_id, datanames[i], datasize, /float)
     5  : sds_id = HDF_SD_CREATE(sd_id, datanames[i], datasize, /double)
     7  : BEGIN 
          sds_id = HDF_SD_CREATE(sd_id, datanames[i], datasize, /byte)
          HDF_SD_ATTRSET,sds_id,'STRING',1B,/BYTE
     END
     ELSE : BEGIN 
       print, 'The variable type '+size(strct.(i),/tname)+$
              ' is not supported by HDF:'
       saveflag = 0B
     END
   ENDCASE
   IF saveflag THEN BEGIN
;   Turn on compression
     HDF_SD_SETCOMPRESS, sds_id, 4
;   Add data to HDF file.
     HDF_SD_ADDDATA, sds_id, data
;   Close this SDS
     HDF_SD_ENDACCESS, sds_id
   ENDIF
 ENDFOR
; Close the HDF file 
 HDF_SD_END, sd_id
 RETURN
END


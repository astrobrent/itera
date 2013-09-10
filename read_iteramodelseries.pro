;+
; NAME:
;
;   read_ITERAmodelseries 
;    
; PURPOSE: 
;  
;   Reads in a model series for itera. This model series (given in
;   FILE_LIST) is a group (of at least one) of hdf files contianing
;   the parameters and emission lines of photionization and shock
;   models.    
;
; CALLING SEQUENCE:
;
;   i.e. model=read_ITERAmodelseries(FILE_LIST="*.hdf",
;                               MODELSETNAME="model name")
; OUTPUT:
;
;   Returns a struct file containing (where #models is the number of
;   model files read in);
;      model_struct={NAME:modelsetname,
;                    METAL:metal[#models],
;                    DENS:dens[#models],
;                    PARAM2:param2[#models],
;                    MODELS:models[[#models] }
;
;   "models" is the actual data from the models, read in by HDF2STRUCT,
;   giving all model parameters, the individual model name and the
;   emission lines (relative to H-beta) from that model.
;
; KEYWORD PARAMETERS:
;
;   FILE_LIST: This is the list of model files (HDF format) to be read
;      in. This can either be an individual file or a group of files,
;      defined using the group operators (ie *.hdf). This is necessary
;      input for the program. Without this, the function prints an
;      example input and returns -1. 
;  
;   MODELSETNAME: This is the name for the model series read in via
;      file_list. If left empty, the file_list string is used.
;
; EXTERNAL:
;
;   Uses the following associated ITERA external programs:
;     HDF2STRUCT
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
FUNCTION read_ITERAmodelseries,file_list=file_list,modelsetname=modelsetname,$
                             metal=metal,dens=dens,type=type, param2=param2

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

 IF keyword_set(file_list) THEN BEGIN 
   series_files=file_list 
   series_name= file_list
 ENDIF ELSE BEGIN 
   print, 'Example Input'
   print, '   Read_ITERAmodelseries(FILE_LIST="file_list.hdf", [MODELSETNAME=""])'
   RETURN,-1
 ENDELSE

 files=FILE_SEARCH(series_files,count=n_files) 
 IF n_files EQ 0 THEN BEGIN
  print,'Series incorrect :',series_files
  RETURN,-1
 ENDIF
 models=PTRARR(n_files,/ALLOCATE_HEAP)
 FOR i=0,n_files-1 DO BEGIN
   model=hdf2struct(files[i])
   *models[i]=model
 ENDFOR 
; Allow for missing or irregular arrays 
 metalarr=MAKE_ARRAY(n_files,type=size((*models[0]).metal,/type))
 densarr=MAKE_ARRAY(n_files,type=size((*models[0]).dens,/type))
 param2arr=MAKE_ARRAY(n_files,type=size((*models[0]).(4),/type))

 FOR i=0,n_files-1 DO BEGIN
   metalarr[i]=(*models[i]).metal
   densarr[i]=(*models[i]).dens
   param2arr[i]=(*models[i]).(4)
 ENDFOR 
 index=sort(param2arr)
 Model_series={Name:keyword_set(modelsetname) ? modelsetname : series_name,$
               Metal:metalarr[index],$
               Dens:densarr[index],$
               param2:param2arr[index],$
               models:models[index] }
 RETURN,model_series
END

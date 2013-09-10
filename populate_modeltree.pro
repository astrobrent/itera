;+
; NAME:
;
;   populate_modeltree
;    
; PURPOSE:
;
;   The purpose of this program is to populate the model tree widget
;   (indicated by the RootID) in ITERA. It does this by finding all
;   HDF files in the given directory If other directories are found in
;   the given directory it will search these directories by
;   recursively calling the same program (populate_modeltree). If no
;   model files are found, the model tree root is not populated. 
;       
;
; INPUT
;
;   ROOT_ID: The ID of the Root model tree widget, in which to
;      populate the model "leaves". If called recursively, its the ID of
;      the marked directory root.
;
;   DIRECTORY: The directory in which to search the HDF model files. 
;
; KEYWORD PARAMETERS:
;
;   FREE_PARAM: This is the "free parameter" with which to group all
;      HDF files in a directory. This can have one of 3 values;
;         free_param= 1: Metallicity/Abund.              
;                   = 2: Density                         
;                   = 3: Unique parameter (B, SB age etc)
;      The other 2 parameters will be listed as individual values in
;      the model tree.If this parameter is unset, the unique
;      parameters are grouped (free_param=3).
;
; OUTPUT:
;
;   Returns a struct with all the model "leaf" IDs and directory IDs.
;
; REQUIREMENTS
; 
;   This program is a subset of the itera proprogram, and requires the
;   setup of the main widget box for the modeltree before calling. 
;
; WARNING:
;
;   As this program recursively calls itself upon finding of
;   directories, this program can be caught in a loop if links to
;   lower order directories exist in the currently searched
;   directory. 
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
FUNCTION populate_modeltree,rootID,directory,free_param=free_param
 ON_ERROR,2
;
; Set Free parameter
 IF n_elements(free_param) NE 1 THEN free_param=3
 IF ((size(free_param,/type) NE 2) OR (size(free_param,/type) NE 3)) $
  AND ((free_param GT 3) OR (free_param LT 1)) THEN free_param=3

;
; Do directories, and recursively call program for sub-directories
 datadir=FILE_SEARCH(directory+'*', COUNT=n_dir,/FULLY_QUALIFY_PATH,$
                     /TEST_DIR)
 tree_struct=CREATE_STRUCT('ROOT',directory)
 IF n_dir NE 0 THEN BEGIN
  dirend=strpos(datadir[0],'/', /REVERSE_SEARCH)+1 ;all in same directory
  dirnames=STRARR(n_dir)
  dirIDs=LONARR(n_dir)
  FOR i=0,n_dir-1 DO BEGIN
    dirnames[i]=STRMID(datadir[i],dirend,STRLEN(datadir[i]))
    dirIDs[i]=WIDGET_TREE(rootID,/FOLDER, VALUE=dirnames[i])
    subdata=populate_modeltree(dirIDs[i],datadir[i]+'/',free_param=free_param)
    tree_struct= CREATE_STRUCT(tree_struct,$
                  dirnames[i]+'_ID',dirIDs[i],$
                  dirnames[i]+'_DATA',subdata)
  ENDFOR
 ENDIF
;
; Now add files in root directory
 files=FILE_SEARCH(directory+'*.hdf', COUNT=n_files,/FULLY_QUALIFY_PATH)
 IF n_files NE 0 THEN BEGIN 
   models=STRARR(n_files)
   dirend=strpos(files[0],'/', /REVERSE_SEARCH)+1 ;all in same directory
   fulldir=STRMID(files[0],0,dirend)
   FOR i=0L, n_files-1 DO BEGIN
     modelname=strmid(files[i],dirend,STRLEN(files[i])-(dirend+4))
     params=STRSPLIT(modelname,'_',/EXTRACT,COUNT=n_params)
     IF n_params NE 4 THEN print,' File Incorrect!'
     CASE params[3] OF 
      's'  : begin
          modeltype='S: '
          uniqueparam=' B='
          uniquecut=1
      end
      'p'  : begin
          modeltype='P: '
          uniqueparam=' B='
          uniquecut=1
      end
      'sp' : begin
          modeltype='SP: '
          uniqueparam=' B='
          uniquecut=1
      end
      'dagn': begin
          modeltype='Dusty AGN: '
          uniqueparam='alpha='
          uniquecut=3
      end
       'dfagn': begin
          modeltype='Dustfree AGN: '
          uniqueparam='alpha='
          uniquecut=3
      end
     'Peg' : begin
          modeltype='Peg v2.0 SB: '
          uniqueparam=' Age='
          uniquecut=0
      end
      'SB99' : begin
          modeltype='SB99: '
          uniqueparam=' Age='
          uniquecut=0
    end
      'SB99L' : begin
          modeltype='SB99 Lej. Atm.: '
          uniqueparam=' Age='
          uniquecut=0
      end
      'SB99K' : begin
          modeltype='SB99 Kur. Atm.: '
          uniqueparam=' Age='
          uniquecut=0
      end
      'cparam' : begin
          modeltype='C param: '
          uniqueparam=' log R'
          uniquecut=4
       end
      'rparam' : begin
          modeltype='R param: '
          uniqueparam=' log R'
          uniquecut=4
       end
      'umodel' : BEGIN
          modeltype='Umodel:'
          uniqueparam='P2:'
          uniquecut=2        
      END
      ELSE: BEGIN
       print,' No match found for ',modelname
       RETURN,-1
      ENDELSE
     ENDCASE
     params[free_param-1]='*'
     models[i]=modeltype
     IF params[0] NE '*' THEN models[i] = models[i]+params[0]+' Abn,'
     IF params[1] NE '*' THEN $
        models[i] = models[i]+'n='+STRMID(params[1],1,STRLEN(params[1])-1)
     IF params[2] NE '*' THEN models[i] = models[i]+uniqueparam+$
         STRMID(params[2],uniquecut,STRLEN(params[2])-1)
        
     files[i]=fulldir+params[0]+"_"+params[1]+"_"+params[2]+"_"+params[3]+".hdf"
   ENDFOR
   sortind=sort(files)
   files=files[sortind]
   models=models[sortind]
   set_index=UNIQ(files)
   model_sets=models[set_index]
   model_filesets=files[set_index]
   n_modelsets=n_elements(set_index)
   leafID=lonarr(n_modelsets) 
   FOR i=0, n_modelsets-1 DO BEGIN
      leafID[i]=WIDGET_TREE(rootID,VALUE=model_sets[i],$
                                 UVALUE=model_filesets[i])
   ENDFOR
   IF n_elements(tree_struct) NE 0 THEN $ 
      tree_struct=CREATE_STRUCT(tree_struct,'leafIDs',leafID) $
   ELSE $
      tree_struct=CREATE_STRUCT('leafIDs',model_filesets)
 ENDIF
 RETURN,(n_elements(tag_names(tree_struct)) GT 1)? tree_struct : -1
END

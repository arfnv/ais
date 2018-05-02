;;/**********************************************************************************
;;    Copyright (C) 2008 Investment Science Corp.
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;***********************************************************************************/
;;
;;  
;;  Title:    Default Libraries Update 
;;
;;  Author:   Tim May
;;
;;  Project:  Libraries
;;
;;  Notes:    No other dependencies.  
;;   

;; *******************************************************************
;; Application aisapp.ini file overrides.
;; *******************************************************************
;#ClientViewUrl=http://$MachineName$/view.htm
;#HttpDir=../wwwroot
;#memory=1000

;; *******************************************************************
;; Load C libraries
;; *******************************************************************
;(loadLib "xml")
;(setq oldXml xml)

;; *******************************************************************
;; Load browse Lambda from a central location
;; *******************************************************************
(runScript "BrowseLib/BrowseLib.sl")

;; Add libs information to _ais structure
(setq _ais.libs #(
	;; cabinet name, database name, one or more source files to place in cabinet
	;; Temporarily take out browse Lambda, it causes a problem when loading
	;;#(BrowseLib:,"BrowseLib.db",false,"BrowseLib/BrowseLib.sl")
 ;; Load BrowseLib first, loading it last removes previous data for some reason
	#(DataMineLib:,"DataMineLib.db",true,"DataMineLib/DataMineLib.sl")
	#(Index:,"Index.db",true,"Index/Index.sl")
	#(ParseLib:,"ParseLib.db",true,"ParseLib/ParseLib.sl")
	#(JavaScript:,"JavaScript.db",true,"JavaScript/JavaScript.sl")
	#(RulesLib:,"RulesLib.db",true,"RulesLib/RulesLib.sl")
	#(Math:,"Math.db",true,"Math/Math.sl")
	#(Xml:,"Xml.db",true,"Xml/Xml.sl")
	#(Svm:,"Svm.db",true,"Svm/Svm.sl")
	#(Esm:,"Esm.db",true,"Esm/Esm.sl")
	#(Gsm:,"Gsm.db",true,"Gsm/Gsm.sl")
	)
)

(defun loadLibs()
	vars:(i libs)
	(setq libs _ais.libs)
	(loop for i from 0 until (length libs) do
	   (if (= libs[i][2] true)
		   (browseLib libs[i][0] libs[i][1] libs[i][3] "file" "ask" "ask" "true" "true")
           ;; Using fixed values for other arguments to addExtent
		   (browseLib.addExtent libs[i][0] libs[i][1] libs[i][3] "file" "ask" "ask" "true" "true")
		   ) ; end if
	) ; end loop
true) ; end loadLib

(loadLibs) 
          
(defun libupdate() 
	vars:(i c libs children status)
	(setq libs _ais.libs)
	(loop for i from 0 until (length libs) do
	 	;; clear existing cabinet repository
	 	(browseLib.setFocus libs[i][0])
        (setq status (browseLib.checkStatus libs[i][0]))
        ;; Check current status of cabinet, skip it if is up-to-date
        (if (= (find "nothing" status) 0)
            (begin
            (writeln libs[i][0]" is up-to-date.")
            (goto SKIP:)
            )
        )
	 	(setq children (browseLib.getChildNames)) 
	 	(loop for c from 0 until (length children) do
	 	 	(browseLib.eraseChildren children[c])
	 	)
	 	;; import new source
	 	(loop for c from 3 until (length libs[i]) do
			(writeln "Importing " libs[i][c])
			(browseLib.importSource libs[i][c])	
	 	)
        SKIP::
	)
true)

(browseLib.bind)
(lock _globals)
(writeln "Welcome to library update. Execute (libupdate) to update current library cabinets.")


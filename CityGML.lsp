
(Defun c:CityGML () (vl-load-com) (setq selset (ssget (list (cons 0 "polyline")))) (if selset (progn (setq xmlpath (strcat (getvar "dwgprefix") (vl-filename-base (getvar "Dwgname")) ".xml")) (setq xmlfile (open xmlpath "w")) 
(write-line (Strcat "") xmlfile) 
(write-line (strcat "http://www.citygml.org/citygml/1/0/0" (chr 34) " xmlns:gml=" (chr 34) "http://www.opengis.net/gml" (chr 34) " xmlns:xlink=" (chr 34) "http://www.w3.org/1999/xlink" (chr 34) " xmlns:xsi=" (chr 34) "http://www.w3.org/2001/XMLSchema-instance" (chr 34) " xsi:schemaLocation=" (chr 34) "http://www.citygml.org/citygml/1/0/0 http://www.citygml.org/citygml/1/0/0/CityGML.xsd" (chr 34) ">") xmlfile) (write-line "City Exporeted by Infotech" xmlfile) 
(setq count 0) (setq setlen (sslength selset)) (while (<>\n \n \n \n \n ") xmlfile) (foreach extval extlist (CityGML_Write_Func xmlfile extval) ) (write-line (Strcat " \n \n \n \n \n ") xmlfile) ) ) (setq count (1+ count)) ) (write-line "" xmlfile) (close xmlfile) ) )) 
;;; ! ************************************************************************************************************************************************************************* 
(defun CityGML_Write_Func (xmlfile lstvals ) 
(write-line (strcat " \n \n \n \n ") xmlfile) 
(setq prntvals (mapcar '(lambda(x) (mapcar '(lambda (y) (rtos y)) x)) lstvals)) (setq retvals (INFO-LIST->STRING (mapcar '(lambda (x) (INFO-LIST->STRING x " ")) prntvals) " ")) 
(write-line (strcat "\n " retvals "") xmlfile) 
(write-line "\n \n \n \n \n " xmlfile) ) 
;;; ! ************************************************************************************************************************************************************************* 



;;; ! ************************************************************************************************************************************************************************* 
(defun Genrate_Extrude (Ent) (setq retlist '()) (setq ptlist (Info-GetVertices Ent)) 
(command "pline" ) (apply 'command ptlist) (command "c") (setq lent (entlast)) 
(setq chkflag (Lwcl lent)) (if chkflag (setq ptlist (reverse ptlist)) ) 
(command "erase" lent "") 
(setq retlist (append retlist (list ptlist))) ;; Top of Extrude ;; bottom of extrude.... 
(setq botlist '()) 
(foreach pt ptlist (setq npt (list (car pt) (cadr pt) 0.0)) (setq botlist (append botlist (list npt))) ) (setq retlist (append retlist (list botlist))) ;; bottom complete... ;; sides of extrude.... 
(setq sidelist '()) (setq ctr 0) (while (< ctr (- (length ptlist) 1)) (setq xpt (nth ctr ptlist)) (setq ypt (nth (1+ ctr) ptlist)) (setq dummylist '()) (setq dummylist (list (list (car xpt) (cadr xpt) 0.0) (list (car ypt) (cadr ypt) 0.0) ypt xpt (list (car xpt) (cadr xpt) 0.0) ) ) (setq sidelist (append sidelist (list dummylist))) (setq ctr (1+ ctr)) ) (setq retlist (append retlist sidelist))) 
;;; ! ************************************************************************************************************************************************************************* 
;;; To return the vertices of the given entity 
(defun Info-GetVertices (enty) 
(cond ((and enty (setq obj (vlax-ename->vla-object enty))) 
(setq objName (vla-get-objectname obj)) 
(cond ((and (= objName "AcDb3dPolyline") 
(Setq coors (vla-get-coordinates obj)) 
(setq coors (vlax-variant-value coors)) 
(setq coors (vlax-safearray->list coors)) 
) 
(setq retlst (INFO-CREATELISTSFROMTHELISTOFVALUES coors 3)) 
) 
((and (= objName "AcDbPolyline") 
(Setq coors (vla-get-coordinates obj)) 
(setq coors (vlax-variant-value coors)) 
(setq coors (vlax-safearray->list coors)) 
) 
(setq retlst (INFO-CREATELISTSFROMTHELISTOFVALUES coors 2)) 
) 
((setq eprp (entget enty)) 
(setq retlst '()) 
(while (or (setq vertex (assoc 10 eprp)) (setq vertex (assoc 11 eprp))) 
(setq coors (cdr vertex)) 
(if (not (member coors retlst)) 
(setq retlst (append retlst (list coors))) 
) 
(setq eprp (vl-remove vertex eprp)) 
) 
) 
) 
) 
) 
retlst 
) 
;;; ! ************************************************************************************************************************************************************************* 

;;; To Create the Lists From the List of Values with Specified number of Values 
(defun Info-CreateListsFromTheListOfValues (listofValues NumVals) 
(setq retVal '()) 
(setq cntr 1) 
(setq coor '()) 
(cond ((= (rem (length listofValues) NumVals) 0) 
(repeat (length listofValues) 
(cond ((= cntr NumVals) (setq cntr 1) (setq coor (append coor (list (nth 0 listofValues))))) 
((< cntr NumVals) (setq cntr (1+ cntr)) (setq coor (append coor (list (nth 0 listofValues))))) 
) 
(cond ((= (length coor) NumVals) 
(if (not retVal) 
(setq retVal (list coor)) 
(setq retVal (append retVal (list coor))) 
) 
(setq coor '()) 
) 
) 
(Setq listofValues (Cdr listofValues)) 
) 
) 
) 
retVal 
) 
;;; ! ************************************************************************************************************************************************************************* 
(defun Info-List->String (listofValues DelimChar) (if (not DelimChar) (setq DelimChar ",") ) (if (not (setq chkval (vl-remove 'STR (mapcar 'type listofValues)))) (if listofValues (vl-string-trim DelimChar (apply 'strcat (mapcar (function (lambda (x) (strcat x DelimChar))) listofValues)) ) ) )) 
;;; ! ************************************************************************************************************************************************************************* 
(defun Lwcl (ent / LW LST MAXP MINP) ; Writer Evgeniy Elpanov. (setq lw (vlax-ename->vla-object ent)) (vla-GetBoundingBox lw 'MinP 'MaxP) (setq minp (vlax-safearray->list minp) MaxP (vlax-safearray->list MaxP) lst (mapcar (function (lambda (x) (vlax-curve-getParamAtPoint lw (vlax-curve-getClosestPointTo lw x) ) ;_ vlax-curve-getParamAtPoint ) ;_ lambda ) ;_ function (list minp (list (car minp) (cadr MaxP)) MaxP (list (car MaxP) (cadr minp)) ) ;_ list ) ;_ mapcar ) ;_ setq (if (or (<= (car lst) (cadr lst) (caddr lst) (cadddr lst)) (<= (cadr lst) (caddr lst) (cadddr lst) (car lst)) (<= (caddr lst) (cadddr lst) (car lst) (cadr lst)) (<= (cadddr lst) (car lst) (cadr lst) (caddr lst)) ) ;_ or t ) ;_ if) ;_ defun 
;;; ! ************************************************************************************************************************************************************************* 

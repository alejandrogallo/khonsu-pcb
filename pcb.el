;;; pcb.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Alejandro Gallo

;; Author: Alejandro Gallo <gallo@cqc02>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; https://dev-docs.kicad.org/en/file-formats/sexpr-intro/index.html

(require 'cl-lib)
(require 'dash)

(defvar pcb-kicad-modules nil)
(defun pcb-refresh-kicad-cache ()
  (interactive)
  (setf pcb-kicad-modules
        (directory-files-recursively "." ".*.kicad_mod")))

(defun pcb-find-module (name)
  "Get a module as a list by NAME."
  (unless pcb-kicad-modules
    (pcb-refresh-kicad-cache))
  (let ((found (cl-remove-if-not (lambda (path)
                                   (string-match-p
                                    (format ".*%s.kicad_mod$" name) path))
                                 pcb-kicad-modules)))
    (unless found
      (error "Could not find the modules with name %s" name))
    (with-current-buffer (find-file-noselect (car found))
      (read (buffer-string)))))

(defun pcb-extract-nets (modules)
  "Extract nets informations from a list of MODULES or any s-expression really."
  (let (nets)
    (-tree-map-nodes (lambda (x) (pcase x
                                   (`(net ,_ ,_) t)))
                     (lambda (net) (cl-pushnew net nets
                                               :test #'equal))
                     modules)
    (cl-sort nets #'< :key #'cadr)))

(defun pcb-apply-module (module attributes)
  "Add ATTRIBUTES to module such as (at x y rotation) etc.

The MODULE should be an s-expression with the contents of the module,
and the attributes a list with the properties, such as

  ((at x y 0.5) (fp_line ....))"
  (cl-concatenate 'list module attributes))

(defvar pcb-default-setup
  `((last_trace_width 0.2032)
    (trace_clearance 0.254)
    (zone_clearance 0.508)
    (zone_45_only no)
    (trace_min 0.2032)
    (segment_width 0.381)
    (edge_width 0.381)
    (via_size 0.889)
    (via_drill 0.635)
    (via_min_size 0.889)
    (via_min_drill 0.508)
    (uvia_size 0.508)
    (uvia_drill 0.127)
    (uvias_allowed no)
    (uvia_min_size 0.508)
    (uvia_min_drill 0.127)
    (pcb_text_width 0.3048)
    (pcb_text_size 1.524 2.032)
    (mod_edge_width 0.381)
    (mod_text_size 1.524 1.524)
    (mod_text_width 0.3048)
    (pad_size 1.524 1.524)
    (pad_drill 0.8128)
    (pad_to_mask_clearance 0.254)
    (aux_axis_origin 0 0)
    (visible_elements FFFFFF7F)
    (pcbplotparams
     (layerselection 3178497)
     (usegerberextensions true)
     (excludeedgelayer true)
     (linewidth 0.150000)
     (plotframeref false)
     (viasonmask false)
     (mode 1)
     (useauxorigin false)
     (hpglpennumber 1)
     (hpglpenspeed 20)
     (hpglpendiameter 15)
     (hpglpenoverlay 0)
     (psnegative false)
     (psa4output false)
     (plotreference true)
     (plotvalue true)
     (plotothertext true)
     (plotinvisibletext false)
     (padsonsilk false)
     (subtractmaskfromsilk false)
     (outputformat 1)
     (mirror false)
     (drillshape 1)
     (scaleselection 1)
     (outputdirectory ""))))

(cl-defun pcb-render (&key
                        file
                        (thickness 1.6002)
                        (setup pcb-default-setup)
                        drawings
                        modules
                        ;; nets 0 is missing
                        (nets (append '((net 0 ""))
                                      (pcb-extract-nets modules)))
                        zones
                        links
                        tracks)
  "Create the pcb header of the file"
  (let ((result `(kicad_pcb (version 3)
                            (host pcbnew "(2014-02-26 BZR 4721)-product")
                            (general (links ,(length links))
                                     (no_connects ,(length links))
                                     ;;(area 15.560886 13.205459 268.695594 107.670601)
                                     (thickness ,thickness)
                                     (drawings ,(length drawings))
                                     (tracks ,(length tracks))
                                     (zones ,(length zones))
                                     (modules ,(length modules))
                                     (nets ,(length nets)))
                            (page A4)
                            (title_block (date "16 oct 2014"))
                            (layers (15 Front signal)
                                    (0 Back signal)
                                    (16 B.Adhes user)
                                    (17 F.Adhes user)
                                    (18 B.Paste user)
                                    (19 F.Paste user)
                                    (20 B.SilkS user)
                                    (21 F.SilkS user)
                                    (22 B.Mask user)
                                    (23 F.Mask user)
                                    (24 Dwgs.User user)
                                    (25 Cmts.User user)
                                    (26 Eco1.User user)
                                    (27 Eco2.User user)
                                    (28 Edge.Cuts user))
                            (setup ,@setup)
                            ,@nets
                            (net_class
                             Default
                             "This is the default net class."
                             (clearance 0.254)
                             (trace_width 0.2032)
                             (via_dia 0.889)
                             (via_drill 0.635)
                             (uvia_dia 0.508)
                             (uvia_drill 0.127)
                             ,@(mapcar (lambda (net) `(add_net ,(nth 2 net)))
                                       nets))
                            ,@modules)))
    (if file
        (with-current-buffer (find-file-noselect file)
          (erase-buffer)
          (insert (format "%S" result))
          (lisp-data-mode)
          (pp-buffer)
          (save-buffer)
          file)
      result)))

(defun pcb-pad-valid-p (expr)
  (pcase expr
    ((and `(pad ,n . ,_)
          (guard (or (numberp n)
                     (and (stringp n)
                          (not (string= "" n))))))
     t)))

(defun pcb-module-fn (module)
  "Create a function from a module to render the s-expression.

MODULE must be the original s-expression.  This function
finds the pads of the module and returns a lambda which can be called
to set the position and the pad connections."
  (let (pad-indices)
    (let ((result (-tree-map-nodes  #'pcb-pad-valid-p
                                    (lambda (x)
                                      (pcase x
                                        (`(pad ,n . ,_)
                                          ;; turn n into a number
                                          (setf n (read (format "%s" n)))
                                          (cl-pushnew (cons n (gensym))
                                                      pad-indices
                                                      :key #'car)
                                          (let ((variable (alist-get n pad-indices)))
                                            `(,@x ,variable)))))
                                    module)))
      (setf pad-indices (cl-sort pad-indices #'< :key #'car))
      (let ((pad-vars (mapcar (lambda (ip) (intern (format "pad-%s" (car ip))))
                              pad-indices)))
        `(lambda (x y rotation ,@pad-vars &optional apply-args)
           (cl-sublis (list ,@(cl-mapcar (lambda (x y) `(cons ',x ,y))
                                         (mapcar #'cdr pad-indices)
                                         pad-vars))
                      (pcb-apply-module ',result
                                        `((at ,x ,y ,rotation)
                                          ,@apply-args))))))))

(defun pcb-module (name)
  "Function to call to get a module from a NAME."
  (pcb-module-fn (pcb-find-module name)))

(defun pcb-preview-sexp (sexp)
  "Preview a module given in SEXP as a s-sexpression."
  (let ((pcb (make-temp-file (concat "kicad-" (format "%s" (cadr sexp)) "-") nil ".kicad_pcb")))
    (pcb-render
     :file pcb
     :modules (list sexp))
    (shell-command (format "pcbnew %s" pcb))
    pcb))

(defun pcb-preview-module (name)
  "Preview a part search by NAME."
  (interactive (list (pcb-read-module-name)))
  (pcb-preview-sexp (pcb-find-module name)))

(defun pcb-read-module-name ()
  "Get a name from the available paths."
  (interactive)
  (let ((part (f-base
               (f-no-ext (completing-read "Part: " pcb-kicad-modules)))))
    (if (called-interactively-p)
        (insert part)
      part)))


(defun x-rotation-matrix (theta x y)
  (list (- (* (cos theta) x)
           (* (sin theta) y))
        (+ (* (sin theta) x)
           (* (cos theta) y))))



;; https://dev-docs.kicad.org/en/file-formats/sexpr-intro/index.html#_footprint_pad
;; thru_hole, smd, connect, or np_thru_hole
;; circle, rect, oval, trapezoid, roundrect, or custom
(cl-defun pcb-element-thru-holes (nx ny &key
                                          (x-sep 2.5)
                                          (y-sep 2.5)
                                          (type 'thru_hole)
                                          (shape 'circle)
                                          (size '(1.7526 1.7526))
                                          (drill 1.0922))
  "Make "
  (cl-assert (memq type '(thru_hole smd connect np_thru_hole)))
  (cl-assert (memq shape '(circle rect oval trapezoid roundrect custom)))
  `(footprint pcb-element-thru-holes
              ,@(let ((index 0))
                  (cl-loop for i below nx
                        appending
                        (cl-loop for j below ny
                              collect (progn
                                        (cl-incf index)
                                        `(pad ,index ,type ,shape
                                              (at ,(* x-sep i)
                                                  ,(* y-sep j))
                                              (size ,@size)
                                              (drill ,drill)
                                              (layers *.Cu *.SilkS *.Mask))))))))

(cl-defun pcb-element-rect (start end &key layer solid)
  `(gr_rect (start ,@start)
            (end ,@end)
            (layer ,layer)
            ,(if solid
                 '(fill solid)
               '(fill none))))

(cl-defun pcb-element-circle (center radius &key
                                              (end (list (car center)
                                                         (+ (cadr center) radius)))
                                              layer
                                              solid)
  `(gr_circle (center ,@center)
              (end ,@end)
              (layer ,layer)
              ,(if solid
                   '(fill solid)
                 '(fill none))))

(cl-defun pcb-element-line (start end &key layer)
  `(gr_line (start ,@start) (end ,@end) (layer ,layer)))


(defun pcb-element-points (&rest points)
  `(pts ,@(mapcar (lambda (xy) (cons 'xy xy))
                  points)))


(provide 'pcb)
;;; pcb.el ends here

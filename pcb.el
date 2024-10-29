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

(require 'cl-lib)
(require 'dash)

(defvar pcb-kicad-modules nil)
(defun pcb-refresh-kicad-cache ()
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
  (pcase module
    (`(module ,name . ,rest)
      `(module ,name
               ,@attributes
               ,@rest))))

(pcb-apply-module (pcb-find-module "MX-6U")
                  `((at penis ass rotation)))


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
                                     (area 15.560886 13.205459 268.695594 107.670601)
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
                            (gr_text Khonsu (at 131.5 84) (layer F.SilkS)
                                     (effects (font (size 4 3) (thickness 0.3048))))
                            (gr_text "Alejandro Gallo" (at 145.25 107.5) (layer F.SilkS)
                                     (effects (font (size 2.032 1.524) (thickness 0.3048))))
                            (gr_text "GPLv3" (at 68.5 98.5 350) (layer F.SilkS)
                                     (effects (font (size 1.5 1.1) (thickness 0.2))))
                            (gr_text https://atreus.technomancy.us (at 63 100.5 350) (layer F.SilkS)
                                     (effects (font (size 2.032 1.524) (thickness 0.3048))))
                            (gr_text "rev 3, Apr 2019" (at 200.5 101.5 10) (layer F.SilkS)
                                     (effects (font (size 2.032 1.524) (thickness 0.3048))))
                            (gr_text "© 2014-2019" (at 117 107.75) (layer F.SilkS)
                                     (effects (font (size 2.032 1.524) (thickness 0.3048))))
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

(defun pcb-module-fn (module)
  "Create a function from a module to render the s-expression.

MODULE must be the original s-expression.  This function
finds the pads of the module and returns a lambda which can be called
to set the position and the pad connections."
  (let (pad-indices)
    (let ((result (-tree-map-nodes (lambda (x)
                                     (pcase x
                                       ((and `(pad ,n . ,_)
                                             (guard (numberp n)))
                                        (cl-pushnew (cons n (gensym))
                                                    pad-indices
                                                    :key #'car)
                                        t)))
                                   (lambda (x)
                                     (pcase x
                                       ((and `(pad ,n . ,_)
                                             (guard (numberp n)))
                                        (let ((variable (alist-get n pad-indices)))
                                          `(,@x ,variable)))))
                                   module)))
      (setf pad-indices (cl-sort pad-indices #'< :key #'car))
      (let ((pad-vars (mapcar (lambda (ip) (intern (format "pad-%s" (car ip))))
                              pad-indices)))
        `(lambda (x y rotation ,@pad-vars)
           (cl-sublis (list ,@(cl-mapcar (lambda (x y) `(cons ',x ,y))
                                         (mapcar #'cdr pad-indices)
                                         pad-vars))
                      (pcb-apply-module ',result
                                        `((at ,x ,y ,rotation)))))))))

(defun pcb-module (name)
  "Function to call to get a module from a name."
  (pcb-module-fn (pcb-find-module name)))

(defvar x-mx (pcb-find-module "MX-6U"))
(funcall (pcb-module-pads x-mx)
         5 9 98 9 8 7 9)
(pcb-module "MX-6U")



(defun pcb-switch-module (x y rotation label net-pos net-neg)
  `(module MX_FLIP
           (layer Front) (tedit 4FD81CDD) (tstamp 543EF801)
           (at ,x ,y ,rotation)
           (path /543DB910) ; TODO: this is not documented; no idea what it does
           (fp_text reference ,label (at 0 3.302 ,rotation) (layer F.SilkS)
                    (effects (font (size 1.524 1.778) (thickness 0.254))))
           (fp_line (start -6.35 -6.35) (end 6.35 -6.35)
                    (layer F.SilkS) (width 0.381))
           (fp_line (start 6.35 -6.35) (end 6.35 6.35)
                    (layer F.SilkS) (width 0.381))
           (fp_line (start 6.35 6.35) (end -6.35 6.35)
                    (layer F.SilkS) (width 0.381))
           (fp_line (start -6.35 6.35) (end -6.35 -6.35)
                    (layer F.SilkS) (width 0.381))
           (pad 0 np_thru_hole circle (at 0 0) (size 3.9878 3.9878)
                (drill 3.9878))         ; switch hole, no copper
           (pad 0 np_thru_hole circle (at -5.08 0) (size 1.7018 1.7018)
                (drill 1.7018))         ; board-mount hole, no copper
           (pad 0 np_thru_hole circle (at 5.08 0) (size 1.7018 1.7018)
                (drill 1.7018))         ; board-mount hole, no copper
           (pad 1 thru_hole circle (at 2.54 -5.08) (size 2.286 2.286) (drill 1.4986)
                (layers *.Cu *.SilkS *.Mask) ,net-pos)
           (pad 1 thru_hole circle (at 3.81 -2.54) (size 2.286 2.286) (drill 1.4986)
                (layers *.Cu *.SilkS *.Mask) ,net-pos)
           (pad 2 thru_hole circle (at -2.54 -5.08) (size 2.286 2.286) (drill 1.4986)
                (layers *.Cu *.SilkS *.Mask) ,net-neg)
           (pad 2 thru_hole circle (at -3.81 -2.54) (size 2.286 2.286) (drill 1.4986)
                (layers *.Cu *.SilkS *.Mask) ,net-neg)))





(let ((mx (pcb-module "MX-6U")))
  (pcb-render
   :file "test.kicad_pcb"
   :modules (list (funcall mx 0 0 0
                           '(net 1 COL-0)
                           '(net 1 COL-0)
                           '(net 2 COL-1)
                           '(net 2 COL-1)))))


(provide 'pcb)
;;; pcb.el ends here

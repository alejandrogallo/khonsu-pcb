(pcb-render :modules (cl-loop for mod in (mapcar #'pcb-find-module
                                                 '(Diode-hybrid-3pad
                                                   Diode-Hybrid-Back
                                                   Diode_Long
                                                   MD_Diode
                                                   Diode
                                                   Diode-dual))
                           for i from 0
                           collect (pcb-apply-module mod `((at ,(* i 20) 0 90))))
            :file "diodes.kicad_pcb")

(pcb-render :modules (cl-loop for mod in (mapcar (-compose #'pcb-find-module
                                                           #'f-base)
                                                 (cl-remove-if-not (lambda (x)
                                                                     (string-match-p "MXOnly" x))
                                                                   pcb-kicad-modules))
                           for i from 0
                           for x = (% i 20)
                           for y = (/ i 20)
                           appending (let ((position `(at ,(* x 25) ,(* y -40) 90))
                                           (text-position `(at ,(* x 25) ,(* y -40) 90)))
                                       (list
                                        `(gr_text ,(format "%S" (cadr mod))
                                                  (layer F.SilkS)
                                                  ,text-position)
                                        (pcb-apply-module mod `(,position)))))
            :file "all.kicad_pcb")

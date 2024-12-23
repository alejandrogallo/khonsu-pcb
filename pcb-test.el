(require 'pcb)
(require 'cl-lib)

(ert-deftest pcb-extract-nets ()
  (should
   (cl-tree-equal (with-current-buffer (find-file-noselect "examples/original-atreus-pcb.lisp")
                    (pcb-extract-nets (read (buffer-string))))
                  '((net 0 "") (net 1 N-row-0) (net 2 N-row-1) (net 3 N-row-2)
                    (net 4 N-row-3) (net 5 N-col-0) (net 6 N-col-1) (net 7 N-col-2)
                    (net 8 N-col-3) (net 9 N-col-4) (net 10 N-col-5) (net 11 N-col-6)
                    (net 12 N-col-7) (net 13 N-col-8) (net 14 N-col-9) (net 15 N-col-10)
                    (net 16 N-diode-0) (net 17 N-diode-1) (net 18 N-diode-2)
                    (net 19 N-diode-3) (net 20 N-diode-4) (net 21 N-diode-5)
                    (net 22 N-diode-6) (net 23 N-diode-7) (net 24 N-diode-8)
                    (net 25 N-diode-9) (net 26 N-diode-10) (net 27 N-diode-11)
                    (net 28 N-diode-12) (net 29 N-diode-13) (net 30 N-diode-14)
                    (net 31 N-diode-15) (net 32 N-diode-16) (net 33 N-diode-17)
                    (net 34 N-diode-18) (net 35 N-diode-19) (net 36 N-diode-20)
                    (net 37 N-diode-21) (net 38 N-diode-22) (net 39 N-diode-23)
                    (net 40 N-diode-24) (net 41 N-diode-25) (net 42 N-diode-26)
                    (net 43 N-diode-27) (net 44 N-diode-28) (net 45 N-diode-29)
                    (net 46 N-diode-30) (net 47 N-diode-31) (net 48 N-diode-32)
                    (net 49 N-diode-33) (net 50 N-diode-34) (net 51 N-diode-35)
                    (net 52 N-diode-36) (net 53 N-diode-37) (net 54 N-diode-38)
                    (net 55 N-diode-39) (net 56 N-diode-40) (net 57 N-diode-41)))))

(ert-deftest pcb-module ()
  (should (cl-tree-equal (funcall (pcb-module "MX-6U")
                                  1 2 3
                                  '(net 0)
                                  '(net this-test)
                                  '(net other-test)
                                  '(net 1))
                         '(module MX-6U-Pretty (at 1 2 3) (layer F.Cu) (tedit 5A9F4279)
                           (fp_text reference REF** (at 9.525 3.175) (layer Dwgs.User)
                            (effects (font (size 1 1) (thickness 0.15))))
                           (fp_text value 6U (at 9.525 -7.9375) (layer Dwgs.User)
                            (effects (font (size 1 1) (thickness 0.15))))
                           (fp_line (start 14.525 -7) (end 16.525 -7) (layer Dwgs.User)
                            (width 0.15))
                           (fp_line (start 16.525 -7) (end 16.525 -5) (layer Dwgs.User)
                            (width 0.15))
                           (fp_line (start 14.525 7) (end 16.525 7) (layer Dwgs.User)
                            (width 0.15))
                           (fp_line (start 16.525 7) (end 16.525 5) (layer Dwgs.User)
                            (width 0.15))
                           (fp_line (start 2.525 5) (end 2.525 7) (layer Dwgs.User)
                            (width 0.15))
                           (fp_line (start 2.525 7) (end 4.525 7) (layer Dwgs.User)
                            (width 0.15))
                           (fp_line (start 4.525 -7) (end 2.525 -7) (layer Dwgs.User)
                            (width 0.15))
                           (fp_line (start 2.525 -7) (end 2.525 -5) (layer Dwgs.User)
                            (width 0.15))
                           (fp_line (start -57.15 -9.525) (end 57.15 -9.525)
                            (layer Dwgs.User) (width 0.15))
                           (fp_line (start 57.15 -9.525) (end 57.15 9.525)
                            (layer Dwgs.User) (width 0.15))
                           (fp_line (start -57.15 9.525) (end 57.15 9.525)
                            (layer Dwgs.User) (width 0.15))
                           (fp_line (start -57.15 9.525) (end -57.15 -9.525)
                            (layer Dwgs.User) (width 0.15))
                           (pad 2 thru_hole oval (at 12.025 -4.5 86.0548)
                            (size 2.831378 2.25) (drill 1.47 (offset 0.290689 0))
                            (layers *.Cu B.Mask) (net this-test))
                           (pad 2 thru_hole circle (at 12.065 -5.08) (size 2.25 2.25)
                            (drill 1.47) (layers *.Cu B.Mask) (net this-test))
                           (pad 1 thru_hole oval (at 5.715 -2.54 48.0996)
                            (size 4.211556 2.25) (drill 1.47 (offset 0.980778 0))
                            (layers *.Cu B.Mask) (net 0))
                           (pad "" np_thru_hole circle (at 9.525 0) (size 3.9878 3.9878)
                            (drill 3.9878) (layers *.Cu *.Mask))
                           (pad 1 thru_hole circle (at 7.025 -4) (size 2.25 2.25)
                            (drill 1.47) (layers *.Cu B.Mask) (net 0))
                           (pad 3 thru_hole circle (at 8.255 5.08) (size 1.905 1.905)
                            (drill 1.04) (layers *.Cu B.Mask) (net other-test))
                           (pad 4 thru_hole rect (at 10.795 5.08) (size 1.905 1.905)
                            (drill 1.04) (layers *.Cu B.Mask) (net 1))
                           (pad "" np_thru_hole circle (at 4.445 0 48.0996)
                            (size 1.75 1.75) (drill 1.75) (layers *.Cu *.Mask))
                           (pad "" np_thru_hole circle (at 14.605 0 48.0996)
                            (size 1.75 1.75) (drill 1.75) (layers *.Cu *.Mask))
                           (pad "" np_thru_hole circle (at -47.625 -6.985)
                            (size 3.048 3.048) (drill 3.048) (layers *.Cu *.Mask))
                           (pad "" np_thru_hole circle (at 47.625 -6.985)
                            (size 3.048 3.048) (drill 3.048) (layers *.Cu *.Mask))
                           (pad "" np_thru_hole circle (at -47.625 8.255)
                            (size 3.9878 3.9878) (drill 3.9878) (layers *.Cu *.Mask))
                           (pad "" np_thru_hole circle (at 47.625 8.255)
                            (size 3.9878 3.9878) (drill 3.9878) (layers *.Cu *.Mask))))))

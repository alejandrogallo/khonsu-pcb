(require 'pcb)


(defun pcb-svg-parse-transform (transform)
  (read (format "(%s)" (replace-regexp-in-string "," " " transform))))

(defun pcb-svg-apply-transform (transform point)
  (cl-destructuring-bind (x y) point
    (pcase transform
      (`(translate (,tx ,ty))
        (list (+ x tx) (+ y ty)))
      ;; x' = a*x + c*y + e
      ;; y' = b*x + d*y + f
      (`(matrix (,a ,b ,c ,d ,e ,f))
        (list (+ (* a x) (* c y) e)
              (+ (* b x) (* d y) f)))
      (`(rotate (,angle))
        (pcb-svg-apply-transform `(matrix (,(cos (* angle (/ pi 180)))
                                            ,(sin (* angle (/ pi 180)))
                                            ,(- (sin (* angle (/ pi 180))))
                                            ,(cos (* angle (/ pi 180)))
                                            0
                                            0))
                                 point))
      (`nil point)
      (_ (error "Transform %s not recognized" transform)))))

(defun pcb-svg-apply-transforms (transforms point)
  (if transforms
      (progn (cl-loop for tr in (reverse transforms)
                   with initial = point
                   do (setf initial (pcb-svg-apply-transform tr initial))
                   finally return initial))
    point))


(pcb-svg-apply-transform (pcb-svg-parse-transform "rotate(90)")
                         '(0 1))
(pcb-svg-apply-transform (pcb-svg-parse-transform "translate(-17.7656,-26.763426)")
                         '(0 0))
(pcb-svg-apply-transform (pcb-svg-parse-transform "matrix(0.88477178,0,0,0.88477178,5.7447324,24.139443)")
                         '(2 3))
(pcb-svg-apply-transform (pcb-svg-parse-transform "matrix(0.443,0.896,-0.896,0.443,589.739,-373.223)")
                         (list 27 -9))
(pcb-svg-apply-transforms (list (pcb-svg-parse-transform "translate(2, 0)")
                                (pcb-svg-parse-transform "matrix(0.443,0.896,-0.896,0.443,589.739,-373.223)"))
                          (list 27 -9))

(progn
  (defun pcb-xml-to-modules (xml-tree transforms)
    (let (modules)
      (-tree-map-nodes
       (lambda (x) (and (listp x)
                        (memq (car x) '(rect path circle ellipse g))))
       (lambda (x)
         (pcase x
           (`(g ,attrs . ,elements)
             (let ((transform (when-let ((it (alist-get 'transform attrs nil)))
                                (pcb-svg-parse-transform it))))
               (message "transforms: %s" transforms)
               (setf modules
                     (cl-concatenate 'list modules
                                     (pcb-xml-to-modules elements (cl-concatenate 'list transforms (list transform)))))))
           (`(rect ,attrs)
             (let ((x (read (alist-get 'x attrs)))
                   (y (read (alist-get 'y attrs)))
                   (transform (when-let ((it (alist-get 'transform attrs nil)))
                                (pcb-svg-parse-transform it)))
                   (width (read (alist-get 'width attrs)))
                   (height (read (alist-get 'height attrs)))
                   (style (alist-get 'style attrs)))
               (push (pcb-element-rect (pcb-svg-apply-transforms `(,@transforms ,transform)
                                                                 (list x y))
                                       (pcb-svg-apply-transforms `(,@transforms ,transform)
                                                                 (list (+ x width) (+ y height)))
                                       :layer 'F.Cu
                                       :solid (null (string-match-p
                                                     "fill:none" style)))
                     modules)))
           (`(,(or 'ellipse 'circle) ,attrs)
             (let* ((cx (read (alist-get 'cx attrs)))
                    (transform (when-let ((it (alist-get 'transform attrs nil)))
                                 (pcb-svg-parse-transform it)))
                    (cy (read (alist-get 'cy attrs)))
                    (r (alist-get 'r attrs nil))
                    (rx (read (alist-get 'rx attrs r)))
                    (ry (read (alist-get 'ry attrs r)))
                    (style (alist-get 'style attrs)))
               (push (let* ((center (pcb-svg-apply-transforms `(,@transforms ,transform) (list cx cy)))
                            (radius-end-point (list (+ rx (car center))
                                                    (cadr center))))
                       (pcb-element-circle center
                                           nil
                                           :end radius-end-point
                                           :layer 'F.Cu
                                           :solid (null (string-match-p
                                                         "fill:none" style))))
                     modules)))
           (`(path ,attrs)
             (let* ((d (alist-get 'd attrs))
                    (transform (when-let ((it (alist-get 'transform attrs nil)))
                                 (pcb-svg-parse-transform it)))
                    (style (alist-get 'style attrs))
                    (paths (pcb-svg-path-to-kicad d `(,@transforms
                                                      ,@(when transform
                                                          (list transform))))))
               (setf modules (cl-concatenate 'list
                                             (cl-loop for path in paths
                                                   collect
                                                   (pcb-apply-module path
                                                                     `((fill none)
                                                                       (width 54)
                                                                       (stroke
                                                                        (width 0.381)))))
                                             modules))))))
       xml-tree)
      modules))

  (defun pcb-svg-to-modules (filepath)
    (pcb-xml-to-modules
     (with-current-buffer (find-file-noselect filepath)
       (libxml-parse-xml-region))
     nil))

  (pcb-render  :file "from-svg.kicad_pcb"
               :modules
               (pcb-svg-to-modules "~/drawing.svg")))

(pcb-svg-path-to-kicad "m 25.848306,125.77615 4.705681,-12.23554 10.446265,11.46724 2.928613,-10.56634 7.328204,11.8747 6.751441,-10.93884 21.591587,3.66575 -8.548422,11.24034 -14.779048,1.13318 -14.662519,-4.15918 -7.421471,4.38734 14.830612,1.53687")

(defun pcb-parse-path-string (str)
  ;; https://www.w3.org/TR/SVG/paths.html
  (cl-labels ((sum (p1 p2) (-zip-with #'+ p1 p2))
              (sum-if-relative (relative p1 p2) (if relative
                                                    (sum p1 p2)
                                                  p1)))
    (cl-macrolet ((lines-from-points (points)
                    `(cl-loop for point in ,points
                           for newpoint = (sum-if-relative
                                           relative
                                           point
                                           current)
                           collect (prog1`(line :from ,current
                                                :to ,newpoint)
                                         (setf current newpoint)))))
      (let (cmds
            is-polygon)
        (with-temp-buffer
          (insert str)
          (beginning-of-buffer)
          (setf is-polygon (string-match-p ".*[zZ] *" (buffer-string)))
          (while (re-search-forward (rx (group (group (any "mclhvMCLHV"))
                                               (group (0+ (any ". e0-9,-")))))
                                    nil t)
            (let ((command (string-trim (match-string 2))))
              (push (list (downcase command)
                          (string= command (downcase command))
                          (mapcar (lambda (x)
                                    (mapcar #'read (string-split x ",")))
                                  (string-split (replace-regexp-in-string
                                                 "  +" " "
                                                 (string-trim (match-string 3)))
                                                " ")))
                    cmds))))
        (let ((current '(0 0))
              elements)
          (setf elements
                (cl-loop for command in (reverse cmds)
                      appending (pcase command
                                  (`("m" ,relative ,args)
                                    (setf current (sum-if-relative relative
                                                                   (car args)
                                                                   current))
                                    (lines-from-points (cdr args)))
                                  (`("l" ,relative ,args) (lines-from-points args))
                                  (`("h" ,relative ,args)
                                    (let ((last current)
                                          new-points)
                                      (setf new-points (mapcar (lambda (pt)
                                                                 (let ((new
                                                                        (list (car pt)
                                                                              (if relative 0
                                                                                (cadr last)))))
                                                                   (let ((target (sum-if-relative relative new last)))
                                                                     (setf last target)
                                                                     target)))
                                                               args))
                                      (let ((relative nil))
                                        (lines-from-points new-points))))
                                  (`("v" ,relative ,args)
                                    (let ((last current)
                                          new-points)
                                      (setf new-points (mapcar (lambda (pt)
                                                                 (let ((new
                                                                        (list (if relative 0
                                                                                (car last))
                                                                              (car pt))))
                                                                   (let ((target (sum-if-relative relative new last)))
                                                                     (setf last target)
                                                                     target)))
                                                               args))
                                      (let ((relative nil))
                                        (lines-from-points new-points))))
                                  (`("c" ,relative ,args)
                                    (cl-loop for (p1 p2 p3) on args by #'cdddr
                                          for target = (sum-if-relative relative p3 current)
                                          collect (prog1 `(curve :ctrl1 ,p1
                                                                 :ctrl2 ,p2
                                                                 :from ,current
                                                                 :to ,target)
                                                    (setf current target)))))))
          (when is-polygon
            (setf elements
                  (list (list 'polygon
                              (mapcar (lambda (e) (list :from (getf (cdr e) :from)
                                                        :to (getf (cdr e) :to)))
                                      elements)))))
          elements)))))
(pcb-parse-path-string "m 1,2 v 5")


(defun pcb-svg-path-to-kicad (svg-string &optional transforms)
  (let ((elements (pcb-parse-path-string svg-string)))
    (cl-loop for el in elements
          collect
          (pcase el
            (`(line :from ,from :to ,to)
              (pcb-element-line (pcb-svg-apply-transforms transforms from)
                                (pcb-svg-apply-transforms transforms to)))
            (`(curve ,p1 ,p2 :from ,from :to ,to)
              (warn "Bezier is not supported in KICAD"))
            (`(polygon ,points)
              `(gr_poly ,(apply #'pcb-element-points (append (mapcar (lambda (pt)
                                                                       (pcb-svg-apply-transforms transforms
                                                                                                 (getf pt :from)))
                                                                     points)
                                                             (list (pcb-svg-apply-transforms transforms
                                                                                             (getf (car (last points)) :to)))))
                        ))))))




(pcb-parse-path-string "m50.986408,90.901 c 0.190304,-0.292127      -0.02508,-0.754804 0.152463,-1.067248 0.793126,-1.395827 1.379694,-2.126236 2.715078,-3.125374 0.687554,-0.514432 1.447691,-0.92623 2.199767,-1.340668 0.472926,-0.260609 0.721976,-0.319786 1.210926,-0.276317 0.129061,0.01148 0.258323,0.03502 0.381161,0.07623 0.523865,0.175779 1.195594,0.524833 1.782184,0.5886 0.307422,0.03342 0.618468,0 0.9277,0 2.226329,0 1.771407,0.131615 4.75315,-1.009266 0.299632,-0.114647 0.522504,-0.37324 0.770416,-0.576868 0.114075,-0.0937 0.248134,-0.180616 0.313156,-0.313145 0.10033,-0.20451 0.103375,-0.443889 0.165732,-0.662983 0.04533,-0.159271 0.09908,-0.316195 0.157099,-0.471297 0.0199,-0.05322 0.09627,-0.0993 0.07623,-0.152463 -0.01572,-0.0417 -0.08913,0 -0.133694,0")


(pcb-svg-path-to-kicad            "m 58.800916,88.000642 c 3.209647,0.595644 2.384851,0.627386 5.405541,0.545111 1.029089,-0.02803 2.059733,-0.05513 3.084708,-0.151247 3.072503,-0.288131 4.763767,-0.72776 7.589517,-0.756557 0.992143,-0.01011 1.507374,0.07046 2.388103,0.397785 0.618858,0.229994 0.593635,0.381532 1.229503,0.71592 0.379732,0.199692 0.757676,0.420087 1.17162,0.53289 0.526319,0.143425 1.076571,0.187483 1.620573,0.228002 1.086688,0.08094 1.841582,0.102891 2.780982,-0.557729 0.606026,-0.42618 0.995069,-1.517028 1.104294,-2.164461 0.07041,-0.417361 0,-0.846516 0,-1.269772 0,-0.221316 0.03911,-0.446114 0,-0.663946 -0.07623,-0.203284 -0.152466,-0.406569 -0.228698,-0.609854 -0.0254,0 -0.06936,0.02447 -0.07623,0 -0.02447,-0.0871 0,-0.180932 0,-0.271396 M 50.986408,90.901 c 0.190304,-0.292127 -0.02508,-0.754804 0.152463,-1.067248 0.793126,-1.395827 1.379694,-2.126236 2.715078,-3.125374 0.687554,-0.514432 1.447691,-0.92623 2.199767,-1.340668 0.472926,-0.260609 0.721976,-0.319786 1.210926,-0.276317 0.129061,0.01148 0.258323,0.03502 0.381161,0.07623 0.523865,0.175779 1.195594,0.524833 1.782184,0.5886 0.307422,0.03342 0.618468,0 0.9277,0 2.226329,0 1.771407,0.131615 4.75315,-1.009266 0.299632,-0.114647 0.522504,-0.37324 0.770416,-0.576868 0.114075,-0.0937 0.248134,-0.180616 0.313156,-0.313145 0.10033,-0.20451 0.103375,-0.443889 0.165732,-0.662983 0.04533,-0.159271 0.09908,-0.316195 0.157099,-0.471297 0.0199,-0.05322 0.09627,-0.0993 0.07623,-0.152463 -0.01572,-0.0417 -0.08913,0 -0.133694,0")

(pcb-parse-path-string            "m 58.800916,88.000642 c 3.209647,0.595644 2.384851,0.627386 5.405541,0.545111 1.029089,-0.02803 2.059733,-0.05513 3.084708,-0.151247 3.072503,-0.288131 4.763767,-0.72776 7.589517,-0.756557 0.992143,-0.01011 1.507374,0.07046 2.388103,0.397785 0.618858,0.229994 0.593635,0.381532 1.229503,0.71592 0.379732,0.199692 0.757676,0.420087 1.17162,0.53289 0.526319,0.143425 1.076571,0.187483 1.620573,0.228002 1.086688,0.08094 1.841582,0.102891 2.780982,-0.557729 0.606026,-0.42618 0.995069,-1.517028 1.104294,-2.164461 0.07041,-0.417361 0,-0.846516 0,-1.269772 0,-0.221316 0.03911,-0.446114 0,-0.663946 -0.07623,-0.203284 -0.152466,-0.406569 -0.228698,-0.609854 -0.0254,0 -0.06936,0.02447 -0.07623,0 -0.02447,-0.0871 0,-0.180932 0,-0.271396 M 50.986408,90.901 c 0.190304,-0.292127 -0.02508,-0.754804 0.152463,-1.067248 0.793126,-1.395827 1.379694,-2.126236 2.715078,-3.125374 0.687554,-0.514432 1.447691,-0.92623 2.199767,-1.340668 0.472926,-0.260609 0.721976,-0.319786 1.210926,-0.276317 0.129061,0.01148 0.258323,0.03502 0.381161,0.07623 0.523865,0.175779 1.195594,0.524833 1.782184,0.5886 0.307422,0.03342 0.618468,0 0.9277,0 2.226329,0 1.771407,0.131615 4.75315,-1.009266 0.299632,-0.114647 0.522504,-0.37324 0.770416,-0.576868 0.114075,-0.0937 0.248134,-0.180616 0.313156,-0.313145 0.10033,-0.20451 0.103375,-0.443889 0.165732,-0.662983 0.04533,-0.159271 0.09908,-0.316195 0.157099,-0.471297 0.0199,-0.05322 0.09627,-0.0993 0.07623,-0.152463 -0.01572,-0.0417 -0.08913,0 -0.133694,0")

(pcb-parse-path-string "m 25.848306,125.77615 4.705681,-12.23554 10.446265,11.46724 2.928613,-10.56634 7.328204,11.8747 6.751441,-10.93884 21.591587,3.66575 -8.548422,11.24034 -14.779048,1.13318 -14.662519,-4.15918 -7.421471,4.38734 14.830612,1.53687")



(with-current-buffer (find-file-noselect "~/drawing.svg")
  (libxml-parse-xml-region))

(let ((str "m 73.030403,31.120702 -1.809665,1.998127 -2.73693,2.912899 -4.959183,5.390418 -3.881101,3.881102 -4.920496,5.132182 -1.598306,1.758664 -0.906515,1.122137 v 0.107808 l 0.207684,2.091589 m 4.322279,-22.223887 11.241881,6.909092 5.152022,2.765742 5.174803,3.234251 3.589417,2.108909 1.414735,0.794179 0.110929,0.218736 h -0.09448 V 49.2518 L 82.82045,49.145334 m -12.477505,-8.945836 0.946579,-2.482959 0.554112,-0.889402 0.64685,-0.539042 v 0.196041 0.64685 l 0.127955,1.752574 0.127852,1.150771 v 1.465763 0.698392 l 0.965182,0.749572 0.10781,-0.215616 0.221157,-0.782348 1.035984,-3.651676 0.59971,-2.132871 0.107809,-0.862467 v -0.210524 1.457243 l 0.110072,1.967973 0.248833,2.848832 0.107807,0.431233 v 0.107809 l 0.0992,-0.0992 0.09919,-0.836634 0.113665,-0.795777 0.563653,-1.912957 0.112281,-0.224579 0.10781,-0.323425 0.215616,-0.64685 0.215617,-0.970276 0.107807,-0.539042 v -0.431233 -0.107809 l -0.09577,-0.05621 M 48.911609,43.890677 48.705,44.392305 h -0.107809 -0.107808 l -0.107808,-0.215617 -0.692003,-1.184076 -0.377619,-1.003287 v -0.556098 l 0.622175,-2.259676 1.353276,-1.364762 0.541121,-0.325508 0.673724,-0.449144 0.663366,-0.331696 0.799431,-0.344406 0.220064,-0.332335 0.431234,-0.431233 0.215617,-0.431234 0.431234,-1.185892 0.107807,-0.431233 v -0.322086 0.07829 0.522043 0.929978 l -0.529948,2.118118 -1.667793,4.409122 -0.411308,1.224636 -0.842332,1.676998 -0.901645,1.135791 -0.31982,0.215617 -0.09285,-0.322044 -0.107808,-0.754658 v -0.431234 l 0.414106,-0.135998 0.01713,-0.726469 0.107809,-0.107808 0.107808,-0.107808 1.734082,-0.756942 0.55944,-0.222421 0.655693,-0.217821 1.405353,-1.05558 0.725363,-0.725357 0.49077,-0.861148 0.974342,-1.834871 0.493982,-0.867711 0.239146,-0.956612 0.325649,-1.188116 v -0.323425 -0.646851 0.09898 l -0.206785,0.314595 -0.323426,0.431233 -0.543687,1.196953 -0.586222,2.230365 -0.1144,0.79812 -0.113626,0.794347 v 1.631769 l -0.107809,0.539042 0.107809,1.832743 0.107807,0.323425 v 0.215617 l 0.10781,0.107808 0.107807,0.215617 0.323427,0.539041 h 0.106479 0.0996 0.09958 0.09976 v -0.0998 l 0.536395,-0.645534 0.10781,-0.323425 h 0.09717 l 0.07313,-0.10646 0.323424,-0.215616 0.215617,-0.646851 0.107807,-0.431233 v -0.106466 l -0.215617,-0.322076 v -0.106466 h -0.09973 -0.09973 v 0.08288 l 0.10781,0.525112 v 0.323425 l 0.215617,0.64685 0.323424,0.970275 0.107809,0.107809 0.215617,0.215616 1.315453,0.546162 h 0.215617 l 0.10781,-0.215617 0.678288,-1.365311 0.640599,-2.29079 0.238818,-0.845194 0.349668,-1.612145 0.257953,-1.16075 0.113414,-0.851073 0.114509,-0.799446 v -0.64685 -0.431234 -0.215617 l -0.10781,-0.107808 -0.299736,0.199515 v 0.539042 l -0.10781,0.778071 v 0.956588 1.114627 l 0.142801,2.028306 v 2.623651 l 0.221263,1.24943 0.431234,0.754658 h 0.106479 0.09974 l 0.996136,-1.239132 0.114913,-0.459659 1.60569,-3.748266 0.385435,-2.819761 -0.107807,-1.905761 -0.215617,-0.646851 h -0.101637 -0.08526 l -0.214273,0.644163 -0.221895,0.551587 -0.756896,2.15547 -0.400108,2.548831 v 1.311516 0.986765 l 0.323424,1.308223 0.431233,0.754659 0.539041,0.862467 h 0.09932 l 0.422744,-0.323425 0.431233,-0.970276 v -0.539042 l 0.10781,-0.539041 v -0.323425 -0.431234 0.09804 l 0.107807,1.274158 0.215617,0.970275 v 0.107808 l 0.215617,0.323425 h 0.862468 L 67.30994,44.521357 67.525557,43.766698 67.417749,42.688615 67.30994,42.365189 67.094323,42.149573 66.878706,41.933956 66.77224,41.827497 66.664433,41.61188 H 66.5636 v 0.248563 0.644205 0.215617 0.215617 l 0.42177,0.215616 0.413798,-0.214294 v -0.106492 l 0.106452,-0.315424 v -0.352713 l 0.07843,-0.07843 z"))
  (pcb-parse-path-string str)
  (pcb-svg-path-to-kicad str))




(pcb-parse-path-string "m 103.15794,155.41515 v 27.48339 h 23.63491 V 152.66018 L 107.3154,141.41487 Z")

(pcb-parse-path-string "m 27.947872,173.14618 20.862494,-7.43307 -2.550639,-4.63527 -4.023554,16.07746 10.883861,0.12423 Z")
(pcb-svg-path-to-kicad "m 27.947872,173.14618 20.862494,-7.43307 -2.550639,-4.63527 -4.023554,16.07746 10.883861,0.12423 Z")


(pcb-parse-path-string "M 8,16 7.4427788,15.542288 6.8855575,15.084575 6.3283362,14.626863 5.771115,14.16915 5.2138937,13.711438 4.6566725,13.253725 4.0994513,12.796013 3.54223,12.3383 2.9709054,11.813044 2.4660198,11.233084 2.0305747,10.604768 1.6675713,9.934445 1.3800108,9.2284631 1.1708945,8.4931709 1.0432238,7.7349171 1,6.96005 1.1431201,5.557357 1.5531973,4.2508842 2.2013075,3.0686196 3.0585263,2.0385512 4.0959292,1.1886671 5.284592,0.54695516 6.5955904,0.14140346 8,0 l 1.404401,0.14140346 1.310994,0.4055517 1.188663,0.64171194 1.037405,0.8498841 0.857222,1.0300684 0.648113,1.1822646 0.06487,0.1570905 0.06109,0.1589765 0.05726,0.1608079 0.05337,0.1625847 0.04943,0.1643068 0.04543,0.1659743 0.04137,0.167587 0.03725,0.1691451 0.03308,0.1706486 0.02886,0.1720973 0.02457,0.1734914 0.02023,0.1748309 0.01583,0.1761157 0.01138,0.1773458 0.0069,0.1785213 L 15,6.96005 l -6.82e-4,0.09755 -0.002,0.097377 -0.0034,0.097192 -0.0047,0.096994 -0.0061,0.096784 -0.0074,0.096562 -0.0088,0.096327 -0.01008,0.09608 -0.0114,0.09582 -0.01272,0.095549 -0.01402,0.095264 -0.01532,0.094967 -0.01662,0.094658 -0.01791,0.094336 -0.0192,0.094003 -0.02047,0.093656 -0.02175,0.093297 -0.02301,0.092926 -0.02428,0.092543 -0.02553,0.092147 -0.02678,0.091738 -0.02802,0.091318 -0.02926,0.090884 -0.03049,0.090439 -0.03172,0.089981 -0.03294,0.08951 -0.03415,0.089028 -0.03536,0.088532 -0.03656,0.088025 -0.03776,0.087505 -0.03895,0.086973 -0.04013,0.086428 -0.04131,0.085871 -0.04248,0.0853 -0.04365,0.08472 -0.04481,0.08413 -0.04597,0.08352 -0.04712,0.0829 -0.04826,0.08227 -0.0494,0.08162 -0.05053,0.08097 -0.05166,0.0803 -0.05278,0.07962 -0.05389,0.07892 -0.055,0.07822 -0.0561,0.0775 -0.0572,0.07677 -0.05829,0.07602 -0.504875,0.57996 L 12.4578,12.3383 11.900575,12.796013 11.34335,13.253725 10.786125,13.711438 10.2289,14.16915 9.671675,14.626863 9.11445,15.084575 8.557225,15.542288 Z M 3,6 H 3.5 4 4.5 5 L 5.7784905,6.1571698 6.4142138,6.5857862 6.8428302,7.2215095 7,8 V 8.25 8.5 8.75 9 L 6,8.625 5,8.25 4,7.875 3,7.5 V 7.125 6.75 6.375 Z M 11,6 10.59693,6.040633 10.22151,6.1571699 9.8817808,6.3415685 9.5857862,6.5857862 9.3415685,6.8817808 9.1571698,7.2215095 9.0406329,7.5969301 9,8 V 8.125 8.25 8.375 8.5 8.625 8.75 8.875 9 L 9.5,8.8125 10,8.625 10.5,8.4375 11,8.25 11.5,8.0625 12,7.875 12.5,7.6875 13,7.5 V 7.3125 7.125 6.9375 6.75 6.5625 6.375 6.1875 6 H 12.75 12.5 12.25 12 11.75 11.5 11.25 Z")

(pcb-parse-path-string "m 126.53374,27.176719 -0.0857,-0.04644 -0.17347,-0.0036 -0.2422,0.03074 -0.29186,0.05643 -0.32242,0.07353 -0.33391,0.08202 -0.3263,0.08191 -0.29963,0.07321 -0.27621,0.06708 -0.26603,0.06851 -0.25166,0.06879 -0.23312,0.06793 -0.21041,0.06592 -0.18351,0.06277 -0.15244,0.05847 -0.1172,0.05302 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 -0.0702,0.03571 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 0.0249,-0.07476 7.9e-4,-0.0024 7.9e-4,-0.0024 8e-4,-0.0024 7.9e-4,-0.0024 7.9e-4,-0.0024 8e-4,-0.0024 7.9e-4,-0.0024 8e-4,-0.0024 7.9e-4,-0.0024 7.9e-4,-0.0024 8e-4,-0.0024 7.9e-4,-0.0024 7.9e-4,-0.0024 8e-4,-0.0024 7.9e-4,-0.0024 8e-4,-0.0024 -0.16751,0.04967 -0.17717,0.05796 -0.18536,0.06572 -0.1921,0.07294 -0.19737,0.07962 -0.20116,0.08577 -0.20351,0.09138 -0.20437,0.09646 -0.20377,0.100999 -0.20171,0.105004 -0.19818,0.108472 -0.19319,0.111405 -0.18673,0.113802 -0.1788,0.115664 -0.16941,0.116988 -0.15855,0.117779 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 -0.0747,0.05776 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 0.0236,-0.09145 2.6e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 5.2e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 5.3e-4,-0.0016 2.7e-4,-0.0016 2.6e-4,-0.0016 2.6e-4,-0.0016 -0.20686,0.136794 -0.20699,0.149213 -0.20597,0.159536 -0.20386,0.167761 -0.20059,0.173885 -0.19621,0.177914 -0.19071,0.179843 -0.18408,0.179674 -0.17632,0.177407 -0.16744,0.173042 -0.15743,0.166579 -0.1463,0.158017 -0.13405,0.147358 -0.12066,0.134599 -0.10615,0.119743 -0.0905,0.102789 -0.0115,0.01305 -0.0112,0.01279 -0.011,0.01252 -0.0108,0.01225 -0.0106,0.01198 -0.0103,0.0117 -0.0101,0.01142 -0.01,0.01114 -0.01,0.01086 -0.009,0.01056 -0.009,0.01027 -0.009,0.01 -0.009,0.0097 -0.008,0.0094 -0.008,0.0091 -0.008,0.0088 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 -0.0599,0.06655 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 0.003,-0.08701 -0.325,0.40601 -0.30896,0.436532 -0.29093,0.465771 -0.27095,0.49372 -0.24898,0.520384 -0.22503,0.545761 -0.19913,0.569851 -0.17125,0.592655 -0.14138,0.614172 -0.10955,0.634401 -0.0757,0.653346 -0.04,0.671001 -0.002,0.687373 0.0375,0.702455 0.0792,0.716252 0.12288,0.728762 0.0132,0.0657 0.0135,0.06578 0.0138,0.06587 0.0142,0.06594 0.0145,0.06602 0.0149,0.0661 0.0152,0.06619 0.0156,0.06627 0.016,0.06635 0.0164,0.06643 0.0168,0.06651 0.0172,0.0666 0.0176,0.06668 0.018,0.06677 0.0184,0.06685 0.0188,0.06694 0.0528,0.01487 0.0528,0.01463 0.0528,0.01439 0.0528,0.01418 0.0528,0.01394 0.0528,0.01373 0.0528,0.01349 0.0529,0.01325 0.0528,0.01304 0.0529,0.01283 0.0529,0.01259 0.0529,0.01238 0.0529,0.01217 0.0529,0.01196 0.0529,0.01172 0.0529,0.01151 0.16737,0.03502 0.16739,0.03279 0.1674,0.03056 0.16739,0.02832 0.16736,0.02609 0.16732,0.02384 0.16725,0.02159 0.16717,0.01931 0.16706,0.01707 0.16695,0.01482 0.16681,0.01254 0.16667,0.01027 0.16648,0.008 0.1663,0.0057 0.16609,0.0034 0.16586,0.0011 0.56333,-0.01326 0.54854,-0.03873 0.53328,-0.06268 0.51756,-0.08513 0.50136,-0.106039 0.4847,-0.125431 0.46758,-0.143301 0.44999,-0.15965 0.43193,-0.174477 0.41341,-0.18778 0.39443,-0.199562 0.37498,-0.209822 0.35506,-0.218559 0.33467,-0.225776 0.31383,-0.23147 0.29251,-0.235642 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 -0.0856,0.0025 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.0655,-0.05895 0.009,-0.0079 0.009,-0.0081 0.009,-0.0084 0.01,-0.0086 0.01,-0.0089 0.0103,-0.0091 0.0106,-0.0094 0.0109,-0.0096 0.0111,-0.0098 0.0114,-0.01008 0.0117,-0.01032 0.012,-0.01056 0.0123,-0.01079 0.0125,-0.01102 0.0128,-0.01125 0.0131,-0.01148 0.10277,-0.09051 0.11973,-0.106147 0.13459,-0.120657 0.14736,-0.134041 0.15801,-0.1463 0.16658,-0.157432 0.17304,-0.167441 0.17742,-0.176324 0.17967,-0.18408 0.17985,-0.190712 0.17792,-0.196218 0.1739,-0.200598 0.16776,-0.203853 0.15954,-0.205983 0.14922,-0.206987 0.13679,-0.206866 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.96e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.96e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.002,3.97e-4 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 -0.0914,0.0236 0.0578,-0.07472 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07472 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.0578,-0.07473 0.11757,-0.15832 0.11683,-0.169284 0.11555,-0.178766 0.11372,-0.186765 0.11136,-0.193284 0.10845,-0.19832 0.10501,-0.201874 0.10103,-0.203945 0.0965,-0.204536 0.0914,-0.203642 0.0858,-0.201268 0.0797,-0.197412 0.073,-0.192072 0.0658,-0.185252 0.058,-0.176949 0.0497,-0.167165 -0.002,7.68e-4 -0.002,7.67e-4 -0.002,7.67e-4 -0.002,7.68e-4 -0.002,7.67e-4 -0.002,7.94e-4 -0.002,7.93e-4 -0.002,7.94e-4 -0.002,7.94e-4 -0.002,7.94e-4 -0.002,7.93e-4 -0.002,7.94e-4 -0.002,7.94e-4 -0.002,8.2e-4 -0.002,8.2e-4 -0.002,8.2e-4 -0.0747,0.02485 -0.0748,0.02485 -0.0747,0.02486 -0.0748,0.02485 -0.0748,0.02485 -0.0747,0.02486 -0.0748,0.02485 -0.0747,0.02486 -0.0748,0.02485 -0.0747,0.02485 -0.0748,0.02486 -0.0748,0.02485 -0.0747,0.02485 -0.0748,0.02486 -0.0748,0.02486 -0.0747,0.02485 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0357,-0.07023 0.0631,-0.13829 0.066,-0.171179 0.0681,-0.199786 0.0693,-0.224109 0.0696,-0.244151 0.069,-0.25991 0.0675,-0.271386 0.0651,-0.27858 0.0699,-0.285521 0.0772,-0.289905 0.0768,-0.286554 0.0685,-0.275467 0.0524,-0.256643 0.0285,-0.230083 -0.003,-0.195788 -0.0427,-0.153755 v -1e-6 -1e-6 0 -10e-7 0 -1e-6 -1e-6 -1e-6 0 -10e-7 0 l -1e-5,-1e-6 v -1e-6 -10e-7 0 z")

(provide 'pcb-svg)

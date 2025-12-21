(define-module (scm-bin gg)
;;; All used modules must be present in the module (services cli-utils) under:
;;;   service-file -> with-imported-modules
  #:use-module (dotf utils)
  #:use-module (ice-9 optargs)     ; define*-public
  )

#|

#!/usr/bin/env -S guile \\
-L ./guix/common -L ./guix/home/common -e (scm-bin\ gg) -s
!#

cd $dotf
./guix/home/common/scm-bin/gg.scm

|#


;; https://gitlab.com/alezost-config/guix/-/blob/master/misc/shell-script-example/guix.scm?ref_type=heads
;; ;;; guix.scm --- Package to build/install shell script from this directory

;; ;; Copyright © 2016 Alex Kost <alezost@gmail.com>

;; ;; This program is free software; you can redistribute it and/or modify
;; ;; it under the terms of the GNU General Public License as published by
;; ;; the Free Software Foundation, either version 3 of the License, or
;; ;; (at your option) any later version.
;; ;;
;; ;; This program is distributed in the hope that it will be useful,
;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; ;; GNU General Public License for more details.
;; ;;
;; ;; You should have received a copy of the GNU General Public License
;; ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ;;; Commentary:

;; ;; See <http://lists.gnu.org/archive/html/help-guix/2016-08/msg00101.html>.

;; ;;; Code:

;; (use-modules
;;  (guix gexp)
;;  (guix packages)
;;  ;; (guix licenses)
;;  (guix build-system trivial)
;;  (gnu packages bash))

;; ;; The ./my-script is packaged
;; (let ((script-name "my-script"))
;;   (package
;;     (name script-name)
;;     (version "0.1")
;;     (source (local-file (string-append (dirname (current-filename))
;;                                        "/" script-name)))
;;     (build-system trivial-build-system)
;;     (arguments
;;      `(#:modules ((guix build utils))
;;        #:builder
;;        (begin
;;          (use-modules (guix build utils))
;;          (let* ((bin-dir  (string-append %output "/bin"))
;;                 (bin-file (string-append bin-dir "/" ,script-name))
;;                 (bash-bin (string-append (assoc-ref %build-inputs "bash")
;;                                          "/bin")))
;;            (mkdir-p bin-dir)
;;            (copy-file (assoc-ref %build-inputs "source") bin-file)
;;            (patch-shebang bin-file (list bash-bin))
;;            (chmod bin-file #o555)))))
;;     (inputs `(("bash" ,bash)))
;;     (home-page #f)
;;     (synopsis "bla bla")
;;     (description "More verbose bla bla")
;;     (license #f)))

;; ;;; guix.scm ends here

(define m (module-name-for-logging))
(evaluating-module)

(define-public (main args)
  ((comp
    exec-background
    (partial append (list "git" "gui"))
    cdr)
   args))
(testsymb 'main)

(module-evaluated)

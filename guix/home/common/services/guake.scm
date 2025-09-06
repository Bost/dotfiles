;;; Copyright © 2025 Rostislav Svoboda <Rostislav.Svoboda@gmail.com>
;;;
;;; This file is NOT part of GNU Guix.
;;;
;;; This program is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the Free
;;; Software Foundation; either version 3 of the License, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;; for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Home services for Guake, the Drop-down terminal for GNOME

;; Commands:
;;   guake --save-preferences=$dotf/.config/guake.cfg
;;   guake --restore-preferences=$dotf/.config/guake.cfg

(define-module (services guake)
  #:use-module (bost gnu packages guake)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  ;; #:use-module (ice-9 pretty-print)
  ;; #:use-module (srfi srfi-1)
  #:use-module (utils)

  #:export (<home-guake-configuration>
            home-guake-configuration
            home-guake-configuration-guake
            home-guake-service-type
            home-guake-service
            ))

(define m (module-name-for-logging))
(evaluating-module)

;; Define a record for the service configuration
(define-record-type* <home-guake-configuration>
  home-guake-configuration make-home-guake-configuration
  home-guake-configuration?
  ;; guake package
  (guake home-guake-configuration-guake
         (default guake)))

(define (home-guake-packages config)
  "Add guake package to profile."
  (define f (format #f "~a [home-guake-packages]" m))
  (let [(guake (home-guake-configuration-guake config))]
    (list guake)))
(testsymb 'home-guake-packages)

(define (home-guake-config-installer config)
  "Gexp to restore Guake preferences."

  (define f (format #f "~a [home-guake-config-installer]" m))
  (define guake-package (home-guake-configuration-guake config))
  ;; Here guake-package has value:
  ;; #<package guake@3.10 bost/gnu/packages/guake.scm:75 7f1c56228790>
  ;; (format #t "### ~a guake-package: ~a\n" f guake-package)

  #~(let [(guake-bin #$(file-append guake-package "/bin/guake"))]
      ;; (format #t "### ~a [gexp] guake-package: ~a\n" #$f #$guake-package)
      ;; Here guake-package has value:
      ;; /gnu/store/n85nrx9pphiw4wljc27rincdlrgzvg1q-guake-3.10
      (use-modules (utils) (fs-utils))
      ((comp
        (partial invoke guake-bin)
        (partial str "--restore-preferences=")
        user-dotf)
       "/.config/guake.cfg")))
(testsymb 'home-guake-config-installer)

(define home-guake-service-type
  (service-type
   (name 'home-guake)
   (extensions
    (list
     (service-extension home-profile-service-type home-guake-packages)
     (service-extension
      ;; defined by the upstream in gnu/home/services.scm
      home-activation-service-type
      ;; function of one parameter: config
      home-guake-config-installer)
     ))
   (default-value (home-guake-configuration))
   (description "Run Guake as a Guix Home service."))
  )
(testsymb 'home-guake-service-type)

(define (home-guake-service)
  (define f (format #f "~a [home-guake-service]" m))
  (service home-guake-service-type (home-guake-configuration)))
(testsymb 'home-guake-service)

(module-evaluated)

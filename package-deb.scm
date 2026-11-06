;; Builds dist/filme_<version>_all.deb (see deploy/debian/README.Debian):
;;
;;   scm package-deb.scm [--out DIR] [--dabscm DIR | --jar SCM.JAR] [--allow-dirty]
;;
;; The package bundles the scm.jar of the dabscm checkout next to this repo
;; (../dabscm; build it first with `scm build-scm-java.scm`). The version
;; comes from the last commit; uncommitted changes need --allow-dirty.
;; Installing it pulls in ffmpeg (for ffprobe, fast-start and thumbnails).

(import (scheme base)
        (scheme process-context)
        (scm deb webapp))

(webapp-deb-main '((pkg . "filme")
                   (maintainer . "Damian Brunold <damian.brunold@bluewin.ch>")
                   (summary .
                    "filme video portal for in-house tutorials (dabscm web application)")
                   (description
                    "Upload, describe and watch short MP4 videos in the browser: titles,"
                    "descriptions, tags, thumbnails, public/restricted/draft visibility with"
                    "optional per-video passphrases, viewer/editor/admin roles and per-site"
                    "branding. All data lives in /var/lib/filme; no database needed.")
                   (trees "src" "static" "docs")
                   (bin-exclude "dev-server.scm")
                   (data-subdirs "videos" "uploads" "site" "trash")
                   (config-settings ("data-dir" . "\"/var/lib/filme\"")
                                    ("secure-cookies?" . "#t"))
                   (setup-hints
                    "create the first administrator: sudo -u filme filme-admin user add <name> admin,"
                    "then put a TLS reverse proxy in front (see /usr/share/doc/filme/)")
                   (depends "ffmpeg")
                   (services "filme.service")
                   (timers "filme-health.timer" "filme-maintenance.timer")
                   (default-port . 8072))
                 (cdr (command-line)))

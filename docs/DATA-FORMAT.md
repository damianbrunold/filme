# filme data format (version 1)

All state lives in one folder, `data-dir` (`/var/lib/filme` when installed
from the package, `var/` in a development checkout). Every metadata file is
a single Scheme datum written with `write`. You can read it with any Scheme
`read`, or with a text editor. Files are replaced atomically: the new
content is written to `name.tmp` in the same directory and then renamed.
Backing up the folder at any time gives a consistent copy of each file.

```
data-dir/
  videos/<id>/         one directory per video
    meta.scm           metadata (below)
    video.mp4          the playable file (index at the front: "faststart")
    thumb.jpg          poster, at most 1280 px wide
    thumb-sm.jpg       grid thumbnail, at most 480 px wide
  videos/.changed      touched by CLI tools so a running server reloads
  users.scm            the users (below)
  site/settings.scm    branding (below)
  site/logo.<ext>      optional logo (png, webp or svg)
  site/favicon.<ext>   optional favicon
  uploads/<id>/        unfinished uploads: upload.scm + data.part
  trash/<id>/          deleted videos (a video directory + trashed.scm)
  .lock                OS lock file used by the CLI tools
```

Video ids are 11 random URL-safe characters (`A–Z a–z 0–9 - _`).

## `videos/<id>/meta.scm`

An association list:

| key | value |
|---|---|
| `version` | `1` |
| `id` | the video id (same as the directory name) |
| `title`, `description` | strings; the description is Markdown |
| `tags` | list of lower-case strings |
| `visibility` | `public`, `restricted` or `draft` |
| `passphrase-hash` | `#f`, or a `pbkdf2$…` hash of the video's passphrase |
| `allow-download` | `#t` / `#f` |
| `status` | `processing`, `ready` or `failed` |
| `error` | `#f`, or the processing error message |
| `warning` | `#f`, or a note (e.g. a codec that some browsers cannot play) |
| `original-filename` | name of the uploaded file |
| `size` | bytes of `video.mp4` |
| `duration` | seconds (real number) |
| `width`, `height` | pixels |
| `video-codec`, `audio-codec` | ffprobe codec names (`"h264"`, `"aac"`; `""` if none) |
| `thumbnail-at` | second the thumbnail was taken from, `custom` for an uploaded image, or `#f` |
| `created`, `updated` | Unix seconds |
| `created-by`, `updated-by` | user names (or `#f`) |

Example:

```scheme
((version . 1) (id . "Rlszt-uxOZE") (title . "Git Grundlagen")
 (description . "Commits, **Branches** und Merges.") (tags "git" "intro")
 (visibility . restricted) (passphrase-hash . "pbkdf2$600000$…")
 (allow-download . #f) (status . ready) (error . #f) (warning . #f)
 (original-filename . "git.mp4") (size . 30541280) (duration . 612.4)
 (width . 1920) (height . 1080) (video-codec . "h264") (audio-codec . "aac")
 (thumbnail-at . 61.2) (created . 1793972000) (updated . 1793972500)
 (created-by . "dab") (updated-by . "dab"))
```

## `users.scm`

A list of association lists:

```scheme
(((name . "dab") (hash . "pbkdf2$600000$…") (role . admin)
  (created . 1793971758) (disabled . #f)))
```

`role` is `viewer`, `editor` or `admin`. Each role includes the rights of
the roles before it. Viewers can watch restricted videos. Editors can
upload, edit and delete videos. Admins can also manage users and branding.

## `site/settings.scm`

```scheme
((site-name . "Filme") (tagline . "") (default-mode . auto)
 (light (bg . "#ffffff") (surface . "#f6f7f9") (fg . "#1a1a1a")
        (muted . "#5f6368") (accent . "#2563eb") (on-accent . "#ffffff")
        (border . "#e2e4e8"))
 (dark (bg . "#14161b") …)
 (logo . "logo.png") (favicon . #f))
```

`default-mode` is `auto` (follow the OS), `light` or `dark`. A missing key
or an invalid colour falls back to the default value.

## Restoring

Stop the service, copy the folder back and make it owned by `filme:filme`,
then start the service. The server reads every `meta.scm` at startup.

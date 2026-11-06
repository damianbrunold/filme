# filme

A small video portal for in-house tutorials and technical explanations,
written in [dabscm](../dabscm). It is built for dozens to hundreds of
videos and a handful of users, not for YouTube scale.

- Upload MP4 files of any size. Uploads are sent in 16 MiB chunks and
  resume after an interruption.
- Each video gets a title, a Markdown description, tags and a thumbnail.
  The thumbnail can be any frame of the video or an uploaded image.
- Uploaded videos are processed automatically. ffprobe reads duration and
  codecs. If the file's index (`moov`) is not at the front, ffmpeg moves it
  there losslessly ("faststart"), so playback starts at once. A poster frame
  becomes the thumbnail.
- Browsing: a grid with search, tag filter and sorting. The watch page
  remembers the playback position, has a "copy link" button and an optional
  download button.
- Visibility per video:
  - **public**: anyone can watch.
  - **restricted**: signed-in users, or anyone with the video's passphrase.
  - **draft**: editors only.
- Roles:
  - **viewer**: watches restricted videos.
  - **editor**: uploads, edits and deletes videos.
  - **admin**: also manages users and branding.
- Per-installation branding: site name, footer text, light and dark colour
  schemes with a live preview and a contrast check, default scheme, logo
  and favicon.
- No database. Everything lives in one data folder (see
  [docs/DATA-FORMAT.md](docs/DATA-FORMAT.md)), so a backup is a copy of
  that folder.
- Ships as a Debian package that pulls in ffmpeg and a Java runtime.

## Run (development)

You need dabscm (`scm` on the PATH) and ffmpeg.

```sh
scm bin/user.scm add alice admin   # prompts for a password; creates config.scm on the first run
scm bin/server.scm                 # http://127.0.0.1:8072
scm bin/dev-server.scm             # the same, restarts when src/ or the config changes
scm bin/import.scm --public some/folder/   # bulk-import MP4 files
```

Data goes to `var/` (gitignored). Settings are in `config.scm`; see
`config.example.scm` for every setting.

## Layout

```
bin/server.scm        load the config, serve the app
bin/dev-server.scm    auto-reloading dev supervisor
bin/user.scm          add / passwd / role / disable / enable / delete / list users
bin/import.scm        bulk-import MP4 files
bin/check.scm         consistency check of the data folder (--fix, --reprocess)
bin/maintenance.scm   purge stale uploads and old trash (nightly timer)
bin/gen-secret.scm    print a new session secret (for key rotation)
bin/init-config.scm   write a new config file
src/app-config.sld    config reading and first-run creation
src/app-cli.sld       shared plumbing of the bin/ scripts
src/app-site.sld      URL prefix: url, asset, cookie-path
src/app-store.sld     atomic data files, ids, the data-dir lock
src/app-users.sld     users.scm and roles
src/app-auth.sld      sign-in, sessions (signed SameSite=Lax cookies), passwords
src/app-tokens.sld    personal API tokens (Bearer) for the upload API
src/app-access.sld    who may see which video; passphrase unlock cookies
src/app-videos.sld    video metadata, in-memory index, search, trash
src/app-uploads.sld   chunked, resumable uploads
src/app-media.sld     ffprobe/ffmpeg, fast-start, thumbnails, processing queue
src/app-theme.sld     branding settings, theme.css, contrast check
src/app-views.sld     HTML pages (German UI)
src/app-web.sld       routes, handlers, middleware: make-app
static/               CSS and ES-module JavaScript (no build step)
tests/                run_all.sh; test_pure_*.scm need no server or ffmpeg
deploy/               systemd units, nginx/apache examples, README.Debian
docs/DATA-FORMAT.md   the data folder, file by file
package-deb.scm       builds the Debian package
```

## API tokens

Programs can upload videos on a user's behalf with a personal API token,
for example the [aufnahme](../aufnahme) recorder's "In filme
veröffentlichen".
- **Creating:** editors and admins create tokens on their profile page
  (`/profile`). A token is shown once, at creation; only a SHA-256 hash of
  it is stored in `users.scm`.
- **Using:** send it as `Authorization: Bearer fltk_...`.
- **Where it works:** only on the upload and video API:
  - `POST /api/uploads`, `PUT /api/uploads/:id?offset=N`;
  - `POST /api/uploads/:id/complete`, with optional JSON `{"title",
    "description", "tags"}`;
  - `GET /api/videos/:id`.

  Pages, settings and user admin never accept a token.
- **Result:** uploaded videos arrive as drafts, created by the token's
  user.
- **Revoking:** revoke tokens on the profile page. Disabling or deleting
  the user also ends their tokens.

## Security notes

- Every page is rendered with the auto-escaping `(scm html builder)`.
  Descriptions go through `markdown->html`, which escapes raw HTML.
- A strict Content-Security-Policy is set: no inline scripts or styles.
  The colours come from the generated `/theme.css`.
- Passwords and passphrases are stored as PBKDF2-SHA256 hashes.
  - Failed sign-ins and passphrase attempts are slowed down and limited
    per IP.
  - Changing a password ends that user's other sessions. Changing a
    passphrase revokes earlier unlocks.
- The session cookie is `SameSite=Lax`, so links from mail or chat arrive
  signed in. Cross-site POST/PUT/DELETE requests are refused by an
  Origin check.
- Media and thumbnail requests are checked against the same access rules
  as the watch page.
- Anonymous visitors only see public videos in the listing. Restricted
  videos are reached through their link.

## Tests

```sh
tests/run_all.sh              # SKIP_MEDIA=1 leaves out the ffmpeg test
```

## Package

```sh
scm package-deb.scm           # dist/filme_<version>_all.deb (needs a commit)
sudo apt install ./dist/filme_*_all.deb
sudo -u filme filme-admin user add alice admin
```

See [deploy/debian/README.Debian](deploy/debian/README.Debian) for the
installed layout, the reverse proxy and backups.

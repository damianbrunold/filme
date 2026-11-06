// Chunked, resumable upload of one file (ES module, shared by upload.js
// and edit.js).
//
// Protocol (see src/app-web.sld):
//   POST   /api/uploads {filename, size, target}  -> {id, offset, chunkSize}
//   GET    /api/uploads/:id                       -> {offset, size}
//   PUT    /api/uploads/:id?offset=N  (raw chunk) -> {offset} | 409 {offset}
//   POST   /api/uploads/:id/complete              -> {video, editUrl}
//   DELETE /api/uploads/:id
// The upload id is kept in localStorage under the file's name, size and
// modification time, so choosing the same file again after a reload or a
// lost connection continues where it stopped.

export function apiBase() {
  // The page's own prefix: everything before the first known route.
  const m = location.pathname.match(/^(.*?)\/(upload|v\/[^/]+\/edit)$/);
  return (m ? m[1] : "") + "/api";
}

function storeKey(file, target) {
  return "upload:" + (target || "") + ":" + file.name + ":" + file.size + ":" + file.lastModified;
}

function remember(key, value) {
  try {
    if (value) localStorage.setItem(key, value);
    else localStorage.removeItem(key);
  } catch (e) {}
}

function recall(key) {
  try {
    return localStorage.getItem(key);
  } catch (e) {
    return null;
  }
}

async function json(resp) {
  try {
    return await resp.json();
  } catch (e) {
    return {};
  }
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

export class Upload {
  // onProgress({sent, total, rate}); onStatus(text)
  constructor(file, { target = null, onProgress = () => {}, onStatus = () => {} } = {}) {
    this.file = file;
    this.target = target;
    this.onProgress = onProgress;
    this.onStatus = onStatus;
    this.paused = false;
    this.cancelled = false;
    this.key = storeKey(file, target);
    this.id = null;
    this.resumeWaiter = null;
  }

  pause() {
    this.paused = true;
  }

  resume() {
    this.paused = false;
    if (this.resumeWaiter) this.resumeWaiter();
  }

  async cancel() {
    this.cancelled = true;
    this.resume();
    remember(this.key, null);
    if (this.id) await fetch(apiBase() + "/uploads/" + this.id, { method: "DELETE" });
  }

  async begin() {
    const old = recall(this.key);
    if (old) {
      const r = await fetch(apiBase() + "/uploads/" + old);
      if (r.ok) {
        const j = await json(r);
        this.id = old;
        return j;
      }
      remember(this.key, null);
    }
    const r = await fetch(apiBase() + "/uploads", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ filename: this.file.name, size: this.file.size, target: this.target }),
    });
    const j = await json(r);
    if (!r.ok) throw new Error(j.error || "Upload konnte nicht starten (" + r.status + ")");
    this.id = j.id;
    remember(this.key, j.id);
    return j;
  }

  // Resolves with {video, editUrl}.
  async run() {
    const info = await this.begin();
    let offset = info.offset || 0;
    const chunk = info.chunkSize || 16 * 1024 * 1024;
    const total = this.file.size;
    let started = performance.now();
    let startOffset = offset;
    let failures = 0;
    while (offset < total) {
      if (this.cancelled) throw new Error("Abgebrochen");
      if (this.paused) {
        this.onStatus("Pausiert");
        await new Promise((r) => (this.resumeWaiter = r));
        this.resumeWaiter = null;
        started = performance.now();
        startOffset = offset;
        continue;
      }
      const end = Math.min(offset + chunk, total);
      let r;
      try {
        r = await fetch(apiBase() + "/uploads/" + this.id + "?offset=" + offset, {
          method: "PUT",
          headers: { "Content-Type": "application/octet-stream" },
          body: this.file.slice(offset, end),
        });
      } catch (e) {
        r = null;
      }
      if (r && r.ok) {
        offset = (await json(r)).offset;
        failures = 0;
      } else if (r && r.status === 409) {
        offset = (await json(r)).offset; // the server knows better
      } else if (r && r.status >= 400 && r.status < 500 && r.status !== 408) {
        const j = await json(r);
        remember(this.key, null);
        throw new Error(j.error || "Upload abgelehnt (" + r.status + ")");
      } else {
        // Network or server trouble: retry with a growing pause.
        failures++;
        const wait = Math.min(30, 2 ** failures);
        this.onStatus("Verbindung unterbrochen – neuer Versuch in " + wait + " s …");
        await sleep(wait * 1000);
        try {
          const s = await fetch(apiBase() + "/uploads/" + this.id);
          if (s.ok) offset = (await json(s)).offset;
        } catch (e) {}
        continue;
      }
      const secs = (performance.now() - started) / 1000;
      this.onProgress({ sent: offset, total, rate: secs > 0.5 ? (offset - startOffset) / secs : 0 });
    }
    this.onStatus("Wird abgeschlossen …");
    const r = await fetch(apiBase() + "/uploads/" + this.id + "/complete", { method: "POST" });
    const j = await json(r);
    if (!r.ok) {
      remember(this.key, null);
      throw new Error(j.error || "Abschluss fehlgeschlagen (" + r.status + ")");
    }
    remember(this.key, null);
    return j;
  }
}

export function formatBytes(n) {
  if (n >= 1073741824) return (n / 1073741824).toFixed(2) + " GB";
  if (n >= 1048576) return (n / 1048576).toFixed(1) + " MB";
  return Math.round(n / 1024) + " KB";
}

export function formatEta(seconds) {
  if (!isFinite(seconds) || seconds <= 0) return "";
  const m = Math.floor(seconds / 60);
  const s = Math.round(seconds % 60);
  return m > 0 ? m + " min " + s + " s" : s + " s";
}

// Edit page: poll while processing, pick a thumbnail frame or upload an
// image, replace the video file.
import { Upload, apiBase, formatBytes } from "./uploader.js";

const root = document.querySelector("section.edit");
const id = root.dataset.videoId;
const api = apiBase() + "/videos/" + id;

if (root.dataset.status === "processing") {
  const poll = async () => {
    try {
      const r = await fetch(api);
      if (r.ok && (await r.json()).status !== "processing") {
        location.reload();
        return;
      }
    } catch (e) {}
    setTimeout(poll, 2000);
  };
  setTimeout(poll, 2000);
}

const picker = document.querySelector(".js-frame-picker");
const preview = document.querySelector(".js-thumb-preview");
const tstatus = document.querySelector(".js-thumb-status");

async function setThumb(body, contentType) {
  tstatus.textContent = "Vorschaubild wird erstellt …";
  const r = await fetch(api + (contentType === "application/json" ? "/thumbnail" : "/thumbnail-image"), {
    method: "POST",
    headers: { "Content-Type": contentType },
    body,
  });
  let j = {};
  try {
    j = await r.json();
  } catch (e) {}
  if (r.ok) {
    preview.src = j.thumb;
    tstatus.textContent = "Vorschaubild gespeichert.";
  } else {
    tstatus.textContent = "Fehler: " + (j.error || r.status);
  }
}

if (picker) {
  document.querySelector(".js-use-frame").addEventListener("click", () => {
    setThumb(JSON.stringify({ at: picker.currentTime }), "application/json");
  });
  for (const b of document.querySelectorAll(".js-frame-at")) {
    b.addEventListener("click", () => {
      const d = parseFloat(root.dataset.duration) || picker.duration || 0;
      const at = d * parseFloat(b.dataset.fraction);
      picker.currentTime = at;
      setThumb(JSON.stringify({ at }), "application/json");
    });
  }
  document.querySelector(".js-thumb-file").addEventListener("change", (e) => {
    const f = e.target.files[0];
    if (f) setThumb(f, f.type || "application/octet-stream");
  });
}

const replace = document.querySelector(".js-replace-file");
if (replace) {
  const bar = document.querySelector(".js-replace-progress");
  const rstatus = document.querySelector(".js-replace-status");
  replace.addEventListener("change", async () => {
    const file = replace.files[0];
    if (!file) return;
    if (!window.confirm("Die Videodatei durch „" + file.name + "“ ersetzen? Link und Angaben bleiben erhalten.")) {
      replace.value = "";
      return;
    }
    bar.hidden = false;
    const up = new Upload(file, {
      target: id,
      onProgress: ({ sent, total }) => {
        bar.value = (100 * sent) / total;
        rstatus.textContent = formatBytes(sent) + " von " + formatBytes(total);
      },
      onStatus: (t) => (rstatus.textContent = t),
    });
    try {
      await up.run();
      location.reload();
    } catch (e) {
      rstatus.textContent = "Fehler: " + e.message;
    }
  });
}

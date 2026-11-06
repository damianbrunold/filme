// Upload page: drag and drop or choose MP4 files; they upload one after
// another, each with progress, pause and cancel.
import { Upload, formatBytes, formatEta } from "./uploader.js";

const zone = document.querySelector(".js-dropzone");
const input = document.querySelector(".js-file");
const list = document.querySelector(".js-upload-list");
const tpl = document.getElementById("upload-item");
const queue = [];
let running = false;

function addFiles(files) {
  for (const file of files) {
    const li = tpl.content.firstElementChild.cloneNode(true);
    li.querySelector(".upload-name").textContent = file.name + " (" + formatBytes(file.size) + ")";
    const status = li.querySelector(".upload-status");
    const bar = li.querySelector("progress");
    status.textContent = "Wartet …";
    list.appendChild(li);
    if (!/\.(mp4|m4v)$/i.test(file.name) && file.type !== "video/mp4") {
      status.textContent = "Keine MP4-Datei – übersprungen.";
      li.querySelector(".upload-actions").hidden = true;
      continue;
    }
    const up = new Upload(file, {
      onProgress: ({ sent, total, rate }) => {
        bar.value = (100 * sent) / total;
        status.textContent =
          formatBytes(sent) + " von " + formatBytes(total) +
          (rate ? " · " + formatBytes(rate) + "/s · noch " + formatEta((total - sent) / rate) : "");
      },
      onStatus: (t) => (status.textContent = t),
    });
    const pause = li.querySelector(".js-pause");
    pause.addEventListener("click", () => {
      if (up.paused) {
        up.resume();
        pause.textContent = "Pause";
      } else {
        up.pause();
        pause.textContent = "Fortsetzen";
      }
    });
    li.querySelector(".js-cancel").addEventListener("click", () => {
      up.cancel();
      status.textContent = "Abgebrochen.";
      li.querySelector(".upload-actions").hidden = true;
    });
    queue.push({ up, li, status, bar });
  }
  next();
}

async function next() {
  if (running || queue.length === 0) return;
  running = true;
  const { up, li, status, bar } = queue.shift();
  try {
    if (!up.cancelled) {
      status.textContent = "Wird hochgeladen …";
      const res = await up.run();
      bar.value = 100;
      status.textContent = "Hochgeladen. Das Video wird verarbeitet und ist vorerst ein Entwurf.";
      li.querySelector(".js-pause").hidden = true;
      li.querySelector(".js-cancel").hidden = true;
      const edit = li.querySelector(".js-edit");
      edit.href = res.editUrl;
      edit.hidden = false;
    }
  } catch (e) {
    if (!up.cancelled) status.textContent = "Fehler: " + e.message;
  }
  running = false;
  next();
}

input.addEventListener("change", () => {
  addFiles(input.files);
  input.value = "";
});
for (const ev of ["dragenter", "dragover"]) {
  zone.addEventListener(ev, (e) => {
    e.preventDefault();
    zone.classList.add("over");
  });
}
for (const ev of ["dragleave", "drop"]) {
  zone.addEventListener(ev, () => zone.classList.remove("over"));
}
zone.addEventListener("drop", (e) => {
  e.preventDefault();
  addFiles(e.dataTransfer.files);
});
window.addEventListener("beforeunload", (e) => {
  if (running) e.preventDefault();
});

// Watch page: remembers the playback position per video (in this browser)
// and resumes there; "copy link" copies the video's address.
const video = document.querySelector(".js-player");
if (video) {
  const key = "pos:" + video.dataset.videoId;
  let last = 0;
  video.addEventListener("loadedmetadata", () => {
    let pos = 0;
    try {
      pos = parseFloat(localStorage.getItem(key) || "0");
    } catch (e) {}
    // Resume unless it is (nearly) at the start or the end.
    if (pos > 5 && pos < video.duration - 10) video.currentTime = pos;
  });
  video.addEventListener("timeupdate", () => {
    if (Math.abs(video.currentTime - last) < 5) return;
    last = video.currentTime;
    try {
      localStorage.setItem(key, String(Math.floor(last)));
    } catch (e) {}
  });
  video.addEventListener("ended", () => {
    try {
      localStorage.removeItem(key);
    } catch (e) {}
  });
}

for (const btn of document.querySelectorAll(".js-copy-link")) {
  btn.addEventListener("click", async () => {
    const link = new URL(btn.dataset.url, location.href).href;
    try {
      await navigator.clipboard.writeText(link);
      btn.textContent = "Link kopiert";
    } catch (e) {
      window.prompt("Link:", link);
    }
  });
}

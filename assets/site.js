document.getElementById("year").textContent = String(new Date().getFullYear());

(function () {
  var toggle = document.getElementById("theme-toggle");
  if (!toggle) return;

  var stored = localStorage.getItem("theme");
  if (stored) {
    document.documentElement.setAttribute("data-theme", stored);
  } else if (window.matchMedia("(prefers-color-scheme: dark)").matches) {
    document.documentElement.setAttribute("data-theme", "dark");
  }

  function updateIcon() {
    var isDark = document.documentElement.getAttribute("data-theme") === "dark";
    toggle.textContent = isDark ? "\u2600\uFE0F" : "\uD83C\uDF19";
    toggle.setAttribute("aria-label", isDark ? "Switch to light mode" : "Switch to dark mode");
  }

  updateIcon();

  toggle.addEventListener("click", function () {
    var isDark = document.documentElement.getAttribute("data-theme") === "dark";
    var next = isDark ? "light" : "dark";
    document.documentElement.setAttribute("data-theme", next);
    localStorage.setItem("theme", next);
    updateIcon();
  });
})();

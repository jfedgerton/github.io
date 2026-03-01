document.getElementById("year").textContent = String(new Date().getFullYear());

// Mobile hamburger menu toggle
(function () {
  var toggle = document.querySelector(".menu-toggle");
  var navList = document.querySelector(".nav-list");
  if (!toggle || !navList) return;
  toggle.addEventListener("click", function () {
    var open = navList.classList.toggle("open");
    toggle.setAttribute("aria-expanded", String(open));
  });
})();

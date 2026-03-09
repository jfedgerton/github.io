# Jared Edgerton

This repository hosts a personal academic website built using **GitHub Pages**.

## Current site structure

The site is a multi-page static website with shared styling and navigation:

```text
.
├── index.html          # Homepage
├── research.html       # Research overview
├── projects.html       # Projects network visualization
├── teaching.html       # Teaching page
├── cv.html             # CV download page
├── contact.html        # Contact page
├── assets/
│   ├── styles.css      # Shared styles
│   └── site.js         # Shared behavior (footer year, dark mode toggle)
└── README.md
```

## Notes

- Navigation links are wired across all pages.
- A dark mode toggle is available in the site header; preference is persisted via localStorage.
- The Inter font is loaded from Google Fonts.

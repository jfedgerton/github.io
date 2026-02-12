# Jared Edgerton

This repository hosts a personal academic website built using **GitHub Pages**.

## Current site structure

The site is now scaffolded as a multi-page static website with shared styling and navigation:

```text
.
├── index.html          # Homepage
├── research.html       # Research overview
├── projects.html       # Projects page
├── teaching.html       # Teaching page
├── cv.html             # CV download page
├── contact.html        # Contact page
├── assets/
│   ├── styles.css      # Shared styles
│   └── site.js         # Small shared behavior (footer year)
├── cv/
│   └── CV.pdf          # Public CV (stable filename)
└── README.md
```

## Notes

- Navigation links are wired across all pages.
- `cv/CV.pdf` remains the stable URL target for CV sharing.
- Page content currently uses placeholders in a few sections so you can quickly swap in polished copy from your existing site.

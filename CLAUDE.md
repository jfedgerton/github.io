# CLAUDE.md

## Project Overview

Personal academic portfolio website for Jared Edgerton (computational political scientist, Penn State). Hosted on GitHub Pages at jfedgerton.github.io.

## Tech Stack

- **Pure static site**: HTML, CSS, vanilla JavaScript — no build step, no bundler, no framework
- **External dependency**: D3.js v7 (loaded via CDN in `projects.html`)
- **Hosting**: GitHub Pages (auto-deploys from `main` branch)

## Repository Structure

```
index.html          # Homepage
research.html       # Research overview
projects.html       # Interactive D3.js network diagram of research projects
teaching.html       # Teaching history
cv.html             # CV download page
contact.html        # Contact info
assets/
  styles.css        # Shared stylesheet (CSS custom properties for theming)
  site.js           # Minimal shared JS (footer year)
```

## Coding Conventions

- **Indentation**: 2 spaces (HTML, CSS, JS)
- **HTML**: Semantic HTML5, ARIA labels on nav/sections, proper heading hierarchy
- **CSS**: Custom properties for colors/theming, mobile-responsive via `@media (max-width: 720px)`
- **JS**: Vanilla ES6+, no frameworks — keep it minimal
- **Navigation**: Consistent header nav across all pages; active page uses `.active` class

## Key Design Decisions

- Zero-build philosophy: no package.json, no Gemfile, no build tooling
- CSS variables define the theme (primary accent `#0f766e`, background `#f5f7fb`)
- D3 visualization in projects.html is self-contained inline script
- System font stack (Inter with fallbacks)

## Development Workflow

- No CI/CD pipeline — push to `main` to deploy
- No tests or linters configured
- Feature branches merge into `main`

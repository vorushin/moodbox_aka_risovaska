# Risovaska 2.0

A creative drawing app for kids and families. Draw, create, share!

This is the modern web rewrite of the original [Risovaska](https://github.com/vorushin/moodbox_aka_risovaska) desktop app (Qt/C++) — now running entirely in the browser as a mobile-first PWA.

## Running locally

```bash
npm install
npm run dev
```

## Tech stack

- **React 19** + **TypeScript** — UI components
- **Vite** — build tooling
- **Tailwind CSS v4** — styling
- **Zustand** — state management
- **IndexedDB** (via idb-keyval) — local gallery storage
- **HTML5 Canvas** — drawing engine

## Drawing engine

The brush system is inspired by the original [Velasquez engine](../velasquez/) and ported to HTML5 Canvas with modernizations:

| Brush | Description |
|-------|-------------|
| Pencil | Simple round strokes, good for fine lines |
| Marker | Wide semi-transparent strokes, no self-intersection buildup |
| Oil Brush | Speed-sensitive size/opacity with color deviation — faster strokes are thinner and more transparent |
| Watercolor | Soft layered blobs with speed-sensitive spread and subtle color variation |
| Spray | Random particle distribution within a circle, builds up on overlap |
| Eraser | Removes drawn content via `destination-out` compositing |

Key engine features:
- **Two-canvas architecture**: main canvas for committed strokes + active canvas for in-progress stroke preview
- **Stroke interpolation**: distance-based point spacing matching the original Velasquez algorithm
- **DPR-aware rendering**: crisp on Retina/HiDPI displays
- **Undo/redo**: ImageData snapshots (up to 40 steps)

## Features

- **150+ SVG clipart** from the original app (Animals, City, Heads, Music, Nature, Personages, Princess, Smile, Transport, Travel)
- **Text tool** — place text on the canvas in any color
- **Color picker** — 4 palettes (vivid, pastel, earth, grayscale) + custom color + background color
- **Brush size & opacity** controls with quick presets
- **Local gallery** — save drawings to IndexedDB, browse and delete
- **Export** — download as PNG or share via Web Share API
- **Creative prompts** — 40+ drawing ideas to spark inspiration
- **Mobile UX** — two-finger tap to undo, three-finger tap to redo, custom cursor preview
- **Onboarding hints** — auto-dismissing tips for first-time users
- **PWA** — installable on mobile via manifest

## Project structure

```
src/
  engine/           Drawing engine (no React dependency)
    brushes.ts        All brush implementations
    color-utils.ts    HSV/RGB conversion, color deviation
    drawing-engine.ts Canvas management, stroke handling
    history.ts        Undo/redo with ImageData snapshots
    types.ts          Shared TypeScript types
  components/       React UI components
  store.ts          Zustand global state
  utils/            Gallery DB, clipart data
public/
  clipart/          150+ original SVG clipart assets
  brushes/          Original brush texture PNGs
  avatars/          Original avatar PNGs
```

## License

Program code — MIT. Clipart & graphics — CC BY.

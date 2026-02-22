import type { BrushStroke, BrushSettings } from './types';
import { addColorDeviation, hexToRgb } from './color-utils';

// ============================================================
// Brush interface — each brush knows how to paint strokes
// ============================================================

export interface Brush {
  readonly type: string;
  /** Distance between stroke samples in px */
  getSpacing(settings: BrushSettings): number;
  /** Whether this brush responds to speed */
  readonly tracksSpeed: boolean;
  /** Paint a single stroke sample onto ctx */
  paint(
    ctx: CanvasRenderingContext2D,
    stroke: BrushStroke,
    prevStroke: BrushStroke | null,
    settings: BrushSettings,
    index: number,
  ): void;
  /** Configure context before a sequence of strokes */
  begin(ctx: CanvasRenderingContext2D, settings: BrushSettings): void;
  /** Finalize after stroke sequence */
  end(ctx: CanvasRenderingContext2D): void;
}

// ============================================================
// Size helpers — map 0-1 normalized size to px
// ============================================================

function sizeToPixels(normalized: number, min: number, max: number): number {
  return min + normalized * (max - min);
}

// ============================================================
// PENCIL — simple round strokes connected by lines
// ============================================================

export const pencilBrush: Brush = {
  type: 'pencil',
  tracksSpeed: false,
  getSpacing() { return 1; },

  begin(ctx, settings) {
    const width = sizeToPixels(settings.size, 1, 20);
    ctx.strokeStyle = settings.color;
    ctx.lineWidth = width;
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';
    ctx.globalAlpha = settings.opacity;
    ctx.globalCompositeOperation = 'source-over';
  },

  paint(ctx, stroke, prev) {
    if (!prev) {
      // Single dot
      ctx.beginPath();
      ctx.arc(stroke.x, stroke.y, ctx.lineWidth / 2, 0, Math.PI * 2);
      ctx.fillStyle = ctx.strokeStyle;
      ctx.fill();
      return;
    }
    ctx.beginPath();
    ctx.moveTo(prev.x, prev.y);
    ctx.lineTo(stroke.x, stroke.y);
    ctx.stroke();
  },

  end() {},
};

// ============================================================
// MARKER — wide semi-transparent strokes (no self-intersection buildup)
// Draws at full opacity on active canvas; engine composites at target opacity.
// ============================================================

export const markerBrush: Brush = {
  type: 'marker',
  tracksSpeed: false,
  getSpacing() { return 1; },

  begin(ctx, settings) {
    const width = sizeToPixels(settings.size, 8, 48);
    ctx.strokeStyle = settings.color;
    ctx.lineWidth = width;
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';
    // Full opacity on active canvas — composited at reduced opacity in endStroke
    ctx.globalAlpha = 1;
    ctx.globalCompositeOperation = 'source-over';
  },

  paint(ctx, stroke, prev) {
    if (!prev) {
      ctx.beginPath();
      ctx.arc(stroke.x, stroke.y, ctx.lineWidth / 2, 0, Math.PI * 2);
      ctx.fillStyle = ctx.strokeStyle;
      ctx.fill();
      return;
    }
    ctx.beginPath();
    ctx.moveTo(prev.x, prev.y);
    ctx.lineTo(stroke.x, stroke.y);
    ctx.stroke();
  },

  end() {},
};

// ============================================================
// OIL BRUSH — speed-sensitive, textured, with color deviation
// Inspired by Velasquez engine OilBrushElement
// ============================================================

export const oilBrush: Brush = {
  type: 'oil',
  tracksSpeed: true,

  getSpacing(settings) {
    return Math.max(2, sizeToPixels(settings.size, 2, 6));
  },

  begin(ctx) {
    ctx.globalCompositeOperation = 'source-over';
  },

  paint(ctx, stroke, _prev, settings) {
    const baseWidth = sizeToPixels(settings.size, 6, 50);

    // Speed affects size and opacity (from Velasquez: faster = smaller + more transparent)
    const maxSpeed = 2.0;
    const speedNorm = Math.min(stroke.speed, maxSpeed) / maxSpeed;

    const size = baseWidth * (1 - speedNorm * 0.4);
    const alpha = settings.opacity * (0.15 + (1 - speedNorm) * 0.35);

    // Color deviation for oil paint feel
    const color = addColorDeviation(settings.color, 0.02, 0.08, 0.05);
    const rgb = hexToRgb(color);

    ctx.save();
    ctx.globalAlpha = alpha;
    ctx.translate(stroke.x, stroke.y);

    // Rotate based on stroke angle for brush texture feel
    if (stroke.angle !== 0) {
      ctx.rotate(stroke.angle);
    }

    // Draw a slightly irregular ellipse to simulate brush texture
    const aspect = 0.6 + Math.random() * 0.3; // Slight randomness
    ctx.beginPath();
    ctx.ellipse(0, 0, size / 2, (size * aspect) / 2, 0, 0, Math.PI * 2);
    ctx.fillStyle = `rgb(${rgb.r}, ${rgb.g}, ${rgb.b})`;
    ctx.fill();

    ctx.restore();
  },

  end() {},
};

// ============================================================
// SPRAY — random particle spray, buildup enabled
// Inspired by Velasquez SprayElement
// ============================================================

export const sprayBrush: Brush = {
  type: 'spray',
  tracksSpeed: false,

  getSpacing() { return 3; },

  begin(ctx) {
    ctx.globalCompositeOperation = 'source-over';
  },

  paint(ctx, stroke, _prev, settings) {
    const radius = sizeToPixels(settings.size, 10, 60);
    const density = Math.floor(radius * 1.5);

    // Color deviation for spray variation
    const rgb = hexToRgb(settings.color);

    ctx.save();
    ctx.globalAlpha = settings.opacity * 0.3;

    for (let i = 0; i < density; i++) {
      // Random point within circle (uniform distribution)
      const angle = Math.random() * Math.PI * 2;
      const r = Math.sqrt(Math.random()) * radius;
      const px = stroke.x + Math.cos(angle) * r;
      const py = stroke.y + Math.sin(angle) * r;
      const dotSize = 0.5 + Math.random() * 1.5;

      // Slight color variation per particle
      const vr = Math.max(0, Math.min(255, rgb.r + (Math.random() - 0.5) * 20));
      const vg = Math.max(0, Math.min(255, rgb.g + (Math.random() - 0.5) * 20));
      const vb = Math.max(0, Math.min(255, rgb.b + (Math.random() - 0.5) * 20));

      ctx.fillStyle = `rgb(${vr|0}, ${vg|0}, ${vb|0})`;
      ctx.fillRect(px - dotSize / 2, py - dotSize / 2, dotSize, dotSize);
    }

    ctx.restore();
  },

  end() {},
};

// ============================================================
// WATERCOLOR — soft, transparent, builds up gently
// Multiple overlapping transparent circles for a watery feel
// ============================================================

export const watercolorBrush: Brush = {
  type: 'watercolor',
  tracksSpeed: true,

  getSpacing(settings) {
    return Math.max(3, sizeToPixels(settings.size, 3, 8));
  },

  begin(ctx) {
    ctx.globalCompositeOperation = 'source-over';
  },

  paint(ctx, stroke, _prev, settings) {
    const baseWidth = sizeToPixels(settings.size, 12, 70);

    // Speed affects spread (faster = wider, more watery)
    const maxSpeed = 2.0;
    const speedNorm = Math.min(stroke.speed, maxSpeed) / maxSpeed;
    const spread = baseWidth * (1 + speedNorm * 0.5);

    // Multiple overlapping transparent blobs for watercolor effect
    const layers = 3;
    const rgb = hexToRgb(settings.color);

    ctx.save();

    for (let i = 0; i < layers; i++) {
      // Each layer: slightly different position, size, and color
      const offsetX = (Math.random() - 0.5) * spread * 0.3;
      const offsetY = (Math.random() - 0.5) * spread * 0.3;
      const layerSize = spread * (0.6 + Math.random() * 0.4);
      const layerAlpha = settings.opacity * (0.03 + Math.random() * 0.04);

      // Subtle color variation
      const vr = Math.max(0, Math.min(255, rgb.r + (Math.random() - 0.5) * 15));
      const vg = Math.max(0, Math.min(255, rgb.g + (Math.random() - 0.5) * 15));
      const vb = Math.max(0, Math.min(255, rgb.b + (Math.random() - 0.5) * 15));

      ctx.globalAlpha = layerAlpha;
      ctx.beginPath();
      ctx.ellipse(
        stroke.x + offsetX,
        stroke.y + offsetY,
        layerSize / 2,
        layerSize / 2 * (0.8 + Math.random() * 0.4),
        Math.random() * Math.PI,
        0,
        Math.PI * 2,
      );
      ctx.fillStyle = `rgb(${vr|0}, ${vg|0}, ${vb|0})`;
      ctx.fill();
    }

    ctx.restore();
  },

  end() {},
};

// ============================================================
// ERASER — draws opaque shapes on active canvas,
// engine composites with destination-out onto main canvas.
// ============================================================

export const eraserBrush: Brush = {
  type: 'eraser',
  tracksSpeed: false,
  getSpacing() { return 1; },

  begin(ctx, settings) {
    const width = sizeToPixels(settings.size, 8, 60);
    ctx.strokeStyle = '#000000';
    ctx.fillStyle = '#000000';
    ctx.lineWidth = width;
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';
    ctx.globalAlpha = 1;
    ctx.globalCompositeOperation = 'source-over'; // Draw opaque on active canvas
  },

  paint(ctx, stroke, prev) {
    if (!prev) {
      ctx.beginPath();
      ctx.arc(stroke.x, stroke.y, ctx.lineWidth / 2, 0, Math.PI * 2);
      ctx.fill();
      return;
    }
    ctx.beginPath();
    ctx.moveTo(prev.x, prev.y);
    ctx.lineTo(stroke.x, stroke.y);
    ctx.stroke();
  },

  end() {},
};

// ============================================================
// Registry
// ============================================================

export const brushes: Record<string, Brush> = {
  pencil: pencilBrush,
  marker: markerBrush,
  oil: oilBrush,
  watercolor: watercolorBrush,
  spray: sprayBrush,
  eraser: eraserBrush,
};

export function getBrush(type: string): Brush {
  return brushes[type] ?? pencilBrush;
}

export interface HSV {
  h: number; // 0-1
  s: number; // 0-1
  v: number; // 0-1
}

export interface RGB {
  r: number; // 0-255
  g: number; // 0-255
  b: number; // 0-255
}

export function hexToRgb(hex: string): RGB {
  const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
  if (!result) return { r: 0, g: 0, b: 0 };
  return {
    r: parseInt(result[1], 16),
    g: parseInt(result[2], 16),
    b: parseInt(result[3], 16),
  };
}

export function rgbToHex(r: number, g: number, b: number): string {
  return '#' + [r, g, b].map(x => Math.round(x).toString(16).padStart(2, '0')).join('');
}

export function rgbToHsv(r: number, g: number, b: number): HSV {
  r /= 255; g /= 255; b /= 255;
  const max = Math.max(r, g, b);
  const min = Math.min(r, g, b);
  const d = max - min;
  let h = 0;
  const s = max === 0 ? 0 : d / max;
  const v = max;

  if (d !== 0) {
    switch (max) {
      case r: h = ((g - b) / d + (g < b ? 6 : 0)) / 6; break;
      case g: h = ((b - r) / d + 2) / 6; break;
      case b: h = ((r - g) / d + 4) / 6; break;
    }
  }
  return { h, s, v };
}

export function hsvToRgb(h: number, s: number, v: number): RGB {
  let r = 0, g = 0, b = 0;
  const i = Math.floor(h * 6);
  const f = h * 6 - i;
  const p = v * (1 - s);
  const q = v * (1 - f * s);
  const t = v * (1 - (1 - f) * s);

  switch (i % 6) {
    case 0: r = v; g = t; b = p; break;
    case 1: r = q; g = v; b = p; break;
    case 2: r = p; g = v; b = t; break;
    case 3: r = p; g = q; b = v; break;
    case 4: r = t; g = p; b = v; break;
    case 5: r = v; g = p; b = q; break;
  }
  return { r: Math.round(r * 255), g: Math.round(g * 255), b: Math.round(b * 255) };
}

export function hsvToHex(h: number, s: number, v: number): string {
  const { r, g, b } = hsvToRgb(h, s, v);
  return rgbToHex(r, g, b);
}

function clamp01(v: number): number {
  return Math.max(0, Math.min(1, v));
}

function cycle01(v: number): number {
  if (v < 0) v += 1;
  if (v > 1) v -= 1;
  return clamp01(v);
}

/** Add random HSV deviation to a color — inspired by Velasquez oil brush */
export function addColorDeviation(
  hex: string,
  hueRange: number,
  satRange: number,
  valRange: number,
): string {
  const rgb = hexToRgb(hex);
  const hsv = rgbToHsv(rgb.r, rgb.g, rgb.b);

  const h = cycle01(hsv.h + (Math.random() * 2 - 1) * hueRange);
  const s = clamp01(hsv.s + (Math.random() * 2 - 1) * satRange);
  const v = clamp01(hsv.v + (Math.random() * 2 - 1) * valRange);

  return hsvToHex(h, s, v);
}

/** Pre-built color palettes for the picker */
export const COLOR_PALETTES = {
  vivid: [
    '#FF3B30', '#FF9500', '#FFCC00', '#34C759', '#00C7BE',
    '#30B0C7', '#007AFF', '#5856D6', '#AF52DE', '#FF2D55',
  ],
  pastel: [
    '#FFB3BA', '#FFDFBA', '#FFFFBA', '#BAFFC9', '#BAE1FF',
    '#D4BAFF', '#FFB3DE', '#C9BAFF', '#BAF2FF', '#FFDDC1',
  ],
  earth: [
    '#8B4513', '#A0522D', '#CD853F', '#DEB887', '#D2B48C',
    '#BC8F8F', '#F4A460', '#DAA520', '#B8860B', '#556B2F',
  ],
  grayscale: [
    '#000000', '#1C1C1E', '#3A3A3C', '#636366', '#8E8E93',
    '#AEAEB2', '#C7C7CC', '#D1D1D6', '#E5E5EA', '#FFFFFF',
  ],
};

export const ALL_COLORS = [
  ...COLOR_PALETTES.vivid,
  ...COLOR_PALETTES.pastel,
  ...COLOR_PALETTES.earth,
  ...COLOR_PALETTES.grayscale,
];

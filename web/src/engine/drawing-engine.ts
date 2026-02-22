import type { Point, BrushStroke, BrushSettings } from './types';
import { getBrush } from './brushes';
import { CanvasHistory } from './history';

/**
 * DrawingEngine manages a two-canvas system:
 * - mainCanvas: committed strokes (persistent drawing)
 * - activeCanvas: current in-progress stroke (overlaid, cleared on stroke end)
 *
 * This avoids re-rendering the entire drawing on every pointer move.
 */
export class DrawingEngine {
  private mainCanvas: HTMLCanvasElement;
  private activeCanvas: HTMLCanvasElement;
  private mainCtx: CanvasRenderingContext2D;
  private activeCtx: CanvasRenderingContext2D;

  private history: CanvasHistory;
  private isDrawing = false;
  private lastPoint: Point | null = null;
  private lastStroke: BrushStroke | null = null;
  private strokeIndex = 0;
  private currentSettings: BrushSettings | null = null;

  // Background color (for display behind transparent canvas)
  private bgColor = '#FFFFFF';

  // Callbacks
  onHistoryChange?: () => void;

  constructor(mainCanvas: HTMLCanvasElement, activeCanvas: HTMLCanvasElement) {
    this.mainCanvas = mainCanvas;
    this.activeCanvas = activeCanvas;
    this.mainCtx = mainCanvas.getContext('2d', { willReadFrequently: true })!;
    this.activeCtx = activeCanvas.getContext('2d')!;
    this.history = new CanvasHistory();
  }

  // ---- Canvas setup ----

  resize(width: number, height: number, dpr: number) {
    // Save current content
    const prevData = this.mainCtx.getImageData(
      0, 0, this.mainCanvas.width, this.mainCanvas.height
    );

    for (const canvas of [this.mainCanvas, this.activeCanvas]) {
      canvas.width = width * dpr;
      canvas.height = height * dpr;
      canvas.style.width = `${width}px`;
      canvas.style.height = `${height}px`;
      const ctx = canvas.getContext('2d')!;
      ctx.scale(dpr, dpr);
    }

    // Restore content (scaled back)
    if (prevData.width > 0 && prevData.height > 0) {
      const tmpCanvas = document.createElement('canvas');
      tmpCanvas.width = prevData.width;
      tmpCanvas.height = prevData.height;
      tmpCanvas.getContext('2d')!.putImageData(prevData, 0, 0);
      this.mainCtx.drawImage(
        tmpCanvas,
        0, 0, prevData.width, prevData.height,
        0, 0, width, height
      );
    }
  }

  setBackgroundColor(color: string) {
    this.bgColor = color;
  }

  getBackgroundColor(): string {
    return this.bgColor;
  }

  // ---- Drawing ----

  startStroke(point: Point, settings: BrushSettings) {
    if (this.isDrawing) return;
    this.isDrawing = true;
    this.currentSettings = { ...settings };
    this.lastPoint = point;
    this.lastStroke = null;
    this.strokeIndex = 0;

    // Save state before stroke for undo
    this.history.push(this.mainCanvas);

    const brush = getBrush(settings.type);

    // For non-speed-sensitive brushes (pencil, marker, eraser), draw on
    // active canvas for real-time feedback, then commit on end.
    // For oil/spray (stamp-based), draw directly on main canvas.
    const ctx = this.getStrokeCtx(settings.type);
    brush.begin(ctx, settings);

    // Paint initial point
    const stroke = this.pointToStroke(point, null);
    brush.paint(ctx, stroke, null, settings, 0);
    this.lastStroke = stroke;
    this.strokeIndex = 1;
  }

  continueStroke(point: Point) {
    if (!this.isDrawing || !this.currentSettings || !this.lastPoint) return;

    const settings = this.currentSettings;
    const brush = getBrush(settings.type);
    const spacing = brush.getSpacing(settings);
    const ctx = this.getStrokeCtx(settings.type);

    // Interpolate between last point and new point
    const dx = point.x - this.lastPoint.x;
    const dy = point.y - this.lastPoint.y;
    const dist = Math.sqrt(dx * dx + dy * dy);

    if (dist < spacing) {
      // Still store last point for next delta
      this.lastPoint = point;
      return;
    }

    const steps = Math.floor(dist / spacing);
    for (let i = 1; i <= steps; i++) {
      const t = (i * spacing) / dist;
      const interpPoint: Point = {
        x: this.lastPoint.x + dx * t,
        y: this.lastPoint.y + dy * t,
        pressure: this.lastPoint.pressure * (1 - t) + point.pressure * t,
        timestamp: this.lastPoint.timestamp + (point.timestamp - this.lastPoint.timestamp) * t,
      };
      const stroke = this.pointToStroke(interpPoint, this.lastPoint);
      brush.paint(ctx, stroke, this.lastStroke, settings, this.strokeIndex);
      this.lastStroke = stroke;
      this.strokeIndex++;
    }

    this.lastPoint = point;
  }

  endStroke() {
    if (!this.isDrawing || !this.currentSettings) return;
    this.isDrawing = false;

    const settings = this.currentSettings;
    const brush = getBrush(settings.type);
    const ctx = this.getStrokeCtx(settings.type);
    brush.end(ctx);

    // If we drew on the active canvas, composite it onto main
    if (this.usesActiveCanvas(settings.type)) {
      this.mainCtx.save();

      // Different compositing per brush type
      if (settings.type === 'eraser') {
        this.mainCtx.globalAlpha = 1;
        this.mainCtx.globalCompositeOperation = 'destination-out';
      } else if (settings.type === 'marker') {
        // Marker: drawn at full opacity on active, composite at target
        this.mainCtx.globalAlpha = settings.opacity * 0.5;
        this.mainCtx.globalCompositeOperation = 'source-over';
      } else {
        this.mainCtx.globalAlpha = 1;
        this.mainCtx.globalCompositeOperation = 'source-over';
      }

      // Draw the active layer content (at physical pixel scale)
      this.mainCtx.setTransform(1, 0, 0, 1, 0, 0);
      this.mainCtx.drawImage(this.activeCanvas, 0, 0);
      // Restore the DPR scaling
      const dpr = this.getDpr();
      this.mainCtx.setTransform(dpr, 0, 0, dpr, 0, 0);

      this.mainCtx.restore();
      this.clearActiveCanvas();
    }

    this.currentSettings = null;
    this.lastPoint = null;
    this.lastStroke = null;
    this.onHistoryChange?.();
  }

  cancelStroke() {
    if (!this.isDrawing) return;
    this.isDrawing = false;
    this.clearActiveCanvas();
    // Undo the snapshot we saved at startStroke
    this.history.undo(this.mainCanvas);
    this.currentSettings = null;
    this.lastPoint = null;
    this.lastStroke = null;
  }

  // ---- Clipart ----

  async drawClipart(svgPath: string, x: number, y: number, size: number): Promise<void> {
    this.history.push(this.mainCanvas);

    return new Promise((resolve, reject) => {
      const img = new Image();
      img.onload = () => {
        const aspect = img.naturalWidth / img.naturalHeight;
        const w = size;
        const h = size / aspect;
        this.mainCtx.drawImage(img, x - w / 2, y - h / 2, w, h);
        this.onHistoryChange?.();
        resolve();
      };
      img.onerror = reject;
      img.src = svgPath;
    });
  }

  // ---- Text ----

  drawText(text: string, x: number, y: number, fontSize: number, color: string) {
    this.history.push(this.mainCanvas);
    this.mainCtx.save();
    this.mainCtx.font = `${fontSize}px "Segoe UI", system-ui, sans-serif`;
    this.mainCtx.fillStyle = color;
    this.mainCtx.textAlign = 'center';
    this.mainCtx.textBaseline = 'middle';
    this.mainCtx.fillText(text, x, y);
    this.mainCtx.restore();
    this.onHistoryChange?.();
  }

  // ---- Undo / Redo ----

  undo(): boolean {
    const result = this.history.undo(this.mainCanvas);
    this.onHistoryChange?.();
    return result;
  }

  redo(): boolean {
    const result = this.history.redo(this.mainCanvas);
    this.onHistoryChange?.();
    return result;
  }

  get canUndo(): boolean { return this.history.canUndo; }
  get canRedo(): boolean { return this.history.canRedo; }

  // ---- Clear ----

  clear() {
    this.history.push(this.mainCanvas);
    this.mainCtx.clearRect(
      0, 0,
      this.mainCanvas.width / this.getDpr(),
      this.mainCanvas.height / this.getDpr()
    );
    this.clearActiveCanvas();
    this.onHistoryChange?.();
  }

  // ---- Export ----

  toDataURL(type = 'image/png', quality = 0.92): string {
    // Render to a flat canvas with background
    const w = this.mainCanvas.width;
    const h = this.mainCanvas.height;
    const exportCanvas = document.createElement('canvas');
    exportCanvas.width = w;
    exportCanvas.height = h;
    const ctx = exportCanvas.getContext('2d')!;

    // Fill background
    ctx.fillStyle = this.bgColor;
    ctx.fillRect(0, 0, w, h);

    // Draw content
    ctx.drawImage(this.mainCanvas, 0, 0);

    return exportCanvas.toDataURL(type, quality);
  }

  toBlob(type = 'image/png', quality = 0.92): Promise<Blob | null> {
    const w = this.mainCanvas.width;
    const h = this.mainCanvas.height;
    const exportCanvas = document.createElement('canvas');
    exportCanvas.width = w;
    exportCanvas.height = h;
    const ctx = exportCanvas.getContext('2d')!;
    ctx.fillStyle = this.bgColor;
    ctx.fillRect(0, 0, w, h);
    ctx.drawImage(this.mainCanvas, 0, 0);

    return new Promise(resolve => {
      exportCanvas.toBlob(blob => resolve(blob), type, quality);
    });
  }

  /** Generate a small thumbnail */
  toThumbnail(maxSize = 200): string {
    const w = this.mainCanvas.width;
    const h = this.mainCanvas.height;
    const scale = maxSize / Math.max(w, h);
    const tw = Math.round(w * scale);
    const th = Math.round(h * scale);

    const thumbCanvas = document.createElement('canvas');
    thumbCanvas.width = tw;
    thumbCanvas.height = th;
    const ctx = thumbCanvas.getContext('2d')!;
    ctx.fillStyle = this.bgColor;
    ctx.fillRect(0, 0, tw, th);
    ctx.drawImage(this.mainCanvas, 0, 0, tw, th);

    return thumbCanvas.toDataURL('image/jpeg', 0.7);
  }

  // ---- Internal helpers ----

  /** Determine whether brush draws on the active (overlay) canvas */
  private usesActiveCanvas(type: string): boolean {
    return type === 'pencil' || type === 'marker' || type === 'eraser';
  }

  private getStrokeCtx(type: string): CanvasRenderingContext2D {
    return this.usesActiveCanvas(type) ? this.activeCtx : this.mainCtx;
  }

  private clearActiveCanvas() {
    this.activeCtx.save();
    this.activeCtx.setTransform(1, 0, 0, 1, 0, 0);
    this.activeCtx.clearRect(0, 0, this.activeCanvas.width, this.activeCanvas.height);
    const dpr = this.getDpr();
    this.activeCtx.setTransform(dpr, 0, 0, dpr, 0, 0);
    this.activeCtx.restore();
  }

  private getDpr(): number {
    if (!this.mainCanvas.style.width) return 1;
    return this.mainCanvas.width / parseFloat(this.mainCanvas.style.width);
  }

  private pointToStroke(point: Point, prevPoint: Point | null): BrushStroke {
    let speed = 0;
    let angle = 0;

    if (prevPoint) {
      const dx = point.x - prevPoint.x;
      const dy = point.y - prevPoint.y;
      const dist = Math.sqrt(dx * dx + dy * dy);
      const dt = point.timestamp - prevPoint.timestamp;
      speed = dt > 0 ? dist / dt : 0; // pixels per ms
      angle = Math.atan2(dy, dx);
    }

    return {
      x: point.x,
      y: point.y,
      pressure: point.pressure,
      speed,
      angle,
    };
  }
}

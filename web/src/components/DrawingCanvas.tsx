import { useRef, useEffect, useCallback, useState } from 'react';
import { DrawingEngine } from '../engine/drawing-engine';
import { useAppStore } from '../store';
import type { Point } from '../engine/types';

interface Props {
  engineRef: React.MutableRefObject<DrawingEngine | null>;
}

/** Map 0-1 normalized size to pixel radius for cursor preview */
function brushSizeToPixels(size: number, type: string): number {
  switch (type) {
    case 'pencil': return 1 + size * 19;
    case 'marker': return 8 + size * 40;
    case 'oil': return 6 + size * 44;
    case 'watercolor': return 12 + size * 58;
    case 'spray': return 10 + size * 50;
    case 'eraser': return 8 + size * 52;
    default: return 4 + size * 16;
  }
}

export default function DrawingCanvas({ engineRef }: Props) {
  const mainCanvasRef = useRef<HTMLCanvasElement>(null);
  const activeCanvasRef = useRef<HTMLCanvasElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);

  const brushType = useAppStore(s => s.brushType);
  const brushColor = useAppStore(s => s.brushColor);
  const brushSize = useAppStore(s => s.brushSize);
  const brushOpacity = useAppStore(s => s.brushOpacity);
  const backgroundColor = useAppStore(s => s.backgroundColor);
  const setHistoryState = useAppStore(s => s.setHistoryState);

  const isPlacingClipart = useAppStore(s => s.isPlacingClipart);
  const clipartPath = useAppStore(s => s.clipartPath);
  const cancelPlacingClipart = useAppStore(s => s.cancelPlacingClipart);

  const isPlacingText = useAppStore(s => s.isPlacingText);
  const textContent = useAppStore(s => s.textContent);
  const cancelPlacingText = useAppStore(s => s.cancelPlacingText);

  // Cursor preview state
  const [cursorPos, setCursorPos] = useState<{ x: number; y: number } | null>(null);

  // Multi-touch undo tracking
  const touchCountRef = useRef(0);
  const touchStartTimeRef = useRef(0);
  const touchMovedRef = useRef(false);

  // Initialize engine
  useEffect(() => {
    const mainCanvas = mainCanvasRef.current;
    const activeCanvas = activeCanvasRef.current;
    const container = containerRef.current;
    if (!mainCanvas || !activeCanvas || !container) return;

    const engine = new DrawingEngine(mainCanvas, activeCanvas);
    engineRef.current = engine;

    const dpr = window.devicePixelRatio || 1;
    const rect = container.getBoundingClientRect();
    engine.resize(rect.width, rect.height, dpr);

    engine.onHistoryChange = () => {
      setHistoryState(engine.canUndo, engine.canRedo);
    };

    const handleResize = () => {
      const r = container.getBoundingClientRect();
      engine.resize(r.width, r.height, window.devicePixelRatio || 1);
    };

    window.addEventListener('resize', handleResize);
    return () => {
      window.removeEventListener('resize', handleResize);
    };
  }, [engineRef, setHistoryState]);

  // Sync background color
  useEffect(() => {
    engineRef.current?.setBackgroundColor(backgroundColor);
  }, [backgroundColor, engineRef]);

  // Two-finger tap to undo, three-finger tap to redo
  useEffect(() => {
    const container = containerRef.current;
    if (!container) return;

    const handleTouchStart = (e: TouchEvent) => {
      touchCountRef.current = e.touches.length;
      touchStartTimeRef.current = Date.now();
      touchMovedRef.current = false;

      // Cancel drawing if second finger added
      if (e.touches.length >= 2) {
        engineRef.current?.cancelStroke();
      }
    };

    const handleTouchMove = () => {
      touchMovedRef.current = true;
    };

    const handleTouchEnd = (e: TouchEvent) => {
      // If all fingers released, check for undo/redo gesture
      if (e.touches.length === 0 && !touchMovedRef.current) {
        const elapsed = Date.now() - touchStartTimeRef.current;
        if (elapsed < 400) {
          if (touchCountRef.current === 2) {
            engineRef.current?.undo();
          } else if (touchCountRef.current === 3) {
            engineRef.current?.redo();
          }
        }
      }
    };

    container.addEventListener('touchstart', handleTouchStart, { passive: true });
    container.addEventListener('touchmove', handleTouchMove, { passive: true });
    container.addEventListener('touchend', handleTouchEnd, { passive: true });

    return () => {
      container.removeEventListener('touchstart', handleTouchStart);
      container.removeEventListener('touchmove', handleTouchMove);
      container.removeEventListener('touchend', handleTouchEnd);
    };
  }, [engineRef]);

  // Get canvas-relative coordinates from pointer event
  const getPoint = useCallback((e: React.PointerEvent): Point => {
    const rect = containerRef.current!.getBoundingClientRect();
    return {
      x: e.clientX - rect.left,
      y: e.clientY - rect.top,
      pressure: e.pressure || 0.5,
      timestamp: e.timeStamp,
    };
  }, []);

  // Pointer handlers
  const handlePointerDown = useCallback((e: React.PointerEvent) => {
    e.preventDefault();
    (e.target as HTMLElement).setPointerCapture(e.pointerId);

    const engine = engineRef.current;
    if (!engine) return;

    const point = getPoint(e);

    // Handle clipart placement
    if (isPlacingClipart && clipartPath) {
      const size = 80 + brushSize * 200;
      engine.drawClipart(clipartPath, point.x, point.y, size);
      cancelPlacingClipart();
      return;
    }

    // Handle text placement
    if (isPlacingText && textContent) {
      const fontSize = 16 + brushSize * 48;
      engine.drawText(textContent, point.x, point.y, fontSize, brushColor);
      cancelPlacingText();
      return;
    }

    engine.startStroke(point, {
      type: brushType,
      color: brushColor,
      size: brushSize,
      opacity: brushOpacity,
    });
  }, [engineRef, getPoint, brushType, brushColor, brushSize, brushOpacity, isPlacingClipart, clipartPath, cancelPlacingClipart, isPlacingText, textContent, cancelPlacingText]);

  const handlePointerMove = useCallback((e: React.PointerEvent) => {
    e.preventDefault();
    const rect = containerRef.current!.getBoundingClientRect();
    setCursorPos({ x: e.clientX - rect.left, y: e.clientY - rect.top });
    engineRef.current?.continueStroke(getPoint(e));
  }, [engineRef, getPoint]);

  const handlePointerUp = useCallback((e: React.PointerEvent) => {
    e.preventDefault();
    engineRef.current?.endStroke();
  }, [engineRef]);

  const handlePointerLeave = useCallback(() => {
    setCursorPos(null);
  }, []);

  const cursorClass = isPlacingClipart || isPlacingText
    ? 'cursor-move'
    : 'cursor-none'; // Hide default cursor, we show custom preview

  const cursorRadius = brushSizeToPixels(brushSize, brushType);
  const showCursor = cursorPos && !isPlacingClipart && !isPlacingText;

  return (
    <div
      ref={containerRef}
      className={`w-full h-full relative overflow-hidden ${cursorClass}`}
      style={{ backgroundColor }}
    >
      <canvas
        ref={mainCanvasRef}
        className="absolute inset-0"
      />
      <canvas
        ref={activeCanvasRef}
        className="absolute inset-0"
        onPointerDown={handlePointerDown}
        onPointerMove={handlePointerMove}
        onPointerUp={handlePointerUp}
        onPointerCancel={handlePointerUp}
        onPointerLeave={handlePointerLeave}
      />

      {/* Brush cursor preview */}
      {showCursor && (
        <div
          className="absolute pointer-events-none"
          style={{
            left: cursorPos.x - cursorRadius,
            top: cursorPos.y - cursorRadius,
            width: cursorRadius * 2,
            height: cursorRadius * 2,
            borderRadius: '50%',
            border: brushType === 'eraser'
              ? '2px dashed rgba(0,0,0,0.4)'
              : '1.5px solid rgba(0,0,0,0.3)',
            backgroundColor: brushType === 'eraser'
              ? 'rgba(255,255,255,0.2)'
              : undefined,
          }}
        />
      )}

      {/* Placement indicator */}
      {(isPlacingClipart || isPlacingText) && (
        <div className="absolute top-3 left-1/2 -translate-x-1/2 bg-black/70 text-white px-4 py-2 rounded-full text-sm animate-fade-in">
          Tap canvas to place {isPlacingClipart ? 'clipart' : 'text'}
        </div>
      )}
    </div>
  );
}

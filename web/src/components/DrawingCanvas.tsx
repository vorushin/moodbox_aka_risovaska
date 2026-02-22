import { useRef, useEffect, useCallback } from 'react';
import { DrawingEngine } from '../engine/drawing-engine';
import { useAppStore } from '../store';
import type { Point } from '../engine/types';

interface Props {
  engineRef: React.MutableRefObject<DrawingEngine | null>;
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
    engineRef.current?.continueStroke(getPoint(e));
  }, [engineRef, getPoint]);

  const handlePointerUp = useCallback((e: React.PointerEvent) => {
    e.preventDefault();
    engineRef.current?.endStroke();
  }, [engineRef]);

  const cursorClass = isPlacingClipart || isPlacingText
    ? 'cursor-move'
    : brushType === 'eraser'
      ? 'cursor-erase'
      : 'cursor-draw';

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
      />
      {/* Placement indicator */}
      {(isPlacingClipart || isPlacingText) && (
        <div className="absolute top-3 left-1/2 -translate-x-1/2 bg-black/70 text-white px-4 py-2 rounded-full text-sm animate-fade-in">
          Tap canvas to place {isPlacingClipart ? 'clipart' : 'text'}
        </div>
      )}
    </div>
  );
}

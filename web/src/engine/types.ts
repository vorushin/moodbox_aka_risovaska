export interface Point {
  x: number;
  y: number;
  pressure: number;
  timestamp: number;
}

export interface BrushStroke {
  x: number;
  y: number;
  pressure: number;
  speed: number;
  angle: number;
}

export type BrushType = 'pencil' | 'marker' | 'oil' | 'spray' | 'eraser';

export interface BrushSettings {
  type: BrushType;
  color: string;
  size: number; // 0-1 normalized
  opacity: number; // 0-1
}

export interface CanvasState {
  imageData: ImageData;
  width: number;
  height: number;
}

export interface Drawing {
  id: string;
  name: string;
  author: string;
  dataUrl: string;
  thumbnail: string;
  createdAt: number;
  width: number;
  height: number;
}

export interface ClipartCategory {
  name: string;
  icon: string;
  items: ClipartItem[];
}

export interface ClipartItem {
  id: string;
  path: string;
  category: string;
}

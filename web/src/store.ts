import { create } from 'zustand';
import type { BrushType } from './engine/types';

type ActivePanel = 'none' | 'colors' | 'brushes' | 'clipart' | 'gallery' | 'export' | 'text' | 'size' | 'background';

interface AppState {
  // User
  username: string;
  setUsername: (name: string) => void;

  // Drawing tool state
  brushType: BrushType;
  brushColor: string;
  brushSize: number;     // 0-1 normalized
  brushOpacity: number;  // 0-1
  backgroundColor: string;

  setBrushType: (type: BrushType) => void;
  setBrushColor: (color: string) => void;
  setBrushSize: (size: number) => void;
  setBrushOpacity: (opacity: number) => void;
  setBackgroundColor: (color: string) => void;

  // UI state
  activePanel: ActivePanel;
  setActivePanel: (panel: ActivePanel) => void;
  togglePanel: (panel: ActivePanel) => void;

  // History state (mirror of engine state)
  canUndo: boolean;
  canRedo: boolean;
  setHistoryState: (canUndo: boolean, canRedo: boolean) => void;

  // Drawing mode
  isPlacingClipart: boolean;
  clipartPath: string | null;
  startPlacingClipart: (path: string) => void;
  cancelPlacingClipart: () => void;

  isPlacingText: boolean;
  textContent: string;
  startPlacingText: (text: string) => void;
  cancelPlacingText: () => void;
}

export const useAppStore = create<AppState>((set) => ({
  // User
  username: '',
  setUsername: (name) => set({ username: name }),

  // Drawing defaults
  brushType: 'pencil',
  brushColor: '#2C2C2C',
  brushSize: 0.3,
  brushOpacity: 1,
  backgroundColor: '#FFFFFF',

  setBrushType: (type) => set({ brushType: type, activePanel: 'none' }),
  setBrushColor: (color) => set({ brushColor: color }),
  setBrushSize: (size) => set({ brushSize: size }),
  setBrushOpacity: (opacity) => set({ brushOpacity: opacity }),
  setBackgroundColor: (color) => set({ backgroundColor: color }),

  // UI
  activePanel: 'none',
  setActivePanel: (panel) => set({ activePanel: panel }),
  togglePanel: (panel) => set((s) => ({
    activePanel: s.activePanel === panel ? 'none' : panel,
  })),

  // History
  canUndo: false,
  canRedo: false,
  setHistoryState: (canUndo, canRedo) => set({ canUndo, canRedo }),

  // Clipart placement
  isPlacingClipart: false,
  clipartPath: null,
  startPlacingClipart: (path) => set({
    isPlacingClipart: true,
    clipartPath: path,
    activePanel: 'none',
  }),
  cancelPlacingClipart: () => set({ isPlacingClipart: false, clipartPath: null }),

  // Text placement
  isPlacingText: false,
  textContent: '',
  startPlacingText: (text) => set({
    isPlacingText: true,
    textContent: text,
    activePanel: 'none',
  }),
  cancelPlacingText: () => set({ isPlacingText: false, textContent: '' }),
}));

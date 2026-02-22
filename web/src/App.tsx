import { useRef, useEffect, useState } from 'react';
import { useAppStore } from './store';
import { getUsername } from './utils/gallery-db';
import type { DrawingEngine } from './engine/drawing-engine';

import WelcomeScreen from './components/WelcomeScreen';
import DrawingCanvas from './components/DrawingCanvas';
import TopBar from './components/TopBar';
import Toolbar from './components/Toolbar';
import ColorPicker from './components/ColorPicker';
import SizePicker from './components/SizePicker';
import ClipartPanel from './components/ClipartPanel';
import TextPanel from './components/TextPanel';
import Gallery from './components/Gallery';
import ExportPanel from './components/ExportPanel';

export default function App() {
  const username = useAppStore(s => s.username);
  const setUsername = useAppStore(s => s.setUsername);
  const [loading, setLoading] = useState(true);
  const engineRef = useRef<DrawingEngine | null>(null);

  // Check for saved username on mount
  useEffect(() => {
    getUsername().then(name => {
      if (name) setUsername(name);
      setLoading(false);
    });
  }, [setUsername]);

  // Global keyboard shortcuts
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.metaKey || e.ctrlKey) {
        if (e.key === 'z' && !e.shiftKey) {
          e.preventDefault();
          engineRef.current?.undo();
        } else if ((e.key === 'z' && e.shiftKey) || e.key === 'y') {
          e.preventDefault();
          engineRef.current?.redo();
        }
      }
    };
    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, []);

  if (loading) {
    return (
      <div className="flex items-center justify-center h-full bg-gradient-to-br from-indigo-500 via-purple-500 to-pink-400">
        <div className="text-white text-xl animate-pulse">Loading...</div>
      </div>
    );
  }

  if (!username) {
    return <WelcomeScreen />;
  }

  return (
    <div className="flex flex-col h-full bg-gray-900">
      <TopBar engineRef={engineRef} />
      <DrawingCanvas engineRef={engineRef} />

      {/* Panels (slide up from bottom) */}
      <ColorPicker />
      <SizePicker />
      <ClipartPanel />
      <TextPanel />
      <ExportPanel engineRef={engineRef} />

      <Toolbar />

      {/* Full-screen overlays */}
      <Gallery />
    </div>
  );
}

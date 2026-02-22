import { useState } from 'react';
import { useAppStore } from '../store';

export default function TextPanel() {
  const activePanel = useAppStore(s => s.activePanel);
  const startPlacingText = useAppStore(s => s.startPlacingText);
  const brushColor = useAppStore(s => s.brushColor);
  const [text, setText] = useState('');

  if (activePanel !== 'text') return null;

  const handlePlace = () => {
    const trimmed = text.trim();
    if (!trimmed) return;
    startPlacingText(trimmed);
    setText('');
  };

  return (
    <div className="bg-panel-bg border-t border-white/10 p-4 animate-fade-in">
      <div className="flex gap-2">
        <input
          type="text"
          value={text}
          onChange={e => setText(e.target.value)}
          onKeyDown={e => e.key === 'Enter' && handlePlace()}
          placeholder="Type your text..."
          maxLength={100}
          autoFocus
          className="flex-1 px-4 py-3 rounded-xl bg-white/10 text-white placeholder-white/30 border border-white/10 focus:border-toolbar-active focus:outline-none"
          style={{ color: brushColor }}
        />
        <button
          onClick={handlePlace}
          disabled={!text.trim()}
          className="px-5 py-3 rounded-xl bg-toolbar-active text-white font-medium disabled:opacity-40 active:scale-95 transition-transform"
        >
          Place
        </button>
      </div>
      <p className="text-xs text-white/30 mt-2">
        Text will use the current brush color. Tap on the canvas to place it.
      </p>
    </div>
  );
}

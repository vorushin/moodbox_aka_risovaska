import { useState } from 'react';
import { useAppStore } from '../store';
import { ALL_COLORS, COLOR_PALETTES } from '../engine/color-utils';

const PALETTE_NAMES: { key: keyof typeof COLOR_PALETTES; label: string }[] = [
  { key: 'vivid', label: 'Vivid' },
  { key: 'pastel', label: 'Pastel' },
  { key: 'earth', label: 'Earth' },
  { key: 'grayscale', label: 'Gray' },
];

const BG_COLORS = [
  '#FFFFFF', '#FFF8E7', '#F0F4F8', '#1A1A2E', '#FFF0F5',
  '#F0FFF0', '#FFFACD', '#E6E6FA', '#F5F5DC', '#2D2D2D',
];

export default function ColorPicker() {
  const activePanel = useAppStore(s => s.activePanel);
  const brushColor = useAppStore(s => s.brushColor);
  const backgroundColor = useAppStore(s => s.backgroundColor);
  const setBrushColor = useAppStore(s => s.setBrushColor);
  const setBackgroundColor = useAppStore(s => s.setBackgroundColor);
  const [tab, setTab] = useState<'brush' | 'bg'>('brush');

  if (activePanel !== 'colors') return null;

  const currentColors = tab === 'brush' ? ALL_COLORS : BG_COLORS;
  const currentSelected = tab === 'brush' ? brushColor : backgroundColor;
  const onSelect = tab === 'brush' ? setBrushColor : setBackgroundColor;

  return (
    <div className="bg-panel-bg border-t border-white/10 p-4 animate-fade-in">
      {/* Tab switcher */}
      <div className="flex gap-2 mb-3">
        <TabButton active={tab === 'brush'} onClick={() => setTab('brush')}>
          Brush Color
        </TabButton>
        <TabButton active={tab === 'bg'} onClick={() => setTab('bg')}>
          Background
        </TabButton>
      </div>

      {tab === 'brush' ? (
        <>
          {/* Palette sections */}
          {PALETTE_NAMES.map(palette => (
            <div key={palette.key} className="mb-3">
              <div className="text-xs text-white/40 mb-1">{palette.label}</div>
              <div className="flex gap-2 flex-wrap">
                {COLOR_PALETTES[palette.key].map(color => (
                  <ColorSwatch
                    key={color}
                    color={color}
                    selected={brushColor === color}
                    onSelect={() => setBrushColor(color)}
                  />
                ))}
              </div>
            </div>
          ))}
        </>
      ) : (
        <div className="flex gap-2 flex-wrap">
          {currentColors.map(color => (
            <ColorSwatch
              key={color}
              color={color}
              selected={currentSelected === color}
              onSelect={() => onSelect(color)}
              large
            />
          ))}
        </div>
      )}

      {/* Custom color input */}
      <div className="mt-3 flex items-center gap-2">
        <input
          type="color"
          value={currentSelected}
          onChange={e => onSelect(e.target.value)}
          className="w-10 h-10 rounded-lg border-0 cursor-pointer"
        />
        <span className="text-xs text-white/50 uppercase">{currentSelected}</span>
      </div>
    </div>
  );
}

function ColorSwatch({
  color,
  selected,
  onSelect,
  large,
}: {
  color: string;
  selected: boolean;
  onSelect: () => void;
  large?: boolean;
}) {
  const size = large ? 'w-10 h-10' : 'w-8 h-8';
  return (
    <button
      onClick={onSelect}
      className={`${size} rounded-full border-2 transition-transform active:scale-90 ${
        selected ? 'border-white scale-110 shadow-lg' : 'border-transparent hover:scale-105'
      }`}
      style={{ backgroundColor: color }}
      aria-label={`Color ${color}`}
    />
  );
}

function TabButton({
  active,
  onClick,
  children,
}: {
  active: boolean;
  onClick: () => void;
  children: React.ReactNode;
}) {
  return (
    <button
      onClick={onClick}
      className={`px-3 py-1 rounded-lg text-sm transition-colors ${
        active
          ? 'bg-toolbar-active text-white'
          : 'bg-white/5 text-white/50 hover:bg-white/10'
      }`}
    >
      {children}
    </button>
  );
}

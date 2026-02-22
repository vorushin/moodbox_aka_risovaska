import { useAppStore } from '../store';

export default function SizePicker() {
  const activePanel = useAppStore(s => s.activePanel);
  const brushSize = useAppStore(s => s.brushSize);
  const brushOpacity = useAppStore(s => s.brushOpacity);
  const brushColor = useAppStore(s => s.brushColor);
  const setBrushSize = useAppStore(s => s.setBrushSize);
  const setBrushOpacity = useAppStore(s => s.setBrushOpacity);

  if (activePanel !== 'size') return null;

  const previewSize = 4 + brushSize * 46;

  return (
    <div className="bg-panel-bg border-t border-white/10 p-4 animate-fade-in">
      {/* Size preview */}
      <div className="flex items-center justify-center mb-4 h-16">
        <div
          className="rounded-full transition-all"
          style={{
            width: previewSize,
            height: previewSize,
            backgroundColor: brushColor,
            opacity: brushOpacity,
          }}
        />
      </div>

      {/* Size slider */}
      <div className="mb-4">
        <div className="flex justify-between text-xs text-white/50 mb-1">
          <span>Size</span>
          <span>{Math.round(brushSize * 100)}%</span>
        </div>
        <input
          type="range"
          min="0"
          max="1"
          step="0.01"
          value={brushSize}
          onChange={e => setBrushSize(parseFloat(e.target.value))}
          className="w-full accent-toolbar-active"
        />
      </div>

      {/* Opacity slider */}
      <div>
        <div className="flex justify-between text-xs text-white/50 mb-1">
          <span>Opacity</span>
          <span>{Math.round(brushOpacity * 100)}%</span>
        </div>
        <input
          type="range"
          min="0.05"
          max="1"
          step="0.01"
          value={brushOpacity}
          onChange={e => setBrushOpacity(parseFloat(e.target.value))}
          className="w-full accent-toolbar-active"
        />
      </div>

      {/* Quick presets */}
      <div className="flex gap-2 mt-4">
        {[0.1, 0.25, 0.5, 0.75, 1.0].map(size => (
          <button
            key={size}
            onClick={() => setBrushSize(size)}
            className={`flex-1 py-2 rounded-lg text-xs transition-colors ${
              Math.abs(brushSize - size) < 0.05
                ? 'bg-toolbar-active text-white'
                : 'bg-white/5 text-white/50 hover:bg-white/10'
            }`}
          >
            {size === 0.1 ? 'XS' : size === 0.25 ? 'S' : size === 0.5 ? 'M' : size === 0.75 ? 'L' : 'XL'}
          </button>
        ))}
      </div>
    </div>
  );
}

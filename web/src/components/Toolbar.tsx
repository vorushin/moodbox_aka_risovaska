import {
  IoPencil,
  IoBrush,
  IoSparkles,
  IoWater,
  IoText,
  IoImages,
  IoResize,
} from 'react-icons/io5';
import { TbSpray, TbEraser } from 'react-icons/tb';
import { useAppStore } from '../store';
import type { BrushType } from '../engine/types';

const BRUSH_TOOLS: { type: BrushType; icon: React.ReactNode; label: string }[] = [
  { type: 'pencil', icon: <IoPencil size={22} />, label: 'Pencil' },
  { type: 'marker', icon: <IoBrush size={22} />, label: 'Marker' },
  { type: 'oil', icon: <IoSparkles size={22} />, label: 'Oil Brush' },
  { type: 'watercolor', icon: <IoWater size={22} />, label: 'Watercolor' },
  { type: 'spray', icon: <TbSpray size={22} />, label: 'Spray' },
  { type: 'eraser', icon: <TbEraser size={22} />, label: 'Eraser' },
];

function getBrushIcon(type: BrushType) {
  return BRUSH_TOOLS.find(t => t.type === type)?.icon ?? <IoPencil size={22} />;
}

export default function Toolbar() {
  const brushType = useAppStore(s => s.brushType);
  const brushColor = useAppStore(s => s.brushColor);
  const setBrushType = useAppStore(s => s.setBrushType);
  const activePanel = useAppStore(s => s.activePanel);
  const togglePanel = useAppStore(s => s.togglePanel);
  const setActivePanel = useAppStore(s => s.setActivePanel);

  return (
    <div className="bg-toolbar-bg border-t border-white/5">
      {/* Brush picker sub-panel */}
      {activePanel === 'brushes' && (
        <div className="bg-panel-bg border-t border-white/10 px-3 py-3 animate-fade-in">
          <div className="flex justify-center gap-2 max-w-sm mx-auto">
            {BRUSH_TOOLS.map(tool => (
              <button
                key={tool.type}
                onClick={() => {
                  setBrushType(tool.type);
                  setActivePanel('none');
                }}
                className={`flex flex-col items-center gap-1 px-3 py-2 rounded-xl transition-all ${
                  brushType === tool.type
                    ? 'bg-toolbar-active text-white'
                    : 'text-toolbar-text hover:bg-white/10 active:bg-white/20'
                }`}
              >
                {tool.icon}
                <span className="text-[10px]">{tool.label}</span>
              </button>
            ))}
          </div>
        </div>
      )}

      {/* Main toolbar */}
      <div className="flex items-center justify-around px-4 py-2 max-w-md mx-auto">
        {/* Current brush (tap to switch) */}
        <ToolButton
          icon={getBrushIcon(brushType)}
          label="Brush"
          active={activePanel === 'brushes'}
          onClick={() => togglePanel('brushes')}
        />

        {/* Color swatch */}
        <button
          onClick={() => togglePanel('colors')}
          aria-label="Color"
          className={`p-2 rounded-xl transition-all ${
            activePanel === 'colors' ? 'bg-toolbar-active scale-110' : 'hover:bg-white/10 active:bg-white/20'
          }`}
        >
          <div
            className="w-6 h-6 rounded-full border-2 border-white/40"
            style={{ backgroundColor: brushColor }}
          />
        </button>

        {/* Size & opacity */}
        <ToolButton
          icon={<IoResize size={22} />}
          label="Size"
          active={activePanel === 'size'}
          onClick={() => togglePanel('size')}
        />

        {/* Clipart */}
        <ToolButton
          icon={<IoImages size={22} />}
          label="Stickers"
          active={activePanel === 'clipart'}
          onClick={() => togglePanel('clipart')}
        />

        {/* Text */}
        <ToolButton
          icon={<IoText size={22} />}
          label="Text"
          active={activePanel === 'text'}
          onClick={() => togglePanel('text')}
        />
      </div>
    </div>
  );
}

function ToolButton({
  icon,
  label,
  active,
  onClick,
}: {
  icon: React.ReactNode;
  label: string;
  active: boolean;
  onClick: () => void;
}) {
  return (
    <button
      onClick={onClick}
      aria-label={label}
      className={`flex flex-col items-center gap-0.5 p-2 rounded-xl transition-all ${
        active
          ? 'bg-toolbar-active text-white scale-105'
          : 'text-toolbar-text hover:bg-white/10 active:bg-white/20'
      }`}
    >
      {icon}
      <span className="text-[10px] opacity-60">{label}</span>
    </button>
  );
}

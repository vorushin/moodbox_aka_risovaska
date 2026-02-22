import {
  IoPencil,
  IoColorPalette,
  IoBrush,
  IoSparkles,
  IoText,
  IoImages,
} from 'react-icons/io5';
import { TbSpray, TbEraser } from 'react-icons/tb';
import { useAppStore } from '../store';
import type { BrushType } from '../engine/types';

const BRUSH_TOOLS: { type: BrushType; icon: React.ReactNode; label: string }[] = [
  { type: 'pencil', icon: <IoPencil size={22} />, label: 'Pencil' },
  { type: 'marker', icon: <IoBrush size={22} />, label: 'Marker' },
  { type: 'oil', icon: <IoSparkles size={22} />, label: 'Oil Brush' },
  { type: 'spray', icon: <TbSpray size={22} />, label: 'Spray' },
  { type: 'eraser', icon: <TbEraser size={22} />, label: 'Eraser' },
];

export default function Toolbar() {
  const brushType = useAppStore(s => s.brushType);
  const brushColor = useAppStore(s => s.brushColor);
  const setBrushType = useAppStore(s => s.setBrushType);
  const activePanel = useAppStore(s => s.activePanel);
  const togglePanel = useAppStore(s => s.togglePanel);

  return (
    <div className="bg-toolbar-bg border-t border-white/5 safe-area-bottom">
      <div className="flex items-center justify-around px-2 py-2 max-w-lg mx-auto">
        {/* Brush tools */}
        {BRUSH_TOOLS.map(tool => (
          <ToolButton
            key={tool.type}
            icon={tool.icon}
            label={tool.label}
            active={brushType === tool.type && activePanel === 'none'}
            onClick={() => setBrushType(tool.type)}
          />
        ))}

        {/* Separator */}
        <div className="w-px h-8 bg-white/10" />

        {/* Color */}
        <button
          onClick={() => togglePanel('colors')}
          aria-label="Color picker"
          className={`p-2 rounded-xl transition-all ${
            activePanel === 'colors' ? 'bg-toolbar-active scale-110' : 'hover:bg-white/10 active:bg-white/20'
          }`}
        >
          <div
            className="w-6 h-6 rounded-full border-2 border-white/40"
            style={{ backgroundColor: brushColor }}
          />
        </button>

        {/* Size */}
        <ToolButton
          icon={<IoColorPalette size={22} />}
          label="Size & Opacity"
          active={activePanel === 'size'}
          onClick={() => togglePanel('size')}
        />

        {/* Clipart */}
        <ToolButton
          icon={<IoImages size={22} />}
          label="Clipart"
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
      className={`p-2 rounded-xl transition-all ${
        active
          ? 'bg-toolbar-active text-white scale-110'
          : 'text-toolbar-text hover:bg-white/10 active:bg-white/20'
      }`}
    >
      {icon}
    </button>
  );
}

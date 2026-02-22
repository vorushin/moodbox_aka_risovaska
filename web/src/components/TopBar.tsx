import {
  IoArrowUndo,
  IoArrowRedo,
  IoTrashOutline,
  IoImagesOutline,
  IoShareOutline,
} from 'react-icons/io5';
import { useAppStore } from '../store';
import type { DrawingEngine } from '../engine/drawing-engine';

interface Props {
  engineRef: React.MutableRefObject<DrawingEngine | null>;
}

export default function TopBar({ engineRef }: Props) {
  const canUndo = useAppStore(s => s.canUndo);
  const canRedo = useAppStore(s => s.canRedo);
  const togglePanel = useAppStore(s => s.togglePanel);
  const activePanel = useAppStore(s => s.activePanel);
  const username = useAppStore(s => s.username);

  const handleUndo = () => engineRef.current?.undo();
  const handleRedo = () => engineRef.current?.redo();
  const handleClear = () => {
    if (confirm('Clear the canvas?')) {
      engineRef.current?.clear();
    }
  };

  return (
    <div className="flex items-center justify-between px-3 py-2 bg-toolbar-bg text-toolbar-text safe-area-top">
      {/* Left: undo/redo */}
      <div className="flex items-center gap-1">
        <BarButton
          icon={<IoArrowUndo size={20} />}
          onClick={handleUndo}
          disabled={!canUndo}
          label="Undo"
        />
        <BarButton
          icon={<IoArrowRedo size={20} />}
          onClick={handleRedo}
          disabled={!canRedo}
          label="Redo"
        />
        <BarButton
          icon={<IoTrashOutline size={20} />}
          onClick={handleClear}
          label="Clear"
        />
      </div>

      {/* Center: app name */}
      <div className="text-sm font-semibold opacity-60 truncate max-w-[120px]">
        {username}
      </div>

      {/* Right: gallery & export */}
      <div className="flex items-center gap-1">
        <BarButton
          icon={<IoImagesOutline size={20} />}
          onClick={() => togglePanel('gallery')}
          active={activePanel === 'gallery'}
          label="Gallery"
        />
        <BarButton
          icon={<IoShareOutline size={20} />}
          onClick={() => togglePanel('export')}
          active={activePanel === 'export'}
          label="Export"
        />
      </div>
    </div>
  );
}

function BarButton({
  icon,
  onClick,
  disabled,
  active,
  label,
}: {
  icon: React.ReactNode;
  onClick: () => void;
  disabled?: boolean;
  active?: boolean;
  label: string;
}) {
  return (
    <button
      onClick={onClick}
      disabled={disabled}
      aria-label={label}
      className={`p-2 rounded-lg transition-colors ${
        active
          ? 'bg-toolbar-active text-white'
          : 'hover:bg-white/10 active:bg-white/20'
      } ${disabled ? 'opacity-30 pointer-events-none' : ''}`}
    >
      {icon}
    </button>
  );
}

import { useState } from 'react';
import { IoDownloadOutline, IoShareSocialOutline, IoSaveOutline } from 'react-icons/io5';
import { useAppStore } from '../store';
import { saveDrawing, generateId } from '../utils/gallery-db';
import type { DrawingEngine } from '../engine/drawing-engine';

interface Props {
  engineRef: React.MutableRefObject<DrawingEngine | null>;
}

export default function ExportPanel({ engineRef }: Props) {
  const activePanel = useAppStore(s => s.activePanel);
  const setActivePanel = useAppStore(s => s.setActivePanel);
  const username = useAppStore(s => s.username);
  const [saving, setSaving] = useState(false);
  const [saved, setSaved] = useState(false);
  const [drawingName, setDrawingName] = useState('');

  if (activePanel !== 'export') return null;

  const handleSaveToGallery = async () => {
    const engine = engineRef.current;
    if (!engine || saving) return;

    setSaving(true);
    try {
      const dataUrl = engine.toDataURL();
      const thumbnail = engine.toThumbnail(200);

      await saveDrawing({
        id: generateId(),
        name: drawingName || `Drawing by ${username}`,
        author: username,
        dataUrl,
        thumbnail,
        createdAt: Date.now(),
        width: 0,
        height: 0,
      });

      setSaved(true);
      setTimeout(() => setSaved(false), 2000);
    } finally {
      setSaving(false);
    }
  };

  const handleDownload = async () => {
    const engine = engineRef.current;
    if (!engine) return;

    const blob = await engine.toBlob();
    if (!blob) return;

    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `${drawingName || 'risovaska'}-${Date.now()}.png`;
    a.click();
    URL.revokeObjectURL(url);
  };

  const handleShare = async () => {
    const engine = engineRef.current;
    if (!engine) return;

    const blob = await engine.toBlob();
    if (!blob) return;

    const file = new File([blob], 'drawing.png', { type: 'image/png' });

    if (navigator.share) {
      try {
        await navigator.share({
          title: drawingName || 'My Drawing',
          text: `Check out my drawing on Risovaska!`,
          files: [file],
        });
      } catch {
        // User cancelled share
      }
    } else {
      // Fallback: download
      handleDownload();
    }
  };

  return (
    <div className="bg-panel-bg border-t border-white/10 p-4 animate-fade-in">
      {/* Drawing name */}
      <input
        type="text"
        value={drawingName}
        onChange={e => setDrawingName(e.target.value)}
        placeholder="Name your drawing..."
        maxLength={50}
        className="w-full px-4 py-2 rounded-xl bg-white/10 text-white placeholder-white/30 border border-white/10 focus:border-toolbar-active focus:outline-none mb-3 text-sm"
      />

      {/* Action buttons */}
      <div className="flex gap-2">
        <ExportButton
          icon={<IoSaveOutline size={20} />}
          label={saved ? 'Saved!' : saving ? 'Saving...' : 'Save to Gallery'}
          onClick={handleSaveToGallery}
          disabled={saving}
          primary={!saved}
          success={saved}
        />
        <ExportButton
          icon={<IoDownloadOutline size={20} />}
          label="Download"
          onClick={handleDownload}
        />
        {typeof navigator.share === 'function' && (
          <ExportButton
            icon={<IoShareSocialOutline size={20} />}
            label="Share"
            onClick={handleShare}
          />
        )}
      </div>
    </div>
  );
}

function ExportButton({
  icon,
  label,
  onClick,
  disabled,
  primary,
  success,
}: {
  icon: React.ReactNode;
  label: string;
  onClick: () => void;
  disabled?: boolean;
  primary?: boolean;
  success?: boolean;
}) {
  return (
    <button
      onClick={onClick}
      disabled={disabled}
      className={`flex-1 flex flex-col items-center gap-1 py-3 rounded-xl text-xs font-medium transition-all active:scale-95 ${
        success
          ? 'bg-green-500/20 text-green-400'
          : primary
            ? 'bg-toolbar-active text-white'
            : 'bg-white/5 text-white/70 hover:bg-white/10'
      } ${disabled ? 'opacity-50 pointer-events-none' : ''}`}
    >
      {icon}
      {label}
    </button>
  );
}

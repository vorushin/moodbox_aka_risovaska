import { useState, useEffect } from 'react';
import { useAppStore } from '../store';
import { getAllDrawings, deleteDrawing } from '../utils/gallery-db';
import { IoTrashOutline, IoClose } from 'react-icons/io5';
import type { Drawing } from '../engine/types';

export default function Gallery() {
  const activePanel = useAppStore(s => s.activePanel);
  const setActivePanel = useAppStore(s => s.setActivePanel);
  const [drawings, setDrawings] = useState<Drawing[]>([]);
  const [selectedDrawing, setSelectedDrawing] = useState<Drawing | null>(null);

  useEffect(() => {
    if (activePanel === 'gallery') {
      getAllDrawings().then(setDrawings);
    }
  }, [activePanel]);

  if (activePanel !== 'gallery') return null;

  const handleDelete = async (id: string) => {
    await deleteDrawing(id);
    setDrawings(prev => prev.filter(d => d.id !== id));
    setSelectedDrawing(null);
  };

  return (
    <div className="absolute inset-0 z-50 bg-panel-bg/95 backdrop-blur-lg flex flex-col animate-fade-in">
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3 border-b border-white/10">
        <h2 className="text-white font-semibold text-lg">My Drawings</h2>
        <button
          onClick={() => setActivePanel('none')}
          className="p-2 rounded-lg text-white/60 hover:bg-white/10"
        >
          <IoClose size={24} />
        </button>
      </div>

      {/* Grid */}
      <div className="flex-1 overflow-y-auto p-4 panel-scroll">
        {drawings.length === 0 ? (
          <div className="text-center text-white/30 mt-20">
            <div className="text-5xl mb-3">🎨</div>
            <p>No drawings yet!</p>
            <p className="text-sm mt-1">Your saved drawings will appear here.</p>
          </div>
        ) : (
          <div className="grid grid-cols-2 sm:grid-cols-3 gap-3">
            {drawings.map(d => (
              <button
                key={d.id}
                onClick={() => setSelectedDrawing(d)}
                className="aspect-[4/3] rounded-xl overflow-hidden bg-white/5 hover:bg-white/10 transition-colors relative group"
              >
                <img
                  src={d.thumbnail}
                  alt={d.name || 'Drawing'}
                  className="w-full h-full object-cover"
                />
                <div className="absolute bottom-0 left-0 right-0 bg-gradient-to-t from-black/60 to-transparent p-2">
                  <div className="text-white text-xs truncate">{d.name || 'Untitled'}</div>
                  <div className="text-white/50 text-[10px]">
                    {new Date(d.createdAt).toLocaleDateString()}
                  </div>
                </div>
              </button>
            ))}
          </div>
        )}
      </div>

      {/* Full view overlay */}
      {selectedDrawing && (
        <div className="absolute inset-0 z-60 bg-black/90 flex flex-col animate-scale-in">
          <div className="flex items-center justify-between px-4 py-3">
            <div className="text-white text-sm">{selectedDrawing.name || 'Untitled'}</div>
            <div className="flex gap-2">
              <button
                onClick={() => handleDelete(selectedDrawing.id)}
                className="p-2 rounded-lg text-red-400 hover:bg-red-400/20"
              >
                <IoTrashOutline size={20} />
              </button>
              <button
                onClick={() => setSelectedDrawing(null)}
                className="p-2 rounded-lg text-white/60 hover:bg-white/10"
              >
                <IoClose size={24} />
              </button>
            </div>
          </div>
          <div className="flex-1 flex items-center justify-center p-4">
            <img
              src={selectedDrawing.dataUrl}
              alt={selectedDrawing.name || 'Drawing'}
              className="max-w-full max-h-full object-contain rounded-lg"
            />
          </div>
        </div>
      )}
    </div>
  );
}

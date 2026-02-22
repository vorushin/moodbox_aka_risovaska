import { useState, useEffect } from 'react';
import { useAppStore } from '../store';
import { getClipartCategories } from '../utils/clipart-data';
import type { ClipartCategory } from '../engine/types';

export default function ClipartPanel() {
  const activePanel = useAppStore(s => s.activePanel);
  const startPlacingClipart = useAppStore(s => s.startPlacingClipart);
  const [categories, setCategories] = useState<ClipartCategory[]>([]);
  const [selectedCat, setSelectedCat] = useState(0);

  useEffect(() => {
    setCategories(getClipartCategories());
  }, []);

  if (activePanel !== 'clipart' || categories.length === 0) return null;

  const cat = categories[selectedCat];

  return (
    <div className="bg-panel-bg border-t border-white/10 animate-fade-in max-h-[50vh] flex flex-col">
      {/* Category tabs */}
      <div className="flex gap-1 px-3 pt-3 pb-2 overflow-x-auto shrink-0">
        {categories.map((c, i) => (
          <button
            key={c.name}
            onClick={() => setSelectedCat(i)}
            className={`px-3 py-1.5 rounded-lg text-sm whitespace-nowrap transition-colors ${
              selectedCat === i
                ? 'bg-toolbar-active text-white'
                : 'bg-white/5 text-white/50 hover:bg-white/10'
            }`}
          >
            {c.icon} {c.name}
          </button>
        ))}
      </div>

      {/* Clipart grid */}
      <div className="flex-1 overflow-y-auto p-3 panel-scroll">
        <div className="grid grid-cols-4 sm:grid-cols-6 gap-2">
          {cat.items.map(item => (
            <button
              key={item.id}
              onClick={() => startPlacingClipart(item.path)}
              className="aspect-square bg-white/5 rounded-xl p-2 hover:bg-white/10 active:bg-white/20 transition-colors flex items-center justify-center"
            >
              <img
                src={item.path}
                alt={item.id}
                className="w-full h-full object-contain"
                loading="lazy"
              />
            </button>
          ))}
        </div>
      </div>
    </div>
  );
}

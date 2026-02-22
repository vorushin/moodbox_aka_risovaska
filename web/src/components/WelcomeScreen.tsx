import { useState } from 'react';
import { useAppStore } from '../store';
import { setUsername as saveUsername } from '../utils/gallery-db';

export default function WelcomeScreen() {
  const [name, setName] = useState('');
  const setStoreName = useAppStore(s => s.setUsername);

  const handleStart = async () => {
    const trimmed = name.trim();
    if (!trimmed) return;
    await saveUsername(trimmed);
    setStoreName(trimmed);
  };

  return (
    <div className="flex flex-col items-center justify-center h-full bg-gradient-to-br from-indigo-500 via-purple-500 to-pink-400 p-6">
      <div className="animate-scale-in bg-white/95 backdrop-blur rounded-3xl shadow-2xl p-8 max-w-sm w-full text-center">
        {/* Logo area */}
        <div className="mb-6">
          <div className="text-6xl mb-3">🎨</div>
          <h1 className="text-3xl font-bold text-gray-800 tracking-tight">
            Risovaska
          </h1>
          <p className="text-gray-500 mt-2 text-sm">
            Draw, create, share with friends
          </p>
        </div>

        {/* Name input */}
        <div className="mb-6">
          <label className="block text-left text-sm font-medium text-gray-600 mb-2">
            What's your name?
          </label>
          <input
            type="text"
            value={name}
            onChange={e => setName(e.target.value)}
            onKeyDown={e => e.key === 'Enter' && handleStart()}
            placeholder="Your name..."
            maxLength={30}
            autoFocus
            className="w-full px-4 py-3 rounded-xl border-2 border-gray-200 focus:border-indigo-400 focus:outline-none text-gray-800 text-lg transition-colors"
          />
        </div>

        {/* Start button */}
        <button
          onClick={handleStart}
          disabled={!name.trim()}
          className="w-full py-3 px-6 rounded-xl bg-gradient-to-r from-indigo-500 to-purple-500 text-white font-semibold text-lg shadow-lg hover:shadow-xl transition-all disabled:opacity-40 disabled:cursor-not-allowed active:scale-[0.98]"
        >
          Start Drawing
        </button>

        <p className="text-xs text-gray-400 mt-4">
          No account needed — just your creativity ✨
        </p>
      </div>
    </div>
  );
}

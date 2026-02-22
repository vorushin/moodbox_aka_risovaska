import { useState, useEffect } from 'react';

const HINT_KEY = 'risovaska_hints_shown';

const HINTS = [
  { id: 'draw', text: 'Start drawing with your finger or mouse!', delay: 500 },
  { id: 'undo', text: 'Two-finger tap to undo, three-finger tap to redo', delay: 5000 },
  { id: 'prompt', text: 'Tap the sparkle button for drawing ideas', delay: 15000 },
];

export default function OnboardingHint() {
  const [currentHint, setCurrentHint] = useState<string | null>(null);
  const [dismissed, setDismissed] = useState<Set<string>>(new Set());

  useEffect(() => {
    // Load previously shown hints
    try {
      const stored = localStorage.getItem(HINT_KEY);
      if (stored) {
        setDismissed(new Set(JSON.parse(stored)));
      }
    } catch { /* ignore */ }
  }, []);

  useEffect(() => {
    const timers: ReturnType<typeof setTimeout>[] = [];

    for (const hint of HINTS) {
      if (dismissed.has(hint.id)) continue;

      const timer = setTimeout(() => {
        setCurrentHint(hint.text);
        // Auto-dismiss after 4 seconds
        const dismissTimer = setTimeout(() => {
          setCurrentHint(null);
          setDismissed(prev => {
            const next = new Set(prev);
            next.add(hint.id);
            localStorage.setItem(HINT_KEY, JSON.stringify([...next]));
            return next;
          });
        }, 4000);
        timers.push(dismissTimer);
      }, hint.delay);
      timers.push(timer);
    }

    return () => timers.forEach(clearTimeout);
  }, [dismissed]);

  if (!currentHint) return null;

  return (
    <div
      className="absolute bottom-4 left-1/2 -translate-x-1/2 bg-black/80 text-white px-5 py-2.5 rounded-full text-sm animate-fade-in backdrop-blur-sm z-20 whitespace-nowrap"
      onClick={() => setCurrentHint(null)}
    >
      {currentHint}
    </div>
  );
}

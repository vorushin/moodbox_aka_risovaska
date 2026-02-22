import { useState, useCallback } from 'react';
import { IoSparkles, IoRefresh, IoClose } from 'react-icons/io5';

const DRAWING_PROMPTS = [
  // Fun challenges
  'Draw your mood today as a weather pattern',
  'Draw your breakfast, but make it fancy',
  'Draw a self-portrait with your eyes closed',
  'Draw your pet (real or imaginary)',
  'Draw what you see out the window right now',
  'Draw your favorite place in the world',
  'Draw a monster that eats homework',
  'Draw a house where you\'d love to live',
  'Draw what happiness looks like',
  'Draw your spirit animal',
  // Storytelling
  'Draw the last dream you remember',
  'Draw your superhero alter ego',
  'Draw a scene from your favorite movie',
  'Draw what you\'ll look like in 20 years',
  'Draw an alien visiting Earth for the first time',
  'Draw a robot doing your least favorite chore',
  'Draw a magical item you wish existed',
  'Draw the perfect weekend',
  // Creative twists
  'Draw something using only circles',
  'Draw a scene using only 3 colors',
  'Draw with your non-dominant hand',
  'Draw the biggest thing you can think of, really tiny',
  'Draw a mashup of two animals',
  'Draw your name as a landscape',
  'Draw food that doesn\'t exist yet',
  'Draw a vehicle from the future',
  // Seasonal / moody
  'Draw what summer sounds like',
  'Draw your cozy corner',
  'Draw a midnight garden',
  'Draw a rainbow made of unusual things',
  'Draw the ocean floor',
  'Draw what music looks like',
  'Draw a treehouse city',
  'Draw a friendly ghost\'s day off',
  // Quick challenges
  'Draw something in under 30 seconds!',
  '5-line challenge: use only 5 strokes',
  'Draw a face using only food items',
  'Draw a building made of candy',
  'Draw a dance move as a stick figure',
  'Draw your favorite emoji, your way',
];

export default function CreativePrompt() {
  const [visible, setVisible] = useState(false);
  const [prompt, setPrompt] = useState('');

  const getRandomPrompt = useCallback(() => {
    let newPrompt: string;
    do {
      newPrompt = DRAWING_PROMPTS[Math.floor(Math.random() * DRAWING_PROMPTS.length)];
    } while (newPrompt === prompt && DRAWING_PROMPTS.length > 1);
    setPrompt(newPrompt);
    setVisible(true);
  }, [prompt]);

  if (!visible) {
    return (
      <button
        onClick={getRandomPrompt}
        className="absolute top-14 right-3 bg-accent/90 text-white p-2.5 rounded-full shadow-lg active:scale-90 transition-transform z-10"
        aria-label="Get drawing idea"
      >
        <IoSparkles size={20} />
      </button>
    );
  }

  return (
    <div className="absolute top-14 right-3 left-3 sm:left-auto sm:w-80 bg-gradient-to-br from-purple-600 to-pink-500 rounded-2xl shadow-2xl p-4 z-10 animate-scale-in">
      <div className="flex items-start justify-between gap-2 mb-2">
        <div className="text-white/70 text-xs font-medium uppercase tracking-wider">
          Drawing idea
        </div>
        <button
          onClick={() => setVisible(false)}
          className="text-white/50 hover:text-white p-1"
        >
          <IoClose size={16} />
        </button>
      </div>
      <p className="text-white text-lg font-semibold leading-snug mb-3">
        {prompt}
      </p>
      <button
        onClick={getRandomPrompt}
        className="flex items-center gap-2 text-white/80 hover:text-white text-sm transition-colors"
      >
        <IoRefresh size={16} />
        Another idea
      </button>
    </div>
  );
}

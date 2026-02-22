import type { ClipartCategory } from '../engine/types';

const categories: { name: string; icon: string; count: number; prefix?: string; hasB?: boolean; bCount?: number }[] = [
  { name: 'Animals', icon: '🐱', count: 12, hasB: true, bCount: 12 },
  { name: 'City', icon: '🏙️', count: 12, prefix: 'City_' },
  { name: 'Heads', icon: '😊', count: 24 },
  { name: 'Music', icon: '🎵', count: 18 },
  { name: 'Nature', icon: '🌿', count: 13 },
  { name: 'Personages', icon: '🧑‍🎨', count: 12 },
  { name: 'Princess', icon: '👸', count: 18 },
  { name: 'Smile', icon: '😄', count: 36 },
  { name: 'Transport', icon: '🚗', count: 18 },
  { name: 'Travel', icon: '✈️', count: 12 },
];

function buildCityItems(): { id: string; path: string }[] {
  const items: { id: string; path: string }[] = [];
  for (let row = 1; row <= 3; row++) {
    for (let col = 1; col <= 4; col++) {
      const num = `${String(row).padStart(2, '0')}_${String(col).padStart(2, '0')}`;
      items.push({
        id: `City_${num}`,
        path: `/clipart/City/City_${num}.svg`,
      });
    }
  }
  return items;
}

export function getClipartCategories(): ClipartCategory[] {
  return categories.map(cat => {
    const items: { id: string; path: string; category: string }[] = [];

    if (cat.name === 'City') {
      items.push(
        ...buildCityItems().map(item => ({ ...item, category: 'City' }))
      );
    } else {
      for (let i = 1; i <= cat.count; i++) {
        const num = String(i).padStart(2, '0');
        items.push({
          id: `${cat.name}_${num}`,
          path: `/clipart/${cat.name}/${num}.svg`,
          category: cat.name,
        });
      }
      // Some categories have "b_" variants (like Animals)
      if (cat.hasB && cat.bCount) {
        for (let i = 1; i <= cat.bCount; i++) {
          const num = String(i).padStart(2, '0');
          items.push({
            id: `${cat.name}_b_${num}`,
            path: `/clipart/${cat.name}/b_${num}.svg`,
            category: cat.name,
          });
        }
      }
    }

    return {
      name: cat.name,
      icon: cat.icon,
      items,
    };
  });
}

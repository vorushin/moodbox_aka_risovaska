import type { ClipartCategory } from '../engine/types';

/** Explicit file lists per category — matches actual files on disk */
const CLIPART_FILES: Record<string, string[]> = {
  Animals: [
    '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12',
    'b_01', 'b_02', 'b_03', 'b_04', 'b_05', 'b_06', 'b_07', 'b_08', 'b_09', 'b_10', 'b_11', 'b_12',
  ],
  City: [
    'City_01_01', 'City_01_02', 'City_01_03', 'City_01_04',
    'City_02_01', 'City_02_02', 'City_02_03', 'City_02_04',
    'City_03_01', 'City_03_02', 'City_03_03', 'City_03_04',
  ],
  Heads: [
    '01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
    '11', '12', '13', '14', '15', '16', '17', '18', '19', '20',
    '21', '22', '23', '24',
  ],
  Music: [
    '01', '02', '03', '04', '05', '06', '07', '08', '09',
    '11', '12', '13', '15', '16', '17', '18',
  ],
  Nature: [
    '01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
    '11', '12', '13',
  ],
  Personages: [
    '01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
    '11', '12',
  ],
  Princess: [
    '01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
    '11', '12', '13', '14', '15',
  ],
  Smile: [
    '01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
    '11', '12',
  ],
  Transport: [
    '01', '02', '03', '04', '05', '06', '07', '08', '09', '10',
    '11', '12',
  ],
  Travel: [
    '01', '01_1', '02', '03', '04', '05', '06', '07', '08', '09',
    '10', '11', '12', '13', '14',
  ],
};

const CATEGORY_ICONS: Record<string, string> = {
  Animals: '🐱',
  City: '🏙️',
  Heads: '😊',
  Music: '🎵',
  Nature: '🌿',
  Personages: '🧑‍🎨',
  Princess: '👸',
  Smile: '😄',
  Transport: '🚗',
  Travel: '✈️',
};

const CATEGORY_ORDER = [
  'Smile', 'Animals', 'Heads', 'Personages', 'Princess',
  'Nature', 'City', 'Music', 'Transport', 'Travel',
];

export function getClipartCategories(): ClipartCategory[] {
  return CATEGORY_ORDER.map(name => ({
    name,
    icon: CATEGORY_ICONS[name] || '📁',
    items: (CLIPART_FILES[name] || []).map(file => ({
      id: `${name}_${file}`,
      path: `/clipart/${name}/${file}.svg`,
      category: name,
    })),
  }));
}

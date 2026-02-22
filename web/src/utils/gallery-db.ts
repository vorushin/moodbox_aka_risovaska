import { get, set, del, keys } from 'idb-keyval';
import type { Drawing } from '../engine/types';

const DRAWINGS_PREFIX = 'drawing_';
const USERNAME_KEY = 'risovaska_username';

export async function saveDrawing(drawing: Drawing): Promise<void> {
  await set(DRAWINGS_PREFIX + drawing.id, drawing);
}

export async function getDrawing(id: string): Promise<Drawing | undefined> {
  return get(DRAWINGS_PREFIX + id);
}

export async function deleteDrawing(id: string): Promise<void> {
  await del(DRAWINGS_PREFIX + id);
}

export async function getAllDrawings(): Promise<Drawing[]> {
  const allKeys = await keys();
  const drawingKeys = allKeys.filter(k =>
    typeof k === 'string' && k.startsWith(DRAWINGS_PREFIX)
  );
  const drawings: Drawing[] = [];
  for (const key of drawingKeys) {
    const d = await get<Drawing>(key);
    if (d) drawings.push(d);
  }
  return drawings.sort((a, b) => b.createdAt - a.createdAt);
}

export async function getUsername(): Promise<string | null> {
  const name = await get<string>(USERNAME_KEY);
  return name ?? null;
}

export async function setUsername(name: string): Promise<void> {
  await set(USERNAME_KEY, name);
}

export function generateId(): string {
  return Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
}

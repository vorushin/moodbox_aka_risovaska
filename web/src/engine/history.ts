/**
 * Canvas undo/redo history using ImageData snapshots.
 * Limited to MAX_HISTORY entries to conserve memory.
 */

const MAX_HISTORY = 40;

export class CanvasHistory {
  private undoStack: ImageData[] = [];
  private redoStack: ImageData[] = [];

  /** Save current canvas state as a snapshot */
  push(canvas: HTMLCanvasElement) {
    const ctx = canvas.getContext('2d');
    if (!ctx) return;
    const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    this.undoStack.push(imageData);
    // Clear redo when a new action happens
    this.redoStack = [];
    // Limit history size
    if (this.undoStack.length > MAX_HISTORY) {
      this.undoStack.shift();
    }
  }

  /** Undo: restore previous state */
  undo(canvas: HTMLCanvasElement): boolean {
    if (this.undoStack.length === 0) return false;
    const ctx = canvas.getContext('2d');
    if (!ctx) return false;

    // Save current state to redo
    const currentData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    this.redoStack.push(currentData);

    // Restore previous
    const prevData = this.undoStack.pop()!;
    ctx.putImageData(prevData, 0, 0);
    return true;
  }

  /** Redo: restore state that was undone */
  redo(canvas: HTMLCanvasElement): boolean {
    if (this.redoStack.length === 0) return false;
    const ctx = canvas.getContext('2d');
    if (!ctx) return false;

    // Save current state to undo
    const currentData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    this.undoStack.push(currentData);

    // Restore redo state
    const nextData = this.redoStack.pop()!;
    ctx.putImageData(nextData, 0, 0);
    return true;
  }

  get canUndo(): boolean {
    return this.undoStack.length > 0;
  }

  get canRedo(): boolean {
    return this.redoStack.length > 0;
  }

  clear() {
    this.undoStack = [];
    this.redoStack = [];
  }
}

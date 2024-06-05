// TODO this should live in ui-core, and be generated
const BASE_SIZE_IN_PX = 16;

function toPx(rem: number): number {
  return rem * BASE_SIZE_IN_PX;
}

export { toPx };

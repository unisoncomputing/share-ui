function truncate(maxLength: number, str: string): string {
  if (str.length > maxLength) {
    return str.slice(0, maxLength - 3) + "...";
  } else {
    return str;
  }
}

function titleize(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

function intersperse<T>(arr: T[], sep: T): T[] {
  return arr.reduce((acc, el, i) => {
    if (i === 0) {
      return [el];
    } else {
      return [...acc, sep, el];
    }
  }, [] as T[]);
}

function userHandle(handle?: string): string {
  if (handle) {
    return `@${handle.replace("@", "")}`;
  } else {
    return "Unknown";
  }
}

function hash(h: string): string {
  return h.replace("#", "").slice(0, 8);
}

export { truncate, titleize, intersperse, userHandle, hash };
